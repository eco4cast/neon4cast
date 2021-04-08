write_meta_template <- function(dir, forecast_file){
  
  template_name <- paste0(tools::file_path_sans_ext(tools::file_path_sans_ext(basename(forecast_file))),".yml")
  
  file.copy(system.file("extdata/metadata_template.yml", package="neon4cast"), file.path(dir, template_name))
  template <- file.path(dir, template_name)
  usethis::edit_file(template)
  
}


## internal functions for metadata

# neon geographic coverage
# 
# @param sites vector of NEON siteID codes
# @noRd
# 
# neon_geographic_coverage(c("BART", "KONZ", "SRER", "OSBS"))
neon_geographic_coverage <- function(sites){
  geo <- jsonlite::read_json(system.file("extdata/geo.json", package="neon4cast"))
  site_ids <- purrr::map_chr(purrr::map(geo, "geographicDescription"), 1)
  site_ids <- purrr::map_chr(strsplit(site_ids, ","), 1)
  names(geo) <- site_ids
  geo[sites]
}


theme_sites <- function(theme){
  switch(theme,
    terrestrial = c("BART", "KONZ", "SRER", "OSBS"),
    aquatic = c("BARC", "POSE"),
    beetles = c("BART", "HARV", "BLAN", "SCBI", "SERC", "DSNY", "JERC", "OSBS",
                "GUAN", "LAJA", "STEI", "TREE", "UNDE", "KONA", "KONZ","UKFS",
                "GRSM", "MLBS", "ORNL", "DELA", "LENO", "TALL", "DCFS", "NOGP",
                "WOOD", "CPER", "RMNP", "STER", "CLBJ", "OAES","YELL", "MOAB",
                "NIWO", "JORN", "SRER", "ONAQ", "ABBY", "WREF", "SJER", "SOAP",
                "TEAK", "BARR", "TOOL", "BONA", "DEJU", "HEAL", "PUUM"),
    tick =  c("BLAN", "ORNL", "SCBI", "SERC", "KONZ", "TALL", "UKFS"),
    phenology = c("HARV", "BART","SCBI","STEI","UKFS","GRSM","DELA","CLBJ")
  )
}

generate_metadata <- function(forecast_file, 
                              metadata_yaml, 
                              forecast_issue_time, 
                              forecast_iteration_id){
  
  forecast_file_name_base <- paste0(tools::file_path_sans_ext(tools::file_path_sans_ext(basename(forecast_file))),".yml")
  metadata <- yaml::read_yaml(metadata_yaml)
  
  if(tools::file_ext(forecast_file) %in% c("csv", "gz")){
    forecast <- readr::read_csv(forecast_file)
  } else {
    #NEED TO ADD NETCDF SUPPORT
    forecast <- NULL
  }
  
  theme <- unlist(stringr::str_split(stringr::str_split(forecast_file, "-")[[1]][1], "_")[[1]][1])
  team_name <- unlist(stringr::str_split(unlist(stringr::str_split(forecast_file, "-"))[5], ".csv"))[1]
  
  if(theme == "terrestrial"){
    attributes <- readr::read_csv(system.file("extdata/terrestrial_metadata_attributes.csv", package="neon4cast"))
    if(!is.null(forecast)){
      if(all.equal(names(forecast), attributes$attributeName) != TRUE){
        message("Column names in file do not match required names for complete metadate")
        message(paste0("File names are: ",names(forecast)))
        message(paste0("Required names are: ",attributes$attributeName))
        stop()
      }
    }
    
    entityDescription_text = "Forecast of NEE and LE for four NEON sites"
    
  }else if(theme == "phenology"){
    attributes <- readr::read_csv(system.file("extdata/terrestrial_metadata_attributes.csv", package="neon4cast"))
    if(!is.null(forecast)){
      if(all.equal(names(forecast), attributes$attributeName) != TRUE){
        message("Column names in file do not match required names for complete metadate")
        message(paste0("File names are: ",names(forecast)))
        message(paste0("Required names are: ",attributes$attributeName))
        stop()
      }
    }
    
    entityDescription_text = "Forecasts of GCC"
  }
  
  #' use `EML` package to build the attribute list
  attrList <- EML::set_attributes(attributes, 
                                  col_classes = c("Date", "numeric", "character","numeric","numeric", 
                                                  "numeric","numeric", "numeric","numeric"))
  
  
  #' use `EML` package to build the physical list
  physical <- EML::set_physical(forecast_file)
  
  #' use `EML` package to dataTable
  dataTable <- eml$dataTable(
    entityName = "forecast",  ## this is a standard name to allow us to distinguish this entity from 
    entityDescription = entityDescription_text,
    physical = physical,
    attributeList = attrList)
  
  sites <- unique(forecast$siteID)
  
  geographicCoverage <- neon_geographic_coverage(sites)
  
  start_time <- min(forecast$time)
  stop_time <- max(forecast$time)
  
  temporalCoverage <- list(rangeOfDates =
                             list(beginDate = list(calendarDate = start_time),
                                  endDate = list(calendarDate = stop_time)))
  #'Create the coverage EML
  coverage <- list(geographicCoverage = geographicCoverage,
                   temporalCoverage = temporalCoverage)
  
  #'Create the dataset EML
  dataset <- eml$dataset(
    title = "Daily persistence null forecast for nee and lee",
    creator = metadata$team_list,
    contact = metadata$team_list[[1]],
    pubDate = as_date(forecast_issue_time),
    intellectualRights = "https://creativecommons.org/licenses/by/4.0/",
    dataTable = dataTable,
    coverage = coverage
  )
  
  metadata$metadata$forecast$forecast_issue_time <- as_date(forecast_issue_time)
  metadata$metadata$forecast$forecast_iteration_id <- forecast_iteration_id
  metadata$metadata$forecast$forecast_project_id <- team_name
  
  my_eml <- eml$eml(dataset = dataset,
                    additionalMetadata = eml$additionalMetadata(metadata = metadata$metadata),
                    packageId = forecast_iteration_id , 
                    system = "datetime"  ## system used to generate packageId
  )
  
  #'Check base EML
  if(!EML::eml_validate(my_eml)){
    message("Error in EML metadata")
  }
  
  #'Check that EML matches EFI Standards
  if(EFIstandards::forecast_validator(my_eml)){
    #'Write metadata
    meta_data_filename <-  paste0(forecast_file_name_base,".xml")
    EML::write_eml(my_eml, meta_data_filename)
  }else{
    message("Error in EFI metadata")
  }
  return(meta_data_filename)
}

