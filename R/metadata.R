#' Write metadata from template
#' 
#' @param forecast_file full path of forecast file
#' @export
#' @examples 
write_meta_template <- function(forecast_file){
  
  dir <- dirname(forecast_file)
  filename <- tools::file_path_sans_ext(basename(forecast_file),
                                        compression = TRUE)
  template_name <- paste0(filenamename,".yml")

  file.copy(system.file("extdata/metadata_template.yml", package="neon4cast"),
            file.path(dir, template_name))
  template <- file.path(dir, template_name)
  usethis::edit_file(template)
  
}

#' Generate metadata from forecast and template files
#' 
#' @param forecast_file full path to forecast file
#' @param metadata_yaml full path to meta data template fill
#' @param forecast_issue_time time that forecast was generated
#' @param forecast_iteration_id unique ID for forecast
#' @export
#' @examples 
generate_metadata <- function(forecast_file, 
                              metadata_yaml, 
                              forecast_issue_time, 
                              forecast_iteration_id){
  
  dir <- dirname(forecast_file)
  
  forecast_file_name_base <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(forecast_file)))
  metadata <- yaml::read_yaml(metadata_yaml)
  
  forecast <- neon4cast:::read_forecast(file_in = forecast_file)
 
  theme <- unlist(stringr::str_split(stringr::str_split(forecast_file, "-")[[1]][1], "_")[[1]][1])
  team_name <- unlist(stringr::str_split(forecast_file_name_base, "-"))[5]
  
  attributes <- readr::read_csv(system.file(paste0("extdata/",theme, "_metadata_attributes.csv"), package="neon4cast"))
  if("ensemble" %in% names(forecast)){
    attributes <- dplyr::filter(attributes, attributeName != "statistic")
    num_variables <- length(which(stringr::str_detect(attributes$attributeDefinition,"variable")))
    #' use `EML` package to build the attribute list
    attrList <- EML::set_attributes(attributes, 
                                    col_classes = c("Date", "numeric", "character","numeric","numeric", 
                                                    rep("numeric", num_variables)))
  }else if("statistic" %in% names(forecast)){
    attributes <- dplyr::filter(attributes, attributeName != "ensemble")
    #' use `EML` package to build the attribute list
    attrList <- EML::set_attributes(attributes, 
                                    col_classes = c("Date", "character", "character","numeric","numeric", 
                                                    rep("numeric", num_variables)))
  }else{
    message("Column names in file does not have ensemble or statistic column")
  }
  
  entityDescription_text <- switch(theme,
         terrestrial = "Forecast of NEE and LE for four NEON sites",
         aquatics = "Forecasts of water temperature and oxygen",
         beetles = "Forecasts of beetles abundance and richness",
         tick =  "Forecasts of tick abundance",
         phenology =  "Forecasts of GCC"
  )

  #' use `EML` package to build the attribute list

  
  
  #' use `EML` package to build the physical list
  physical <- EML::set_physical(forecast_file)
  
  #' use `EML` package to dataTable
  dataTable <- EML::eml$dataTable(
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
  dataset <- EML::eml$dataset(
    title = "Daily persistence null forecast for nee and lee",
    creator = metadata$team_list,
    contact = metadata$team_list[[1]],
    pubDate = lubridate::as_date(forecast_issue_time),
    intellectualRights = "https://creativecommons.org/licenses/by/4.0/",
    dataTable = dataTable,
    coverage = coverage
  )
  
  metadata$metadata$forecast$forecast_issue_time <- lubridate::as_date(forecast_issue_time)
  metadata$metadata$forecast$forecast_iteration_id <- forecast_iteration_id
  metadata$metadata$forecast$forecast_project_id <- team_name
  
  my_eml <- EML::eml$eml(dataset = dataset,
                    additionalMetadata = EML::eml$additionalMetadata(metadata = metadata$metadata),
                    packageId = forecast_iteration_id , 
                    system = "datetime"  ## system used to generate packageId
  )
  
  #'Check that EML matches EFI Standards
  if(!EFIstandards::forecast_validator(my_eml)){
    warning("Error in EFI metadata", call. = FALSE)
  }
  #'Write metadata
  meta_data_filename <-  paste0(dir, "/", forecast_file_name_base,".xml")
  EML::write_eml(my_eml, meta_data_filename)
  return(meta_data_filename)
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


