#' Generate metadata from forecast and template files
#' 
#' @param forecast_file full path to forecast file
#' @param team_list list of team members
#' @param model_metadata list of model metadata
#' @param forecast_iteration_id unique ID for forecast
#' @param title title,
#' @param team_name team name,
#' @param intellectualRights intellectual rights,
#' @param abstract abstract
#' @param methods methods
#' @param attributes attributes 
#' @param forecast_issue_time forecast issue time
#' @export
#' 
generate_metadata <- function(
  forecast_file,
  team_list,
  model_metadata,
  forecast_iteration_id = NULL,
  title = NULL,
  team_name = NULL,
  intellectualRights = "https://creativecommons.org/licenses/by/4.0/",
  abstract = NULL,
  methods = NULL,
  attributes = NULL,
  forecast_issue_time = NULL){
  
  forecast_file_name_base <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(forecast_file)))
  
  forecast <- read4cast::read_forecast(file_in = forecast_file)
  
  theme <- unlist(stringr::str_split(stringr::str_split(forecast_file_name_base, "-")[[1]][1], "_")[[1]][1])
  
  if(is.null(team_name)){
    team_name <- unlist(stringr::str_split(forecast_file_name_base, "-"))[5]
  }
  
  if(is.null(attributes)){
    attribute_file <- system.file(paste0("extdata/",theme, "_metadata_attributes.csv"), package="neon4cast")
    if(file.exists(attribute_file)){
      attributes <- readr::read_csv(attribute_file)
    }else{
      warning("Error in file name.  Please check the submission guidelines for file name conventions", call. = FALSE)
    }
    
    if("data_assimilation" %in% names(forecast)){
      attributes <- dplyr::filter(attributes, attributeName != "data_assimilation")
    }
    
    #NEED TO CHECK THAT COLUMNS IN FORECAST FILE MATCH THE COLUMNS IN THE ATTRIBUTE FILE
    
    if("ensemble" %in% names(forecast)){
      attributes <- dplyr::filter(attributes, attributeName != "statistic")
      num_variables <- length(which(stringr::str_detect(attributes$attributeDefinition,"variable")))
      # use EML package to build the attribute list
    }else if("statistic" %in% names(forecast)){
      attributes <- dplyr::filter(attributes, attributeName != "ensemble")
      num_variables <- length(which(stringr::str_detect(attributes$attributeDefinition,"variable")))
      # use EML package to build the attribute list
      
    }else{
      message("Column names in file does not have ensemble or statistic column")
    }
  }
  
  col_classes <- attributes$numberType
  col_classes[which(col_classes == "datetime")] <- "Date"
  col_classes[which(col_classes == "integer")] <- "numeric"
  col_classes[which(col_classes == "real")] <- "numeric"
  
  
  attrList <- EML::set_attributes(attributes, 
                                  col_classes = col_classes)
  
  entityDescription_text <- switch(theme,
                                   terrestrial = "forecast of ecosystem carbon and water exchange with the atmosphere",
                                   aquatics = "forecast of temperature, oxygen, and chlorophyll-a in lakes and streams",
                                   beetles = "forecast of beetle community abundance and richness",
                                   tick =  "forecast of tick abundance",
                                   phenology =  "forecast of canopy greeness and redness indexes"
  )
  
  # use EML package to build the attribute list
  
  # use EML package to build the physical list
  physical <- EML::set_physical(forecast_file)
  
  # use EML package to dataTable
  dataTable <- EML::eml$dataTable(
    entityName = "forecast",  ## this is a standard name to allow us to distinguish this entity from 
    entityDescription = entityDescription_text,
    physical = physical,
    attributeList = attrList)
  
  sites <- unique(forecast$siteID)
  
  geographicCoverage <- neon_geographic_coverage(sites)
  
  start_date<- lubridate::as_date(min(forecast$time))
  stop_date <- lubridate::as_date(max(forecast$time))
  
  temporalCoverage <- list(rangeOfDates =
                             list(beginDate = list(calendarDate = start_date),
                                  endDate = list(calendarDate = stop_date)))
  
  if(is.null(forecast_issue_time)){
    forecast_issue_time <- Sys.Date()
  }
  
  # Create the coverage EML
  coverage <- list(geographicCoverage = geographicCoverage,
                   temporalCoverage = temporalCoverage)
  
  if(is.null(title)){
    title <- paste0(team_name," ",entityDescription_text)
  }
  # Create the dataset EML
  dataset <- EML::eml$dataset(
    title = title,
    creator = team_list,
    contact = team_list[[1]],
    pubDate = lubridate::as_date(forecast_issue_time),
    intellectualRights = intellectualRights,
    abstract = abstract,
    methods = methods,
    dataTable = dataTable,
    coverage = coverage
  )
  
  model_metadata$forecast$forecast_issue_time <- lubridate::as_date(forecast_issue_time)
  if(is.null(forecast_iteration_id)){
    model_metadata$forecast$forecast_iteration_id <- Sys.time()
  }
  model_metadata$forecast$forecast_project_id <- team_name
  
  if("forecasted" %in% names(forecast)){
    time_vector <- unique(forecast$time[which(forecast$forecast == 1)])
  }else{
    time_vector <- unique(forecast$time)
  }
  
  model_metadata$forecast$timestep <- as.numeric(lubridate::as.duration(time_vector[2] - time_vector[1]))
  model_metadata$forecast$forecast_horizon <- lubridate::as.duration(max(time_vector) - 
                                                           min(time_vector)) + 
    model_metadata$forecast$timestep
  
  model_metadata$forecast$metadata_standard_version <- 0.3
  
  model_metadata$forecast$forecast_horizon <- paste0(as.numeric(model_metadata$forecast$forecast_horizon), " seconds")
  

  
  my_eml <- EML::eml$eml(dataset = dataset,
                         additionalMetadata = EML::eml$additionalMetadata(metadata = model_metadata),
                         packageId = model_metadata$forecast$forecast_iteration_id , 
                         system = "datetime"  ## system used to generate packageId
  )
  
  # Check that EML matches EFI Standards
  if(!EFIstandards::forecast_validator(my_eml)){
    warning("Error in EFI metadata", call. = FALSE)
  }
  # Write metadata
  if(forecast_file != basename(forecast_file)){
    meta_data_filename <-  paste0(dirname(forecast_file), "/", forecast_file_name_base,".xml")
  }else{
    meta_data_filename <- paste0(forecast_file_name_base,".xml")
  }
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
  geo <- jsonlite::read_json(system.file("extdata/neon_geo.json", package="neon4cast"))
  site_ids <- purrr::map_chr(purrr::map(geo, "geographicDescription"), 1)
  site_ids <- purrr::map_chr(strsplit(site_ids, ","), 1)
  geo[which(site_ids %in% sites)]
}

utils::globalVariables("attributeName", "neon4cast")

