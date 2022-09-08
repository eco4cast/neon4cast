## Technically this could become arrow-based

#' submit forecast to EFI
#' 
#' @inheritParams forecast_output_validator
#' @param metadata path to metadata file
#' @param ask should we prompt for a go before submission?
#' @param s3_region subdomain (leave as is for EFI challenge)
#' @param s3_endpoint root domain (leave as is for EFI challenge)
#' @export
submit <- function(forecast_file, 
                   metadata = NULL, 
                   ask = interactive(), 
                   s3_region = "data",
                   s3_endpoint = "ecoforecast.org" 
){
  if(file.exists("~/.aws")){
    warning(paste("Detected existing AWS credentials file in ~/.aws,",
                  "Consider renaming these so that automated upload will work"))
  }
  go <- forecast_output_validator(forecast_file)
  if(go & ask){
    go <- utils::askYesNo("Forecast file is valid, ready to submit?")
  }
  if(!go) return(NULL)
  #GENERALIZATION:  Here are specific AWS INFO
  aws.s3::put_object(file = forecast_file, 
                     object = basename(forecast_file),
                     bucket = "neon4cast-submissions",
                     region= s3_region,
                     base_url = s3_endpoint)
  
  if(!is.null(metadata)){
    if(tools::file_ext(metadata) == "xml"){
      EFIstandards::forecast_validator(metadata)
      aws.s3::put_object(file = metadata, 
                         object = basename(metadata),
                         bucket = "neon4cast-submissions",
                         region= s3_region,
                         base_url = s3_endpoint)
    }else{
      warning(paste("Metadata file is not an .xml file",
                    "Did you incorrectly submit the model description yml file instead of an xml file"))
    }
  }
}

#' Check that submission was successfully processed
#' 
#' @param forecast_file Your forecast csv or nc file
#' @param s3_region subdomain (leave as is for EFI challenge)
#' @param s3_endpoint root domain (leave as is for EFI challenge)
#' @export
check_submission <- function(forecast_file,
                             s3_region = "data",
                             s3_endpoint = "ecoforecast.org"){
  
  theme <- stringr::str_split_fixed(basename(forecast_file), "-", n = 2)
  
  #All forecats are converted into a common file format when they are processed.  This generates that name.
  if (grepl("[.]nc$", forecast_file)) {
    base_name <- paste0(tools::file_path_sans_ext(basename(forecast_file)), 
                        ".csv.gz")
  }else if (grepl("[.]csv$", forecast_file)) {
    base_name <- paste0(tools::file_path_sans_ext(basename(forecast_file)), 
                        ".csv.gz")
  }else if (grepl("[.]csv\\.gz$", forecast_file)) {
    base_name <- basename(forecast_file)
  }else {
    message("File is not a .nc, .cvs, or .csv.gz file")
    base_name <- forecast_file
  }
  
  exists <- suppressMessages(aws.s3::object_exists(object = file.path(theme[,1], base_name), 
                                                   bucket = "neon4cast-forecasts",
                                                   region= s3_region,
                                                   base_url = s3_endpoint))
  
  
  if(exists){
    message("Submission was successfully processed")
  }else{
    not_in_standard <- suppressMessages(aws.s3::object_exists(object = file.path("not_in_standard", basename(forecast_file)), 
                                                              bucket = "neon4cast-forecasts",
                                                              region= s3_region,
                                                              base_url = s3_endpoint))
    if(not_in_standard){
      message("Submission is not in required format. Try running neon4cast::forecast_output_validator on your file to see what the issue may be")
    }else{
      in_submissions <- suppressMessages(aws.s3::object_exists(object = basename(forecast_file), 
                                                               bucket = "neon4cast-submissions",
                                                               region= s3_region,
                                                               base_url = s3_endpoint))
      
      if(in_submissions){
        message("Your forecast is still in queue to be processed by the server. Please check again in a few hours")
      }else{
        message("Submissions is not present on server.  Try uploading again.") 
      }
    }
    
  }
  invisible(exists)
}
