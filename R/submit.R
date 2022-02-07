

#' submit forecast to EFI
#' 
#' @inheritParams forecast_output_validator
#' @param metadata path to metadata file
#' @param ask should we prompt for a go before submission?
#' @export
submit <- function(forecast_file, metadata = NULL, ask = interactive()){
  if(file.exists("~/.aws")){
    warning(paste("Detected existing AWS credentials file in ~/.aws,",
                  "Consider renaming these so that automated upload will work"))
  }
  go <- forecast_output_validator(forecast_file)
  if(go & ask){
    go <- utils::askYesNo("Forecast file is valid, ready to submit?")
  }
  if(!go) return(NULL)
  aws.s3::put_object(file = forecast_file, 
                     bucket = "submissions",
                     region="data",
                     base_url = "ecoforecast.org")
  
  if(!is.null(metadata)){
    if(tools::file_ext(metadata) == "xml"){
      EFIstandards::forecast_validator(metadata)
      aws.s3::put_object(file = metadata, 
                         bucket = "submissions",
                         region="data",
                         base_url = "ecoforecast.org")
    }else{
      warning(paste("Metadata file is not an .xml file",
                    "Did you incorrectly submit the model description yml file instead of an xml file"))
    }
  }
}

#' Check that submission was successfully processed
#' 
#' @param forecast_file Your forecast csv or nc file
#' @export

check_submission <- function(forecast_file){
  
  theme <- stringr::str_split_fixed(forecast_file, "-", n = 2)
  
  
  exists <- aws.s3::object_exists(object = file.path(theme[,1], forecast_file), 
                        bucket = "forecasts",
                        region="data",
                        base_url = "ecoforecast.org")
  if(exists){
    message("Submission was successfully processed")
  }else{
    not_in_standard <- aws.s3::object_exists(object = file.path("not_in_standard", forecast_file), 
                                    bucket = "forecasts",
                                    region="data",
                                    base_url = "ecoforecast.org")
    if(not_in_standard){
      message("Submission is not in required format. Try running neon4cast::forecast_output_validator on your file to see what the issue may be")
    }else{
      in_submissions <- aws.s3::object_exists(object = file.path(forecast_file), 
                                               bucket = "submissions",
                                               region="data",
                                               base_url = "ecoforecast.org")
      
      if(in_submissions){
      message("Your forecast is still in queue to be processed by the server. Please check again in a few hours")}else{
        message("Submissions is not present on server.  Try uploading again.") 
      }
    }
    
  }
  invisible(exists)
}