

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
  
  forecast_output_validator(forecast_file)
  
  go <- TRUE
  if(ask){
    go <- utils::askYesNo("Forecast file is valid, ready to submit?")
  }
  if(!go) return(NULL)
  
  aws.s3::put_object(object = forecast_file, 
                     bucket = "submissions",
                     region="data",
                     base_url = "ecoforecast.org")
  
  if(!is.null(metadata)){
    EFIstandards::forecast_validator(metadata)
    aws.s3::put_object(object = metadata, 
                       bucket = "submissions",
                       region="data",
                       base_url = "ecoforecast.org")
  }
}