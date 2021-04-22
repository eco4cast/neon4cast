

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
    if(file_ext(metadata) == "xml"){
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