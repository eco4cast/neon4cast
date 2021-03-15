


submit(forecast_file, metadata = NULL){
  if(file.exists("~/.aws"))
    warning(paste("Detected existing AWS credentials file in ~/.aws,",
                  "Consider renaming these so that automated upload will work"))
  
  validate(forecast_file)
  aws.s3::put_object(object = forecast_file, 
                     bucket = "submissions", region="data", base_url = "ecoforecast.org")
  
  if(!is.null(metadata)){
    EFIstandards::forecast_validator(metadata)
    aws.s3::put_object(object = metadata, 
                       bucket = "submissions", region="data", base_url = "ecoforecast.org")
  }
}