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
                   s3_region = "submit",
                   s3_endpoint = "ecoforecast.org"
){
  if(file.exists("~/.aws")){
    warning(paste("Detected existing AWS credentials file in ~/.aws,",
                  "Consider renaming these so that automated upload will work"))
  }
  message("validating that file matches required standard")
  go <- forecast_output_validator(forecast_file)

  if(!go){

    warning(paste0("forecasts was not in a valid format and was not submitted\n",
                   "First, try read reinstalling neon4cast (remotes::install_github('eco4cast\\neon4cast'), restarting R, and trying again\n",
                   "Second, see https://projects.ecoforecast.org/neon4cast-docs/Submission-Instructions.html for more information on the file format"))
    return(NULL)
  }

  googlesheets4::gs4_deauth()
  message("Checking if model_id is registered")
  registered_model_id <- suppressMessages(googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1f177dpaxLzc4UuQ4_SJV9JWIbQPlilVnEztyvZE6aSU/edit?usp=sharing", range = "Sheet1!A:V"))

  registered_project_id <- registered_model_id$`What forecasting challenge are you registering for?`
  registered_model_id <- registered_model_id$model_id

  registered_model_project_id <- paste(registered_project_id, registered_model_id, sep = "-")

  df <- read4cast::read_forecast(forecast_file)
  model_id <- df$model_id[1]
  model_project_id <- paste("neon4cast", registered_model_id, sep = "-")

  if(grepl("(example)", model_id)){
    message(paste0("You are submitting a forecast with 'example' in the model_id. As an example forecast, it will be processed but not used in future analyses.\n",
                   "No registration is required to submit an example forecast.\n",
                   "If you want your forecast to be retained, please select a different model_id that does not contain `example` and register you model id at https://forms.gle/kg2Vkpho9BoMXSy57\n"))
  }

  if(!(model_project_id %in% registered_model_project_id) & !grepl("(example)",model_id)){

    message("Checking if model_id for neon4cast is already used in submissions")

    submitted_model_ids <- readr::read_csv("https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/inventory/model_id/model_id-project_id-inventory.csv", show_col_types = FALSE)
    submitted_project_model_id <- paste(submitted_model_ids$project_id, submitted_model_ids$model_id, sep = "-")


    if(model_project_id %in% submitted_project_model_id){

      stop(paste0("Your model_id (",model_id,") has not been registered yet but is already used in other submissions.  Please use and register another model_id\n",
                  "   Register at https://forms.gle/kg2Vkpho9BoMXSy57\n",
                  "If you want to submit without registering, include the word 'example' in your model_id.  As an example forecast, it will be processed but not used in future analyses."))

    }else{

      stop(paste0("Your model_id (",model_id,") has not been registered\n",
                  "   Register at https://forms.gle/kg2Vkpho9BoMXSy57\n",
                  "If you want to submit without registering, include the word 'example' in your model_id.  As an example forecast, it will be processed but not used in future analyses."))

    }
  }

  if(!grepl("(example)",model_id)){
    if(first_submission & model_project_id %in% registered_model_project_id){
      submitted_model_ids <- readr::read_csv("https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/inventory/model_id/model_id-project_id-inventory.csv", show_col_types = FALSE)
      submitted_project_model_id <- paste(submitted_model_ids$project_id, submitted_model_ids$model_id, sep = "-")

      if(model_project_id %in% submitted_project_model_id){
        stop(paste0("Your model_id (",model_id,") is already used in other submitted forecasts. There are two causes for this error: \n
                    - If you have previously submitted a forecast, set the argument `first_submission = FALSE` to remove this error\n
                    - If you have not previously submitted a forecast, this error message means that the model_id has already been registered and used for submissions.  Please register and use another model_id at [https://forms.gle/kg2Vkpho9BoMXSy57](https://forms.gle/kg2Vkpho9BoMXSy57)"))
      }
    }
  }else{
    message("Since `example` is in your model_id, you are submitting an example forecast that will be processed but not used in future analyses.")
  }

  if(go & ask){
    go <- utils::askYesNo("Forecast file is valid, ready to submit?")
  }

  #GENERALIZATION:  Here are specific AWS INFO
  exists <- aws.s3::put_object(file = forecast_file,
                     object = basename(forecast_file),
                     bucket = "submissions",
                     region= s3_region,
                     base_url = s3_endpoint)

  if(exists){
    message("Thank you for submitting!")
  }else{
    warning("Forecasts was not sucessfully submitted to server. Try again, then contact the Challenge organizers.")
  }
}
