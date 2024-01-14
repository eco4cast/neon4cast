#' Validate forecast file
#'
#' @param forecast_file forecast csv or csv.gz file
#' @export

forecast_output_validator <- function(forecast_file){
  
  
  file_in <- forecast_file
  
  valid <- TRUE
  
  message(file_in)
  
  if(any(vapply(c("[.]csv", "[.]csv\\.gz"), grepl, logical(1), file_in))){
    
    # if file is csv zip file
    out <- readr::read_csv(file_in, guess_max = 1e6, show_col_types = FALSE)
    
    if(lexists(out, c("model_id"))){
      usethis::ui_done("file has model_id column")
    }else{
      usethis::ui_warn("file missing model_id column ")
    }
    
    
    if("variable" %in% names(out) & "prediction" %in% names(out)){
      usethis::ui_done("forecasted variables found correct variable + prediction column")
    }else{
      usethis::ui_warn("missing the variable and prediction columns")
      valid <- FALSE
    }
    
    if(lexists(out, "ensemble")){
      usethis::ui_warn("ensemble dimension should be named parameter")
      valid <- FALSE
    }else if(lexists(out, "family")){
      
      if(lexists(out, "parameter")){
        usethis::ui_done("file has correct family and parameter columns")
      }else{
        usethis::ui_warn("file does not have parameter column ")
        valid <- FALSE
      }
      
    }else{
      usethis::ui_warn("file does not have ensemble or family and/or parameter column")
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that file contains siteID column...")
    if(lexists(out, c("site_id"))){
      usethis::ui_done("file has site_id column")
    }else{
      usethis::ui_warn("file missing site_id column")
    }
    
    if(lexists(out, c("datetime"))){
      usethis::ui_done("file has datetime column")
      if(!grepl("-", out$datetime[1])){
        usethis::ui_done("datetime column format is not in the correct YYYY-MM-DD format")
        valid <- FALSE
      }else{
        if(sum(class(out$datetime) %in% c("Date","POSIXct")) > 0){
          usethis::ui_done("file has correct datetime column")
        }else{
          usethis::ui_done("datetime column format is not in the correct YYYY-MM-DD format")
          valid <- FALSE
        }
      }
    }else{
      usethis::ui_warn("file missing datetime column")
      valid <- FALSE
    }
    
    
    if(lexists(out, c("duration"))){
      usethis::ui_done("file has duration column")
    }else{
      usethis::ui_warn("file missing duration column (values for the column: daily = P1D, hourly = PT1H)")
    }
    
    if(lexists(out, c("project_id"))){
      usethis::ui_done("file has project_id column")
    }else{
      usethis::ui_warn("file missing project_id column (use `neon4cast` as the project_id")
    }
    
    if(lexists(out, c("reference_datetime"))){
      usethis::ui_done("file has reference_datetime column")
    }else if(lexists(out, c("start_time"))){
      usethis::ui_warn("file start_time column should be named reference_datetime. We are converting it during processing but please update your submission format")
    }else{
      usethis::ui_warn("file missing reference_datetime column")
      valid <- FALSE
    }
    
  }else{
    usethis::ui_warn("incorrect file extension (csv or csv.gz are accepted)")
    valid <- FALSE
  }
  if(!valid){
    message("Forecast file is not valid. The following link provides information about the format:\nhttps://projects.ecoforecast.org/neon4cast-ci/instructions.html#forecast-file-format")
  }else{
    message("Forecast format is valid")
  }
  return(valid)
}


lexists <- function(list,name){
  any(!is.na(match(name, names(list))))
}
