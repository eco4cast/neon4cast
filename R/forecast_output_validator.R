#' Validate forecast file
#'
#' @param forecast_file forecast csv or csv.gz file
#' @export

forecast_output_validator <- function(forecast_file){
  
  config <- yaml::read_yaml("https://raw.githubusercontent.com/eco4cast/neon4cast-ci/main/challenge_configuration.yaml")
  allowed_combinations <- NULL
  for(i in 1:length(config$variable_groups)){
    
    curr_tibble <- data.frame(variable = unlist(config$variable_groups[i][[1]]$variable),
                              duration = unlist(config$variable_groups[i][[1]]$duration),
                              max_horizon = unlist(config$variable_groups[i][[1]]$max_horizon))
    
    allowed_combinations <- rbind(allowed_combinations, curr_tibble)
  }
  
  file_in <- forecast_file
  
  valid <- TRUE
  
  message(file_in)
  
  if(any(vapply(c("[.]csv", "[.]csv\\.gz"), grepl, logical(1), file_in))){
    
    # if file is csv zip file
    out <- readr::read_csv(file_in, guess_max = 1e6, show_col_types = FALSE)
    
    if(lexists(out, c("model_id"))){
      usethis::ui_done("file has model_id column")
      if(length(unique(out$model_id)) == 1){
        usethis::ui_done("only one unique value in the model_id column")
        if(is.na(unique(out$model_id))){
          usethis::ui_warn("model_id is correctly NA")
          valid <- FALSE
        }
      }else{
        usethis::ui_warn("file has more than one unique value in the model_id column.  Only one model_id per submission file is allowed")
        valid <- FALSE
      }
    }else{
      usethis::ui_warn("file missing model_id column ")
    }
    
    
    if("variable" %in% names(out) & "prediction" %in% names(out)){
      usethis::ui_done("forecasted variables found correct variable + prediction column")
    }else{
      usethis::ui_warn("missing the variable and prediction columns")
      valid <- FALSE
    }
    
    unique_variables <- unique(out$variable)
    
    # NEED TO GENERALIZE FOR MULTIPLE DURATIONS
    
    for(i in 1:length(unique_variables)){
      if (unique_variables[i] %in% allowed_combinations$variable) {
        usethis::ui_done(paste0(unique_variables[i], " is a valid variable name"))
      }
      else {
        usethis::ui_warn(paste0(unique_variables[i], " is not a valid variable name"))
      }
    }
    
    ######## PARAMETER FAMILY CHECK #############
    
    if(lexists(out, "ensemble")){
      usethis::ui_warn("ensemble dimension should be named parameter")
      valid <- FALSE
    }else if(lexists(out, "family")){
      
      if(lexists(out, "parameter")){
        usethis::ui_done("file has correct family and parameter columns")
        
        
        if(length(unique(out$family)) == 1){
          usethis::ui_done("only one unique value in the family column")
          if(unique(out$family) %in% c("ensemble", "sample") & length(unique(out$parameter)) > 500){
            usethis::ui_warn("file has too many ensemble members.  Needs to be less than 500")
            valid <- FALSE
          }
        }else{
          usethis::ui_warn("file has more than one unique value in the family column.  Only one family type per model_id is allowed")
          valid <- FALSE
        }
      }else{
        usethis::ui_warn("file does not have parameter column ")
        valid <- FALSE
      }
    }else{
      usethis::ui_warn("file does not have ensemble or family and/or parameter column")
      valid <- FALSE
    }
    
    ######## SITE_ID COLUMN CHECK #############
    
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
      
      duration_correct <- unique(out$duration) %in% c("P1D", "PT30M", "P1W")
      
      if(!all(duration_correct)){
        usethis::ui_warn("Incorrect value for duration column (values for the column: daily = P1D, 30min = PT30M, Weekly = P1W)")
        
      }else{
        usethis::ui_done("file has correct duration column")
      }
    }else{
      usethis::ui_warn("file missing duration column (values for the column: daily = P1D, 30min = PT30M, Weekly = P1W)")
    }
    
    if(lexists(out, c("project_id"))){
      usethis::ui_done("file has project_id column")
    }else{
      usethis::ui_warn("file missing project_id column (use `neon4cast` as the project_id)")
    }
    
    if(lexists(out, c("reference_datetime"))){
      usethis::ui_done("file has reference_datetime column")
    }else if(lexists(out, c("start_time"))){
      usethis::ui_warn("file start_time column should be named reference_datetime. We are converting it during processing but please update your submission format")
    }else{
      usethis::ui_warn("file missing reference_datetime column")
      valid <- FALSE
    }
    
    if(lexists(out, c("reference_datetime")) & lexists(out, c("datetime"))){
      out$horizon <- as.integer(as.POSIXct(out$datetime) - as.POSIXct(out$reference_datetime))/ (60*60*24)
      
      for(i in 1:length(unique_variables)){
        
        max_horizon <- max(out$horizon[which(out$variable == unique_variables[i])])
        # NEED TO GENERALIZE FOR MULTIPLE DURATIONS
        allowed_horizon <- allowed_combinations$max_horizon[which(allowed_combinations$variable == unique_variables[i])][1]
        
        if (max_horizon > allowed_horizon){
          usethis::ui_done(paste0("submitted horizon (as.integer(as.POSIXct(out$datetime) - as.POSIXct(out$reference_datetime))/ (60*60*24)) for variable ", unique_variables[i]," is longer than the allowed ", allowed_horizon, " days"))
          valid <- FALSE
        }
      }
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
