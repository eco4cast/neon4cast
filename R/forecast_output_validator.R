#' forecast_output_validator
#'
#' @param forecast_file Your forecast csv or nc file
#' @param target_variables  Possible target variables
#' @param theme_names valid EFI theme names
#' @export
#' 
#' @examples 
#' 
#' forecast_file <- system.file("extdata/aquatics-2021-02-01-EFInull.csv.gz", 
#'                               package = "neon4cast")
#' forecast_output_validator(forecast_file)
#' 
forecast_output_validator <- function(forecast_file, 
                                      target_variables = c("oxygen", 
                                                           "temperature", 
                                                           "richness",
                                                           "abundance", 
                                                           "nee",
                                                           "le", 
                                                           "vswc",
                                                           "gcc_90",
                                                           "rcc_90",
                                                           "ixodes_scapularis", 
                                                           "amblyomma_americanum",
                                                           "predicted",
                                                           "observed"),
                                      #GENERALIZATION:  Specific themes
                                      theme_names = c("aquatics", "beetles",
                                                      "phenology", "terrestrial_30min",
                                                      "terrestrial_daily","ticks")){
  file_in <- forecast_file
  
  valid <- TRUE
  
  message(file_in)
  
  #usethis::ui_todo("Checking validity of file name...")
  file_basename <- basename(file_in)
  parsed_basename <- unlist(stringr::str_split(file_basename, "-"))
  file_name_parsable <- TRUE
  
  if(!(parsed_basename[1] %in% theme_names)){
    usethis::ui_warn(paste0("first position of file name (before first -) is not one of the following : ",
                            paste(theme_names, collapse = " ")))
    valid <- FALSE
    file_name_parsable <- FALSE
  }
  
  date_string <- lubridate::as_date(paste(parsed_basename[2:4], collapse = "-"))
  
  if(is.na(date_string)){
    usethis::ui_warn("file name does not contain parsable date")
    file_name_parsable <- FALSE
    valid <- FALSE
  }
  
  if(file_name_parsable){
    usethis::ui_done("file name is correct")
  }
  
  if(any(vapply(c("[.]csv", "[.]csv\\.gz"), grepl, logical(1), file_in))){ 
    
    # if file is csv zip file
    out <- readr::read_csv(file_in, guess_max = 1e6, show_col_types = FALSE)
    
    if("variable" %in% names(out) & "predicted" %in% names(out)){
      usethis::ui_done("forecasted variables found correct variable + preducted column")
    }else{
      usethis::ui_warn("missing the variable and predicted columns")
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that file contains either ensemble or statistic column...")
    
    if(lexists(out, "ensemble")){
      usethis::ui_warn("ensemble dimension should be named parameter")
      valid <- FALSE
    }else if(lexists(out, "family")){
      
      if("normal" %in% unique(out$family)){
        usethis::ui_done("file has normal distribution in family column")
      }else if("ensemble" %in% unique(out$family)){
        usethis::ui_done("file has ensemble distribution in family column")
      }else{
        usethis::ui_warn("only normal or ensemble distributions in family columns are currently supported")
        valid <- FALSE
      }
      
      if(lexists(out, "parameter")){
        if("mu" %in% unique(out$parameter) & "sigma" %in% unique(out$parameter)){
          usethis::ui_done("file has parameter and family column with normal distribution")
        }else if("ensemble" %in% unique(out$family)){
          usethis::ui_done("file has parameter and family column with ensemble generated distribution")
        }else{
          usethis::ui_warn("file does not have parameter column is not a normal or ensemble distribution")
          valid <- FALSE
        }
      }else{
        usethis::ui_warn("file does not have parameter and family column ")
        valid <- FALSE
      }
      
    }else{
      usethis::ui_warn("file does not have ensemble or family + parameter column")
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that file contains siteID column...")
    if(lexists(out, c("site_id"))){
      usethis::ui_done("file has site_id column")
    }else if(lexists(out, c("siteID"))){
      usethis::ui_warn("file siteID column should be named site_id")
    }else{
      usethis::ui_warn("file missing site_id column")
    }
    
    #usethis::ui_todo("Checking that file contains parsable time column...")
    if(lexists(out, c("datetime"))){
      usethis::ui_done("file has time column")
      if(!stringr::str_detect(out$datetime[1], "-")){
        usethis::ui_done("time column format is not in the correct YYYY-MM-DD format")
        valid <- FALSE
      }else{
        if(sum(class(out$datetime) %in% c("Date","POSIXct")) > 0){
          usethis::ui_done("file has correct time column")
        }else{
          usethis::ui_done("time column format is not in the correct YYYY-MM-DD format")
          valid <- FALSE
        }
      }
    }else if(lexists(out, c("time"))){
      usethis::ui_warn("time dimension should be named datetime. We are converting it during processing but please update your submission format")
      valid <- TRUE
    }else{
      usethis::ui_warn("file missing time column")
      valid <- FALSE
    }
    
    if(lexists(out, c("reference_datetime"))){
      usethis::ui_done("file has reference_datetime column")
    }else if(lexists(out, c("start_time"))){
      usethis::ui_warn("file start_time column should be named reference_datetime. We are converting it during processing but please update your submission format")
    }else{
      usethis::ui_warn("file missing reference_datetime column")
    }
    
  } else if(grepl("[.]nc", file_in)){ #if file is nc
    
    nc <- ncdf4::nc_open(file_in)
    
    #usethis::ui_todo("Checking that file contains correct variables...")
    
    if(lexists(nc$var, target_variables) > 0){
      usethis::ui_done("target variables found")
      var_dim <- dim(ncdf4::ncvar_get(nc, varid = names(nc$var[which(names(nc$var) %in% target_variables)][1])))
    }else{
      usethis::ui_warn(paste0("no target variables in found in possible list: ", paste(target_variables, collapse = " ")))
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that time variable exist and is parseable...")
    
    if(lexists(nc$dim, c("time", "datetime"))){
      usethis::ui_done("file has time dimension")
      if("time" %in% names(nc$dim)){
        usethis::ui_warn("time dimension should be named datetime we are converting it during processing but please update your submission format")
        time <- ncdf4::ncvar_get(nc, "time")
        time_dim <- length(time)
        valid <- TRUE
      }else{
        time <- ncdf4::ncvar_get(nc, "datetime")
        tustr<-strsplit(ncdf4::ncatt_get(nc, varid = "datetime", "units")$value, " ")
        t_string <- strsplit(ncdf4::ncatt_get(nc, varid = "datetime", "units")$value, " ")[[1]][1]
        time_dim <- length(time)
        time <-lubridate::as_date(time,origin=unlist(tustr)[3])
        if(t_string %in% c("days","seconds")){
          usethis::ui_done("file has correct time dimension")
        }else{
          usethis::ui_warn("time dimension is in correct format")
          valid <- FALSE
        }
      }
    }else{
      usethis::ui_warn("file missing time dimension")
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that siteID variable exists...")
    #GENERALIZATION: using siteID here - should be site_id
    if(lexists(nc$var, c("siteID", "site_id"))){
      usethis::ui_done("file has siteID variable")
    }else{
      usethis::ui_warn("file missing siteID variable")
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that netcdf contains site dimension...")
    
    if(lexists(nc$dim, c("site")) > 0){
      usethis::ui_done("file has site dimension")
      site_dim <- length(ncdf4::ncvar_get(nc, "site"))
      
    }else{
      usethis::ui_warn("file missing site dimension")
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that netcdf contains ensemble dimension...")
    
    if(lexists(nc$dim, "ensemble")){
      usethis::ui_warn("ensemble dimension should be named parameter")
      ensemble_dim <- length(ncdf4::ncvar_get(nc, "ensemble"))
      valid <- FALSE
    }else if(lexists(nc$dim, "parameter")){
      usethis::ui_done("file has parameter dimension")
      ensemble_dim <- length(ncdf4::ncvar_get(nc, "parameter"))
    }else{
      usethis::ui_warn("file missing parameter dimension")
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that netcdf dimensions are correct order...")
    dim_order <- TRUE
    
    if(var_dim[1] != time_dim){
      usethis::ui_warn("time is not the first dimension")
      valid <- FALSE
      dim_order <- FALSE
    }
    
    if(var_dim[2] != site_dim){
      usethis::ui_warn("site is not the second dimension") 
      valid <- FALSE
      dim_order <- FALSE
    }
    
    if(var_dim[3] != ensemble_dim){
      usethis::ui_warn("ensemble is not the third dimension")
      valid <- FALSE
      dim_order <- FALSE
    }
    
    if(dim_order){
      usethis::ui_done("dimensions are correct order")
    }
    
    ncdf4::nc_close(nc)
    
  }else if(grepl("[.]xml", file_in)){ #if file is eml
    
    #usethis::ui_todo("Checking validity of metdata...")
    
    #out <- EML::read_eml(file_in)
    
    #valid_metadata <- tryCatch(EFIstandards::forecast_validator(out),error = function(e){
    #  message(e)
    #  return(FALSE)
    #}, 
    #finally = NULL)
    
    #if(!valid_metadata){
    #  usethis::ui_warn("metadata is not correct")
    #  valid <- FALSE
    #}else{
    #  usethis::ui_done("metadata is correct")
    #}
    valid <- TRUE
  }else{
    valid <- FALSE
  }
  if(!valid){
    message("Forecast file is not valid. The following link provides information about the format:\nhttps://projects.ecoforecast.org/neon4cast-docs/Submission-Instructions.html")
  }else{
    message("Forecast format is valid")
  }
  return(valid)
}


lexists <- function(list,name){
  any(!is.na(match(name, names(list))))
}

