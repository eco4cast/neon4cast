#' forecast_output_validator
#'
#' @param forecast_file Your forecast csv or nc file
#' @param grouping_variables Grouping variables
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
                               grouping_variables = c("siteID", "time"),
                               target_variables = c("oxygen", 
                                                    "temperature", 
                                                    "richness",
                                                    "abundance", 
                                                    "nee",
                                                    "le", 
                                                    "vswc",
                                                    "gcc_90",
                                                    "ixodes_scapularis", 
                                                    "ambloyomma_americanum"),
                               theme_names = c("aquatics", "beetles",
                                               "phenology", "terrestrial_30min",
                                               "terrestrial_daily","ticks")){
  file_in <- forecast_file
  lexists <- function(list,name){
    sum(name %in% names(list))
  }
  
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
     out <- readr::read_csv(file_in, guess_max = 1e6)
     
     #usethis::ui_todo("Checking that file contains correct variables...")

    if(lexists(out, target_variables) > 0){
      usethis::ui_done("target variables found")
    }else{
      usethis::ui_warn(paste0("no target variables in found in possible list: ", paste(target_variables, collapse = " ")))
      valid <- FALSE
    }
     
     #usethis::ui_todo("Checking that file contains either ensemble or statistic column...")
    
    if(lexists(out, "ensemble")){
      usethis::ui_done("file has ensemble members")
    }else if(lexists(out, "statistic")){
      usethis::ui_done("file has summary statistics column")
      if("mean" %in% unique(out$statistic)){
        usethis::ui_done("file has summary statistic: mean")
      }else{
        usethis::ui_warn("files does not have mean in the statistic column")
        valid <- FALSE
      }
      if("sd" %in% unique(out$statistic)){
        usethis::ui_done("file has summary statistic: sd")
      }else{
        usethis::ui_warn("files does not have sd in the statistic column")
        valid <- FALSE
      }
    }else{
      usethis::ui_warn("files does not have ensemble or statistic column")
      valid <- FALSE
    }
    
     #usethis::ui_todo("Checking that file contains siteID column...")
    if(lexists(out, "siteID")){
      usethis::ui_done("file has siteID column")
    }else{
      usethis::ui_warn("file missing siteID column")
    }
    
     #usethis::ui_todo("Checking that file contains parsable time column...")
    if(lexists(out, "time")){
       usethis::ui_done("file has time column")
       out2  <- readr::read_csv(file_in)
       if(!stringr::str_detect(out2$time[1], "-")){
         usethis::ui_done("time column format is not in the correct YYYY-MM-DD format")
         valid <- FALSE
       }else{
          if(sum(class(out$time) %in% c("Date","POSIXct")) > 0){
            usethis::ui_done("file has correct time column")
          }else{
            usethis::ui_done("time column format is not in the correct YYYY-MM-DD format")
            valid <- FALSE
          }
      }
     }else{
       usethis::ui_warn("file missing time column")
       valid <- FALSE
     }
    
    #usethis::ui_todo("Checking that file contains data assimilation column...")
    #if(lexists(out, "data_assimilation")){
    #  usethis::ui_done("file has data_assimilation column")
    #}else{
    #  usethis::ui_warn("file missing data_assimilation column")
    #  valid <- FALSE
    #}
    
    # usethis::ui_todo("Checking that file contains forecast column...")
     
    #if(lexists(out, "forecast")){
    #  usethis::ui_done("file has forecast column")
    #}else{
    #  usethis::ui_warn("file missing forecast column")
    #  valid <- FALSE
    #}
  
    
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
    
    if(lexists(nc$dim, "time")){
      usethis::ui_done("file has time dimension")
      time <- ncdf4::ncvar_get(nc, "time")
      time_dim <- length(time)
      tustr<-strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")
      time <-lubridate::as_date(time,origin=unlist(tustr)[3])
      t_string <- strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")[[1]][1]
      if(t_string %in% c("days","seconds")){
        usethis::ui_done("file has correct time dimension")
      }else{
        usethis::ui_warn("time dimension is in correct format")
        valid <- FALSE
      }
    }else{
      usethis::ui_warn("file missing time dimension")
      valid <- FALSE
    }
    
    #usethis::ui_todo("Checking that siteID variable exists...")
    if(lexists(nc$var, "siteID")){
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
      usethis::ui_done("file has ensemble dimension")
      ensemble_dim <- length(ncdf4::ncvar_get(nc, "ensemble"))
    }else{
      usethis::ui_warn("file missing ensemble dimension")
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
    
    out <- EML::read_eml(file_in)
    
    valid_metadata <- tryCatch(EFIstandards::forecast_validator(out),error = function(e){
      message(e)
      return(FALSE)
      }, 
      finally = NULL)
    
    if(!valid_metadata){
      usethis::ui_warn("metadata is not correct")
      valid <- FALSE
    }else{
      usethis::ui_done("metadata is correct")
    }
  }else{
    valid <- FALSE
  }
  
  return(valid)
  
}
