
#GENRALIZATION:  Specific target variables, missing spatial dimensions
read_forecast <- function(file_in, 
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
                                               "amblyomma_americanum"),
                          reps_col = "ensemble",
                          include_horizon = TRUE,
                          no_forecast = FALSE,
                          ...){

  if(any(vapply(c("[.]csv", "[.]csv\\.gz"), grepl, logical(1), file_in))){  
    # if file is csv zip file
    out <- readr::read_csv(file_in, guess_max = 1e6, lazy = FALSE, show_col_types = FALSE) 

    
  } else if(grepl("[.]nc", file_in)){ #if file is nc
    out <- read_forecast_nc(file_in, target_variables, reps_col)
  }else{
    out <- NA
    no_forecast <- TRUE
  }
  
  if(!is.na(no_forecast)){
    teams_tmp <- (stringr::str_split(basename(file_in), c("-"), simplify = TRUE))
    #GENERALIZATION: This is looking for 30min in the theme name just to know to use datatime rather than date
    if(stringr::str_detect(teams_tmp[,1], "30min")){
      unique_dates <- sort(lubridate::as_datetime(unique(out$time)))
      dates <- lubridate::as_datetime(out$time)
    }else{
      unique_dates <- sort(lubridate::as_date(unique(out$time)))
      dates <- lubridate::as_date(out$time)
    }
    
  }
  
  out
}

#GENERALIZATION: Specific target variables
read_forecast_nc <- function(file_in,
                             target_variables = c("oxygen", 
                                                  "temperature", 
                                                  "richness",
                                                  "abundance", 
                                                  "nee",
                                                  "le", 
                                                  "vswc",
                                                  "gcc_90",
                                                  "ixodes_scapularis",
                                                  "amblyomma_americanum"),
                             reps_col = "ensemble")
{    
  nc <- ncdf4::nc_open(file_in)
  #GENERALIZATION:  Hack because ticks didn't make siteID unique in Round 1
  if("ixodes_scapularis" %in% nc$var | "amblyomma_americanum" %in% nc$var){
    siteID <- ncdf4::ncvar_get(nc, "plotID")
  }
  
  time <- as.integer(ncdf4::ncvar_get(nc, "time"))
  
  if("siteID" %in% nc$var){
    siteID <- ncdf4::ncvar_get(nc, "siteID")  
  }
  
  if("site" %in% nc$var){
    site <- ncdf4::ncvar_get(nc, "site")  
  }
  
  
  if("depth" %in% nc$var){
    depth <- ncdf4::ncvar_get(nc, "depth")  
  }
  
  if("latitude" %in% nc$var){
    latitude <- ncdf4::ncvar_get(nc, "latitude") 
  }
  
  if("longitude" %in% nc$var){
    longitude <- ncdf4::ncvar_get(nc, "longitude") 
  }
  
  if("x" %in% nc$var){
    x <- ncdf4::ncvar_get(nc, "x") 
  }
  
  if("y" %in% nc$var){
    y <- ncdf4::ncvar_get(nc, "y") 
  }
  
  if("z" %in% nc$var){
    z <- ncdf4::ncvar_get(nc, "z") 
  }
  
  #tustr<-strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")
  #time <-lubridate::as_date(time,origin=unlist(tustr)[3])
  t_string <- strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")[[1]]
  if(t_string[1] == "days"){
    tustr<-strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")
    time <-lubridate::as_date(time,origin=unlist(tustr)[3])
  }else{
    tustr <- lubridate::as_datetime(strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")[[1]][3])
    time <- as.POSIXct.numeric(time, origin = tustr)
  } 
  
  targets <- names(nc$var)[which(names(nc$var) %in% target_variables)]
  combined_forecast <- NULL
  for(j in 1:length(targets)){
    forecast_targets <- ncdf4::ncvar_get(nc, targets[j])
    for(i in 1:length(siteID)){
      tmp <- forecast_targets[ ,i ,]
      d <- cbind(time, as.data.frame(tmp))
      names(d) <- c("time", seq(1,dim(tmp)[2]))
      d <- d %>%
        tidyr::pivot_longer(-time, names_to = reps_col, values_to = "value") %>%
        dplyr::mutate(siteID = siteID[i],
                      variable = targets[j])
      
      combined_forecast <- dplyr::bind_rows(combined_forecast, d)
    }
  }
  ncdf4::nc_close(nc)
  combined_forecast <- combined_forecast %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>% 
    dplyr::mutate(filename = basename(file_in))
  
  out <- combined_forecast
  out

}


utils::globalVariables("plotID")
