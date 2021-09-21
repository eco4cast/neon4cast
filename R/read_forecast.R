
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
                          ...){

  no_forecast <- FALSE
  if(any(vapply(c("[.]csv", "[.]csv\\.gz"), grepl, logical(1), file_in))){  
    # if file is csv zip file
    out <- readr::read_csv(file_in, guess_max = 1e6, ...) 
    if(target_variables == "ixodes_scapularis" | target_variables == "amblyomma_americanum"){
      out <- out %>% 
        mutate(siteID = plotID) %>% 
        select(-plotID)
    }
    
    
  } else if(grepl("[.]nc", file_in)){ #if file is nc
    
    nc <- ncdf4::nc_open(file_in)
    if("ixodes_scapularis" %in% target_variables | "amblyomma_americanum" %in% target_variables){
      siteID <- ncdf4::ncvar_get(nc, "siteID")
    }else{
      siteID <- ncdf4::ncvar_get(nc, "plotID")  
    }
    time <- as.integer(ncdf4::ncvar_get(nc, "time"))
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
  }else{
    out <- NA
    no_forecast <- TRUE
  }
  
  if(!is.na(no_forecast)){
    teams_tmp <- (stringr::str_split(basename(file_in), c("-"), simplify = TRUE))
    if(stringr::str_detect(teams_tmp[,1], "30min")){
      unique_dates <- sort(lubridate::as_datetime(unique(out$time)))
      dates <- lubridate::as_datetime(out$time)
    }else{
      unique_dates <- sort(lubridate::as_date(unique(out$time)))
      dates <- lubridate::as_date(out$time)
    }
    
    time_step <- unique_dates[2] - unique_dates[1]
    first_date <- unique_dates[1] - time_step
    horizon = as.numeric(dates - first_date) / as.numeric(time_step)
    team = tools::file_path_sans_ext(tools::file_path_sans_ext(dplyr::last(teams_tmp[, ncol(teams_tmp)])))
    
    out <- out %>% 
      dplyr::mutate(forecast_start_time = first_date,
                    horizon = horizon,
                    team = team,
                    theme = teams_tmp[,1])
  }
  out
}
