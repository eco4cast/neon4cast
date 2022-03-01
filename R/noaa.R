stack_noaa <- function(dir = tempdir(), model, forecast_date = NULL) {
  
  if(!stringr::str_detect(model,"averaged")){
    
    files <- list.files(file.path(dir, "noaa"), pattern = "[.]nc",
                        recursive = TRUE, full.names = TRUE)
    names(files) <- basename(files)
    files <- files[stringr::str_detect(files, model)]
    if(!is.null(forecast_date)){
      files <- files[stringr::str_detect(files, as.character(forecast_date))]
      if(length(files) == 0){
        warning("Have you downloaded the date that you are stacking?")
      }
    }
    
    out <- purrr::map_dfr(files, function(x){
      tidync::hyper_tibble(tidync::tidync(x))
    }, .id = "file")
    
    if(model == "NOAAGEFS_1hr_stacked"){
      ## Add metadata from filename as column...
      out <- tidyr::separate(out, file, "_",
                             into=c("model","siteID",
                                    "runStartDate", "ensemble")) 
    }else{
      out <- tidyr::separate(out, file, "_",
                             into=c("model","interval","siteID",
                                    "runStartDate", "runEndDate", "ensemble"))
    }
    
    start_time <- stringr::str_split_fixed(out$runStartDate, pattern = "T", n = 2)
    
    out$time <- lubridate::as_datetime(start_time[, 1]) + lubridate::hours(start_time[, 2]) + lubridate::hours(out$time)
    
    out$ensemble <- stringr::str_split_fixed(out$ensemble, ".nc", 2)[, 1]
  }else{
    files <- list.files(file.path(dir, "noaa"), pattern = "[.]nc", 
                        recursive = TRUE, full.names = TRUE)
    names(files) <- basename(files)
    files <- files[stringr::str_detect(files, model)]
    
    out <- purrr::map_dfr(files, function(x) {
      tidync::hyper_tibble(tidync::tidync(x))
    }, .id = "file")
    out <- tidyr::separate(out, file, "_", into = c("model","siteID"))
    out$siteID <- stringr::str_split_fixed(out$siteID, ".nc", 2)[, 1]
    start_time <- rep(NA, length(unique(out$siteID)))
    start_time <- tibble::tibble(file = files, siteID = basename(file), start_time = NA)
    start_time <- tidyr::separate(start_time, siteID, "_", into = c("model","siteID"))
    start_time$siteID <- stringr::str_split_fixed(start_time$siteID, ".nc", 2)[, 1]
    
    for(i in 1:length(start_time$file)){
      nc <- ncdf4::nc_open(start_time$file[i])
      t_string <- strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")[[1]]
      tustr <- lubridate::as_datetime(strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")[[1]][3])
      tustr <- lubridate::as_datetime(strsplit(ncdf4::ncatt_get(nc, varid = "time", "units")$value, " ")[[1]][3])
      start_time$start_time <- tustr
    }
    
    start_time <- dplyr::select(start_time, c("siteID", "start_time"))
    
    out <- dplyr::left_join(out, start_time, by = "siteID")
    
    out$time <- out$start_time + lubridate::hours(out$time)
  }
  
  return(out)
}