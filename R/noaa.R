#' Download NOAA Weather forecasts for NEON sites from the EFI server
#'  
#' @param siteID vector of 4-character NEON siteIDs
#' @param interval Time interval for the forecast
#' @param date start date for the forecast
#' @param cycle NOAA hour-cycle (first hour of the forecast),
#'  options are "00", "06", "12", "18"; only the "00" forecasts
#'  run 35 days into future.
#' @param dir storage location.  Use tempdir unless you want to keep this 
#' data around on your computer, in which case, `neonstore::neon_dir()` might
#' be a convenient choice.
#' @export
# @examples 
# download_noaa("ABBY")
download_noaa <- function(siteID, 
                          interval = "6hr",
                          date = Sys.Date()-2, 
                          cycle = "00", 
                          dir = tempdir()){
  lapply(siteID, download_noaa_, interval, date, cycle, dir)
  invisible(dir)
}
download_noaa_ <- function(siteID, 
                           interval = "6hr",
                           date = Sys.Date()-2, 
                           cycle = "00", 
                           dir = tempdir()){
  
  noaadir <- file.path(dir, "noaa")
  dir.create(noaadir, FALSE, TRUE)
  prefix <- paste("noaa", paste0("NOAAGEFS_", interval), 
                  siteID, date, cycle, sep="/")
  
  #GENERALIZATION:  Specific AWS info
  object <- aws.s3::get_bucket("drivers",
                               prefix = prefix,
                               region = "data",
                               base_url = "ecoforecast.org")
  
  #data <- purrr::map_chr(object, ~ .x$Key)
  
  for(i in seq_along(object)){
    #GENERALIZATION:  Specific AWS info
    aws.s3::save_object(object[[i]], 
                        bucket = "drivers", 
                        file = file.path(noaadir, object[[i]]$Key),
                        region = "data",
                        base_url = "ecoforecast.org")
  }
}

#' Stack downloaded NOAA files
#' 
#' @inheritParams download_noaa
#' @param model NOAA model: NOAAGEFS_1hr, NOAAGEFS_1hr_stacked, NOAAGEFS_1hr_stacked_average
#' @param forecast_date Include only forecasts issued on this date
# @examples 
# stack_noaa()
#' @export
stack_noaa <- function(dir = tempdir(), model = "NOAAGEFS_1hr", forecast_date = NULL) {
  
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

#' Download stacked NOAA data from s3 bucket
#'
#' @param dir full path to working directory
#' @param config flare configuration object
#' @param averaged logistical; TRUE = download averaged stacked forecast
#'
#' @return
#' @export
#'
get_stacked_noaa_s3 <- function(dir, site, averaged = TRUE, s3_region = Sys.getenv("AWS_DEFAULT_REGION")){
  
  if(averaged){
    download_s3_objects(dir, bucket = "drivers", prefix = file.path("noaa/NOAAGEFS_1hr_stacked_average",site), s3_region)
  }else{
    download_s3_objects(dir, bucket = "drivers", prefix = file.path("noaa/NOAAGEFS_1hr_stacked",site), s3_region)
  }
}

#' Download driver forecasts from s3 bucket
#'
#' @param dir full path to working directory
#' @param forecast_path relative path of the driver forecast (relative to driver directory or bucket)
#'
#' @return
#' @export
#'
get_noaa_forecast_s3 <- function(dir, model, site, date, cycle, s3_region = Sys.getenv("AWS_DEFAULT_REGION")){
  
  files_present <- download_s3_objects(dir,
                      bucket = "drivers",
                      prefix = file.path("noaa",model, site, date, cycle),
                      s3_region = s3_region)
  
  invisible(files_present)
}



