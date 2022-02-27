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
#' @examples 
#' download_noaa("ABBY")
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
#' @param forecast_date Include only forecasts issued on this date
#' @examples 
#' stack_noaa()
#' @export
stack_noaa <- function(dir = tempdir(), forecast_date = NULL) {
  files <- list.files(file.path(dir, "noaa"), pattern = "[.]nc",
                      recursive = TRUE, full.names = TRUE)
  names(files) <- basename(files)
  if(!is.null(forecast_date)){
    files <- files[stringr::str_detect(files, forecast_date)]
  }
  
  out <- purrr::map_dfr(files, function(x){
    tidync::hyper_tibble(tidync::tidync(x))
  }, .id = "file")
  
  ## Add metadata from filename as column...
  out <- tidyr::separate(out, file, "_",
                         into=c("model","interval","siteID",
                                "runStartDate", "runEndDate", "ensemble"))
  
  start_time <- stringr::str_split_fixed(out$runStartDate, pattern = "T", n = 2)
  
  out$time <- lubridate::as_datetime(start_time[, 1]) + lubridate::hours(start_time[, 2]) + lubridate::hours(out$time)
  
  out$ensemble <- stringr::str_split_fixed(out$ensemble, ".nc", 2)[, 1]
  
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
  
  download_s3_objects(dir,
                      bucket = "drivers",
                      prefix = file.path("noaa",model, site, date, cycle),
                      s3_region = s3_region)
}



############ and we're ready to go:


# download_noaa("ABBY")
# df <- stack_noaa()