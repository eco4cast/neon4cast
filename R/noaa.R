#' Download NOAA Weather forecasts for NEON sites from the EFI server
#'  
#' @param siteID a 4-character NEON siteID
#' @param interval Time interval for the forecast
#' @param date start date for the forecast
#' @param cycle NOAA hour-cycle (first hour of the forecast),
#'  options are "00", "06", "12", "18"; only the "00" forecasts
#'  run 35 days into future.
#' @param dir storage location.  Use tempdir unless you want to keep this 
#' data around on your computer, in which case, `neonstore::neon_dir()` might
#' be a convenient choice.
#' @export
download_noaa <- function(siteID, 
                          interval = "6hr",
                          date = Sys.Date()-2, 
                          cycle = "00", 
                          dir = tempdir()){
  
  noaadir <- file.path(dir, "noaa")
  dir.create(noaadir, FALSE, TRUE)
  prefix <- paste("noaa", paste0("NOAAGEFS_", interval), 
                  siteID, date, cycle, sep="/")
  object <- aws.s3::get_bucket("drivers",
                               prefix = prefix,
                               region = "data",
                               base_url = "ecoforecast.org")
  
  #data <- purrr::map_chr(object, ~ .x$Key)
  
  for(i in seq_along(object)){
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
#' @export
stack_noaa <- function(dir = tempdir()) {
  files <- list.files(file.path(dir, "noaa"), pattern = "[.]nc",
                      recursive = TRUE, full.names = TRUE)
  
  out <- purrr::map_dfr(files, function(x){
    tidync::hyper_tibble(tidync::tidync(x))
  }, .id = "file")
  
  ## Add metadata from filename as column...
  
  out
}
############ and we're ready to go:


# download_noaa("ABBY")
# df <- stack_noaa()