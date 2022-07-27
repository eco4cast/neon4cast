#' NOAA GEFS tables
#' 
#' Access NOAA Global Ensemble Forecast System (GEFS) forecast predictions
#' at NEON sites. The GEFS is NOAA's longest horizon forecast, extending up
#' to 30 days at present, issued at 0.5 degree spatial resolution. 
#' EFI downsamples these forecasts at the coordinates of all NEON sites and
#' provides efficient access to archives of these forecasts in a simple tabular
#' format for a subset of variables of interest. 
#' 
#' WARNING: This combined dataset contains billions of rows. Filtering
#' to a forecast issued on specific `start_date`s or other subsets before
#' `collect()`ing data into R is essential. Be patient, especially on slow
#' network connections, and handle this data with care. See examples.
#' 
#' At each site, 31 ensemble member forecasts are provided
#' at 3 hr intervals for the first 10 days, and 6 hr intervals for up to 30 days
#' (840 hr) horizon. Forecasts include the following variables:
#' - TMP - temperature (K)
#' - RH - Relative humidity (%)
#' - PRES - Atmospheric pressure (Pa)
#' - UGRD - U-component of wind speed (m/s)
#' - VGRD - V-component of wind speed (m/s)
#' - APCP - Total precipitation in interval (kg/m^2)
#' - DSWRF - Downward shortwave radiation flux in interval
#' - DLWRF - Downward shortwave radiation flux in interval
#' 
#' GEFS forecasts are issued four times a day, as indicated by the `start_date`
#' and `cycle`. Only forecasts at midnight, `cycle = "00"` extend for the full
#' 840 hour horizon. Other cycles 06, 12, 18 are provided only 6hrs ahead,
#' as mostly being of interest for short-term forecasts. (Though users should
#' note that other NOAA products provide more much accurate and higher
#' resolution short term forecasts than GEFS.)
#' 
#' 
#' All variables are given at height 2m above ground, as indicated in height.
#' See https://www.nco.ncep.noaa.gov/pmb/products/gens/ for more details on 
#' GEFS variables and intervals.
#' 
#' @references https://www.nco.ncep.noaa.gov/pmb/products/gens/
#' @param cycle Hour at which forecast was made, as character string 
#' (`"00"`, `"06"`, `"12"` or `"18"`). Only `"00"` (default) has 30 days horizon.
#' @param version GEFS forecast version. Prior versions correspond to forecasts
#' issued before 2020-09-25 which have different ensemble number and horizon,
#' among other changes, and are not made available here. Leave as default.
#' @param endpoint the EFI host address (leave as default)
#' @param verbose logical, displays or hides messages
#' @export
#' @examplesIf interactive()
#' 
#' weather <- noaa_stage1()
#' # 5.7M rows of data:
#' weather |> 
#'   dplyr::filter(start_date == "2022-04-01") |>
#'   dplyr::collect()
#' 
#' 
#' 
noaa_stage1 <- function(cycle = "00",
                        version = "v12",
                        endpoint = "data.ecoforecast.org",
                        verbose = TRUE) {
  noaa_gefs_stage("stage1", 
                  version = version, 
                  endpoint = endpoint,
                  verbose = verbose) |> 
    dplyr::filter(cycle == {cycle})
}

#' NOAA GEFS forecasts with EFI stage 2 processing
#' Stage2 processing involves the following transforms of the data:
#' - Fluxes are standardized to rates
#' - ...
#' 
#' @inheritParams noaa_stage1
#' @export
noaa_stage2 <- function(cycle = "00",
                        version = "v12",
                        endpoint = "data.ecoforecast.org",
                        verbose = TRUE) {
  noaa_gefs_stage("stage2/parquet", 
                  version = version, 
                  endpoint = endpoint,
                  verbose = verbose) |> 
    dplyr::filter(cycle == {cycle})
  
}


#' NOAA GEFS forecasts with EFI stage 3 processing
#' 
#' Stage 3 processing presents a 'nowcast' product by combining the most
#' recent predictions from each available cycle. 
#' @inheritParams noaa_stage1
#' @export
noaa_stage3 <- function(version = "v12",
                        endpoint = "data.ecoforecast.org",
                        verbose = TRUE) {
  noaa_gefs_stage("stage3/parquet", 
                  partitioning = "site_id", 
                  version = version, 
                  endpoint = endpoint,
                  verbose = verbose)
}

noaa_gefs_stage <- function(stage = "stage1",
                            partitioning = c("start_date", "cycle"),
                            cycle = "00",
                            version = "v12",
                            endpoint = "data.ecoforecast.org",
                            verbose = getOption("verbose", TRUE)) {
  if(verbose) 
    message(paste("establishing connection to", stage, "at", endpoint, "..."))
  s3 <- noaa_gefs(version, endpoint)
  ds <- arrow::open_dataset(s3$path(stage), partitioning = partitioning)
  if(verbose)
    message(paste0("connected! Use dplyr functions to filter and summarise.\n",
                  "Then, use collect() to read result into R\n"))
  ds  
}

noaa_gefs <- function(version = "v12",
                      endpoint = "data.ecoforecast.org") {

  vars <- arrow_env_vars()
  gefs <- arrow::s3_bucket(paste0("neon4cast-drivers/noaa/gefs-", version),
                           endpoint_override = endpoint,
                           anonymous = TRUE)
  on.exit(unset_arrow_vars(vars))
  gefs

}

arrow_env_vars <- function(){
  user_region <- Sys.getenv("AWS_DEFAULT_REGION")
  user_meta <- Sys.getenv("AWS_EC2_METADATA_DISABLED")
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")

  list(user_region=user_region, user_meta = user_meta)
}

unset_arrow_vars <- function(vars) {
  Sys.setenv("AWS_DEFAULT_REGION" = vars$user_region)
  if (vars$user_meta != "") {
    Sys.setenv(AWS_EC2_METADATA_DISABLED = vars$user_meta)
  }
}
