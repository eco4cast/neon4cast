to_hourly <- function(df,
                      use_solar_geom = TRUE,
                      psuedo = FALSE){

  if(!psuedo){
    reference_datetime <- lubridate::as_datetime(df$reference_datetime)
  }else{
    reference_datetime <- NA
  }

  var_order <- names(df)

  ensemble_maxtime <- df |>
    dplyr::group_by(site_id, family, ensemble) |>
    dplyr::summarise(max_time = max(datetime), .groups = "drop")

  ensembles <- unique(df$ensemble)
  datetime <- seq(min(df$datetime), max(df$datetime), by = "1 hour")
  variables <- unique(df$variable)
  sites <- unique(df$site_id)

  full_time <- expand.grid(sites, ensembles, datetime, variables) |>
    dplyr::rename(site_id = Var1,
                  ensemble = Var2,
                  datetime = Var3,
                  variable = Var4) |>
    dplyr::mutate(datetime = lubridate::as_datetime(datetime)) |>
    dplyr::arrange(site_id, ensemble, variable, datetime) |>
    dplyr::left_join(ensemble_maxtime, by = c("site_id","ensemble")) |>
    dplyr::filter(datetime <= max_time) |>
    dplyr::select(-c("max_time")) |>
    dplyr::distinct()

  states <- df |>
    dplyr::select(site_id, family, horizon, ensemble, datetime, variable, prediction) |>
    dplyr::filter(horizon != "006") |>
    dplyr::select(-horizon) |>
    dplyr::group_by(site_id, family, ensemble, variable) |>
    dplyr::right_join(full_time, by = c("site_id", "ensemble", "datetime", "family", "variable")) |>
    dplyr::filter(variable %in% c("PRES", "RH", "TMP", "UGRD", "VGRD")) |>
    dplyr::arrange(site_id, family, ensemble, datetime) |>
    dplyr::mutate(prediction =  imputeTS::na_interpolation(prediction, option = "linear")) |>
    dplyr::mutate(prediction = ifelse(variable == "TMP", prediction + 273, prediction)) |>
    dplyr::mutate(prediction = ifelse(variable == "RH", prediction/100, prediction))

  fluxes <- df |>
    dplyr::select(site_id, family, horizon, ensemble, datetime, variable, prediction) |>
    dplyr::filter(horizon != "003") |>
    dplyr::select(-horizon) |>
    dplyr::group_by(site_id, family, ensemble, variable) |>
    dplyr::right_join(full_time, by = c("site_id", "ensemble", "datetime", "family", "variable")) |>
    dplyr::filter(variable %in% c("APCP","DSWRF","DLWRF")) |>
    dplyr::arrange(site_id, family, ensemble, datetime) |>
    tidyr::fill(prediction, .direction = "up") |>
    dplyr::mutate(prediction = ifelse(variable == "APCP", prediction / (6 * 60 * 60), prediction),
                  variable = ifelse(variable == "APCP", "PRATE", variable))

  if(use_solar_geom){

    site_list <- readr::read_csv(paste0("https://github.com/eco4cast/",
                                        "neon4cast-noaa-download/",
                                        "raw/master/noaa_download_site_list.csv"),
                                 show_col_types = FALSE) |>
      dplyr::select(-site_name)

    fluxes <- fluxes |>
      dplyr::left_join(site_list, by = "site_id") |>
      dplyr::mutate(hour = lubridate::hour(datetime),
                    date = lubridate::as_date(datetime),
                    doy = lubridate::yday(datetime) + hour/24,
                    longitude = ifelse(longitude < 0, 360 + longitude, longitude),
                    rpot = downscale_solar_geom(doy, longitude, latitude)) |>  # hourly sw flux calculated using solar geometry
      dplyr::group_by(site_id, family, ensemble, date, variable) |>
      dplyr::mutate(avg.rpot = mean(rpot, na.rm = TRUE),
                    avg.SW = mean(prediction, na.rm = TRUE))|> # daily sw mean from solar geometry
      dplyr::ungroup() |>
      dplyr::mutate(prediction = ifelse(variable == "DSWRF" & avg.rpot > 0.0, rpot * (avg.SW/avg.rpot),prediction)) |>
      dplyr::select(any_of(var_order))
  }

  hourly_df <- dplyr::bind_rows(states, fluxes) |>
    dplyr::arrange(site_id, family, ensemble, datetime) |>
    dplyr::mutate(variable = ifelse(variable == "TMP", "air_temperature", variable),
                  variable = ifelse(variable == "PRES", "air_pressure", variable),
                  variable = ifelse(variable == "RH", "relative_humidity", variable),
                  variable = ifelse(variable == "DLWRF", "surface_downwelling_longwave_flux_in_air", variable),
                  variable = ifelse(variable == "DSWRF", "surface_downwelling_shortwave_flux_in_air", variable),
                  variable = ifelse(variable == "PRATE", "precipitation_flux", variable),
                  variable = ifelse(variable == "VGRD", "eastward_wind", variable),
                  variable = ifelse(variable == "UGRD", "northward_wind", variable),
                  variable = ifelse(variable == "APCP", "precipitation_amount", variable),
                  reference_datetime = reference_datetime) |>
    dplyr::select(any_of(var_order))

  return(hourly_df)

}

cos_solar_zenith_angle <- function(doy, lat, lon, dt, hr) {
  et <- equation_of_time(doy)
  merid  <- floor(lon / 15) * 15
  merid[merid < 0] <- merid[merid < 0] + 15
  lc     <- (lon - merid) * -4/60  ## longitude correction
  tz     <- merid / 360 * 24  ## time zone
  midbin <- 0.5 * dt / 86400 * 24  ## shift calc to middle of bin
  t0   <- 12 + lc - et - tz - midbin  ## solar time
  h    <- pi/12 * (hr - t0)  ## solar hour
  dec  <- -23.45 * pi / 180 * cos(2 * pi * (doy + 10) / 365)  ## declination
  cosz <- sin(lat * pi / 180) * sin(dec) + cos(lat * pi / 180) * cos(dec) * cos(h)
  cosz[cosz < 0] <- 0
  return(cosz)
}

equation_of_time <- function(doy) {
  stopifnot(doy <= 367)
  f      <- pi / 180 * (279.5 + 0.9856 * doy)
  et     <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 *
               sin(4 * f) - 429.3 * cos(f) - 2 *
               cos(2 * f) + 19.3 * cos(3 * f)) / 3600  # equation of time -> eccentricity and obliquity
  return(et)
}

downscale_solar_geom <- function(doy, lon, lat) {

  dt <- median(diff(doy)) * 86400 # average number of seconds in time interval
  hr <- (doy - floor(doy)) * 24 # hour of day for each element of doy

  ## calculate potential radiation
  cosz <- cos_solar_zenith_angle(doy, lat, lon, dt, hr)
  rpot <- 1366 * cosz
  return(rpot)
}
