#' Compute the CRPS score of your forecast
#' 
#' @param forecast forecast data frame or file
#' @param target target path or URL (csv file)
#' @importFrom dplyr `%>%`
#' 
#' @export
#' @examples 
#' forecast_file <- system.file("extdata/aquatics-2021-02-01-EFInull.csv.gz", 
#'                               package = "neon4cast")
#' score(forecast_file, "aquatics")                          
score <- function(forecast,
                  target,
                  theme = c("aquatics", "beetles",
                           "phenology", "terrestrial_30min",
                           "terrestrial_daily","ticks"),
                  target_vars = c("oxygen", 
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
                                  "Amblyomma americanum")){
  
  theme = match.arg(theme)
  
  ## read from file if necessary
  if(is.character(forecast)){
    filename <- forecast
    forecast <- read_forecast(forecast) %>% 
      mutate(filename = filename)
  }
  ## tables must declare theme and be in "long" form:
  target <- readr::read_csv(target) %>% 
    dplyr::mutate(theme = theme) %>%
    pivot_target(target_vars)
  
  forecast <- forecast %>% 
    dplyr::mutate(theme=theme) %>%
    pivot_forecast(target_vars)
  
  
  crps_logs_score(forecast, target)
  
}


#GROUP_VARS = c("theme", "team", "issue_date", "site", "x", "y", "z", "time")
#GENERALIZATION:  Need to put this list from a file
#TARGET_VARS = c("oxygen", 
#                "temperature", 
#                "richness",
#                "abundance", 
#                "nee",
#                "le", 
#                "vswc",
#                "gcc_90",
#                "rcc_90",
#                "ixodes_scapularis",
#                "amblyomma_americanum")
#STAT_VARS = c("ensemble", "statistic")
#VARS <- c(GROUP_VARS, TARGET_VARS, STAT_VARS)



## utils 
isoweek <- function(time) { # Not duckdb-compatible
  ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(time), "-","1"))
}
na_rm <- function(x) as.numeric(stats::na.exclude(x))


## Tidy date formats and drop non-standard columns
## shared by targets + forecasts
standardize_format <- function(df, target_vars) {
  
  column_names <- c("theme", "team", "issue_date", "site", "x", "y", "z", "time",
                    target_vars, "ensemble", "statistic")
  
  #GENERALIZATION:  This is a theme specific hack. How do we generalize?
  ## Put tick dates to ISOweek
  ## (arguably should be applied to beetles if not already done too)
  if ("theme" %in% colnames(df) && all(pull(df,theme) == "ticks")) {
    df <- df %>% 
      mutate(time = isoweek(time))
    if("plotID" %in% names(df)) {
      df <- df %>% 
        select(-siteID) %>%
        rename(siteID = plotID)
    }
  }
  
  if(("siteID" %in% colnames(df))){
    df <- df %>% 
      rename(site = siteID)
  }
  
  if(!("site" %in% colnames(df))){
    df <- df %>% 
      mutate(site = NA)
  }
  
  if(("depth" %in% colnames(df))){
    df <- df %>% 
      rename(z = depth)
  }
  
  if(("height" %in% colnames(df))){
    df <- df %>% 
      rename(z = height)
  }
  
  
  if(!("z" %in% colnames(df))){
    df <- df %>% 
      mutate(z = NA)
  }
  
  if(("latitude" %in% colnames(df))){
    df <- df %>% 
      rename(y = latitude)
  }
  
  if(!("y" %in% colnames(df))){
    df <- df %>% 
      mutate(y = NA)
  }
  
  if(("longitude" %in% colnames(df))){
    df <- df %>% 
      rename(x = longitude)
  }
  
  if(!("x" %in% colnames(df))){
    df <- df %>% 
      mutate(x = NA)
  }
  
  # drop non-standard columns
  df %>% 
    dplyr::select(tidyselect::any_of(column_names)) %>%
    enforce_schema()
}

#Select forecasted times using "forecast" flag in standard
select_forecasts <- function(df, only_forecasts){
  
  if("forecast" %in% colnames(df) & only_forecasts){
    df <- df %>% dplyr::filter(forecast == 1)
  }
  
  df
  
}

#' @importFrom dplyr across any_of

deduplicate_predictions <- function(df){
  
  has_dups <- df %>% 
    select(-any_of("predicted")) %>% 
    vctrs::vec_group_id() %>% 
    vctrs::vec_duplicate_any()
  
  if(has_dups) {
    df <- df %>%
      filter(!is.na(predicted)) %>%
      group_by(across(-any_of("predicted"))) %>% 
      filter(dplyr::row_number() == 1L)
  }
  
  df
}

## Parses neon4cast challenge forecast filename components.
split_filename <- function(df){
  
  ## arguably better to split on "-" and unite date components?
  if("filename" %in% colnames(df)) {
    pattern <- "(\\w+)\\-(\\d{4}\\-\\d{2}\\-\\d{2})\\-(\\w+)\\.(csv)?(\\.gz)?(nc)?"
    df <- df %>% 
      mutate(theme = gsub(pattern, "\\1", basename(filename)),
             issue_date = gsub(pattern, "\\2", basename(filename)),
             team = gsub(pattern, "\\3", basename(filename)))
  }
  df
}

pivot_target <- function(df, target_vars){
  
  df %>% 
    standardize_format(target_vars = target_vars) %>% 
    tidyr::pivot_longer(tidyselect::any_of(target_vars), 
                        names_to = "target", 
                        values_to = "observed") %>% 
    filter(!is.na(observed))
}


pivot_forecast <- function(df, target_vars){
  
  df <- df %>% 
    split_filename() %>%
    standardize_format(target_vars = target_vars) %>% 
    tidyr::pivot_longer(tidyselect::any_of(target_vars), 
                        names_to = "target", 
                        values_to = "predicted")
  
  df <- deduplicate_predictions(df)
  
  if("statistic" %in% colnames(df)){
    df <- df %>% 
      tidyr::pivot_wider(names_from = statistic,
                         values_from = predicted)
  }
  
  df
}

## Teach crps to treat any NA observations as NA scores:
crps_sample <- function(y, dat) {
  tryCatch(scoringRules::crps_sample(y, dat),
           error = function(e) NA_real_, finally = NA_real_)
}

crps_norm <- function(y, mean, sd) {
  tryCatch(scoringRules::crps_norm(y, mean = mean, sd = sd),
           error = function(e) NA_real_, finally = NA_real_)
}

## Teach crps to treat any NA observations as NA scores:
logs_sample <- function(y, dat) {
  tryCatch(scoringRules::logs_sample(y, dat),
           error = function(e) NA_real_, finally = NA_real_)
}

logs_norm <- function(y, mean, sd) {
  tryCatch(scoringRules::logs_norm(y, mean = mean, sd = sd),
           error = function(e) NA_real_, finally = NA_real_)
}

## Requires that forecasts and targets have already been cleaned & pivoted!
crps_logs_score <- function(forecast, target){
  
  ## FIXME ensure either both or none have "theme", "issue_date", "team"
  # left join will keep predictions even where we have no observations
  joined <- dplyr::left_join(forecast, target, by = c("theme", "site", "x", "y", "z", "time", "target"))
  
  if("ensemble" %in% colnames(joined)){
    out <- joined %>% 
      group_by(across(-any_of(c("ensemble", "predicted")))) %>% 
      summarise(mean = mean(predicted, na.rm =TRUE),
                sd = sd(predicted, na.rm =TRUE),
                crps = crps_sample(observed[[1]], na_rm(predicted)),
                logs = logs_sample(observed[[1]], na_rm(predicted)),
                quantile02.5 = stats::quantile(predicted, 0.025, na.rm = TRUE),
                quantile10 = stats::quantile(predicted, 0.10, na.rm = TRUE),
                quantile90 = stats::quantile(predicted, 0.90, na.rm = TRUE),
                quantile97.5 = stats::quantile(predicted, 0.975, na.rm = TRUE), .groups = "drop")
    
  } else {
    out <- joined  %>% 
      dplyr::mutate(crps = crps_norm(observed, mean, sd),
                    logs = logs_norm(observed, mean, sd),
                    quantile02.5 = stats::qnorm( 0.025, mean, sd),
                    quantile10 = stats::qnorm(0.10, mean, sd),
                    quantile90 = stats::qnorm(0.90, mean, sd),
                    quantile97.5 = stats::qnorm(0.975, mean, sd))
    
  }
  
  ## Ensure both ensemble and stat-based have identical column order:
  out %>% select(any_of(c("theme", "team", "issue_date", "site", "x", "y", "z", "time",
                          "target", "mean", "sd", "observed", "crps",
                          "logs", "quantile02.5", "quantile10","quantile90","quantile97.5","interval", 
                          "forecast_start_time")))
}

enforce_schema <- function(df) {
  df %>% 
    mutate(across(any_of(c("time", "forecast_start_time")),
                  .fns = as.POSIXct))
}

include_horizon <- function(df, allow_difftime = getOption("neon4cast.allow_difftime", FALSE)){
  
  interval <- df %>%
    group_by(across(any_of(c("theme", "team", "issue_date", "target", "site", "x", "y", "z")))) %>% 
    summarise(interval = min(time-dplyr::lag(time), na.rm=TRUE),
              forecast_start_time = min(time) - interval,
              .groups = "drop")
  
  ## add columns for start_time and horizon
  df <- df %>% 
    left_join(interval, by = c("theme", "team", "issue_date", "site", "x", "y", "z", "target")) %>% 
    mutate(horizon = time - forecast_start_time)
  
  if(!allow_difftime){
    df <- df %>% mutate(horizon = as.integer(horizon),
                        interval = as.integer(interval))
  }
  df
}



## score_it is batch-oriented: takes a large batch of forecast files,
## outputs scores to disk.  This avoid the need to store all forecasts and
## scores in working RAM at the same time.
score_it <- function(targets_file, 
                     forecast_files, 
                     dir = "scores",
                     target_vars = c("oxygen", 
                                     "temperature", 
                                     "richness",
                                     "abundance", 
                                     "nee",
                                     "le", 
                                     "vswc",
                                     "gcc_90",
                                     "rcc_90",
                                     "ixodes_scapularis",
                                     "amblyomma_americanum"),
                     only_forecasts = TRUE
){
  
  dir.create(dir, FALSE, TRUE)
  theme <- strsplit(basename(targets_file), "[-]")[[1]][[1]]
  
  ## Target is processed only once
  target <- 
    readr::read_csv(targets_file, show_col_types = FALSE,
                    lazy = FALSE, progress = FALSE) %>% 
    mutate(theme = theme) %>%
    pivot_target(target_vars)
  
  for(jj in 1:length(forecast_files)){
    message(paste0(jj, "of", length(forecast_files), " ", forecast_files[jj]))
    forecast_file <- forecast_files[jj]
    d <- forecast_file %>%
      read_forecast() %>%
      mutate(filename = forecast_file) %>%
      select_forecasts(only_forecasts) %>%
      pivot_forecast(target_vars) %>%
      crps_logs_score(target) %>% 
      include_horizon() %>%
      write_scores(dir)
    
  }
  
  # ## read, format, and score and write out each forecast file
  # suppressMessages({
  #  furrr::future_walk(forecast_files,
  #                      function(forecast_file, target, target_vars, only_forecasts){
  #                        forecast_file %>%
  #                          read_forecast() %>%
  #                          mutate(filename = forecast_file) %>%
  #                          select_forecasts(only_forecasts) %>%
  #                          pivot_forecast(target_vars) %>%
  #                          crps_logs_score(target) %>% 
  #                          include_horizon() %>%
  #                          write_scores(dir)
  #                      }, 
  #                      target = target,
  #                      target_vars = target_vars,
  #                      only_forecasts = only_forecasts
  #   )
  # })
}


## construct filename from columns and write to disk
write_scores <- function(scores, dir = "scores"){
  r <- utils::head(scores,1)
  output <- file.path(dir, 
                      paste0(paste("scores", r$theme, r$time, r$team, sep="-"),
                             ".csv"))
  
  readr::write_csv(scores, output)
  invisible(output)
  
}


#' score schema for arrow::open_dataset
#' 
#' Open all score.csv files from local or remote disk using arrow by
#' declaring the appropriate file schema.
#' @importFrom arrow schema
#' @export
score_schema  <- function() {
  
  arrow::schema(
    theme      = arrow::string(),
    team       = arrow::string(),
    issue_date = arrow::date32(),
    site       = arrow::string(),
    x          = arrow::float64(),
    y          = arrow::float64(),
    z          = arrow::float64(),
    time       = arrow::timestamp("s", timezone="UTC"),
    target     = arrow::string(),
    mean       = arrow::float64(),
    sd         = arrow::float64(),
    observed   = arrow::float64(),
    crps       = arrow::float64(),
    logs       = arrow::float64(),
    quantile02.5 = arrow::float64(),
    quantile10 =arrow::float64(),
    quantile90 = arrow::float64(),
    quantile97.5 = arrow::float64(),
    interval   = arrow::int64(),
    forecast_start_time = arrow::timestamp("s", timezone="UTC"),
    horizon    = arrow::float64()
  )
}

score_spec <- function() {
  list(
    "theme" = readr::col_character(),
    "team" = readr::col_character(),
    "issue_date" = readr::col_character(),
    "site" = readr::col_character(),
    "x"      = readr::col_double(),
    "y"     = readr::col_double(),
    "z"   = readr::col_double(),
    "time" = readr::col_datetime(),
    "target"  = readr::col_character(),
    "mean" = readr::col_double(),
    "sd" = readr::col_double(),
    "observed" = readr::col_double(),
    "crps" = readr::col_double(),
    "logs" = readr::col_double(),
    "quantile02.5"= readr::col_double(),
    "quantile10" = readr::col_double(),
    "quantile90" = readr::col_double(),
    "quantile97.5" = readr::col_double(),
    "interval" = readr::col_integer(),
    "forecast_start_time" = readr::col_datetime()
  )
}

#utils::globalVariables(c("observed", "predicted", "value",
#                         "variable", "statistic", "sd", 
#                         "filename"),
#                       "neon4cast")