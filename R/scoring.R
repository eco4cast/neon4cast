#' Compute the CRPS score of your forecast
#' 
#' @param forecast forecast data frame or file
#' @param theme theme name. Note: terrestrial must specify the interval.
#' @importFrom dplyr `%>%`
#' 
#' @export
#' @examples 
#' forecast_file <- system.file("extdata/aquatics-2021-02-01-EFInull.csv.gz", 
#'                               package = "neon4cast")
#' score(forecast_file, "aquatics")                          
score <- function(forecast,
                  theme = c("aquatics", "beetles",
                            "phenology", "terrestrial_30min",
                            "terrestrial_daily","ticks")){

  theme = match.arg(theme)
  
  ## read from file if necessary
  if(is.character(forecast)){
    filename <- forecast
    forecast <- read_forecast(forecast) %>% mutate(filename = filename)
  }
  ## tables must declare theme and be in "long" form:
  target <- download_target(theme) %>% 
    mutate(theme = theme) %>%
    pivot_target()
  forecast <- forecast %>% 
    mutate(theme=theme) %>%
    pivot_forecast()
  
  
  crps_logs_score(forecast, target)
  
}
  



GROUP_VARS = c("theme", "team", "issue_date", "siteID", "time")
TARGET_VARS = c("oxygen", 
                "temperature", 
                "richness",
                "abundance", 
                "nee",
                "le", 
                "vswc",
                "gcc_90",
                "rcc_90",
                "ixodes_scapularis",
                "amblyomma_americanum")
STAT_VARS = c("ensemble", "statistic")
VARS <- c(GROUP_VARS, TARGET_VARS, STAT_VARS)



## utils 
isoweek <- function(time) { # Not duckdb-compatible
  ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(time), "-","1"))
}
na_rm <- function(x) as.numeric(stats::na.exclude(x))


## Tidy date formats and drop non-standard columns
## shared by targets + forecasts
standardize_format <- function(df) {
  ## Put tick dates to ISOweek
  ## (arguably should be applied to beetles if not already done too)
  if ("theme" %in% colnames(df) && all(pull(df,theme) == "ticks")) {
    df <- df %>% 
      mutate(time = isoweek(time)) %>%
      rename(siteID = plotID)
    
  }
  
  # drop non-standard columns
  df %>% dplyr::select(tidyselect::any_of(VARS))
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
      filter(row_number() == 1L)
  }
  
  df
}

## Parses neon4cast challenge forecast filename components.
split_filename <- function(df){
  
  ## arguably better to split on "-" and unite date components?
  if("filename" %in% colnames(df)) {
    pattern<- "(\\w+)\\-(\\d{4}\\-\\d{2}\\-\\d{2})\\-(\\w+)\\.csv(\\.gz)?"
    df <- df %>% 
      mutate(theme = gsub(pattern, "\\1", basename(filename)),
             issue_date = gsub(pattern, "\\2", basename(filename)),
             team = gsub(pattern, "\\3", basename(filename)))
  }
  df
}


pivot_target <- function(df){

  df %>% 
    standardize_format() %>% 
    tidyr::pivot_longer(tidyselect::any_of(TARGET_VARS), 
                        names_to = "target", 
                        values_to = "observed") %>% 
    filter(!is.na(observed))
}


pivot_forecast <- function(df){
  
  df <- df %>% 
    split_filename() %>%
    standardize_format() %>% 
    tidyr::pivot_longer(tidyselect::any_of(TARGET_VARS), 
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
  joined <- dplyr::left_join(forecast, target)
  
  if("ensemble" %in% colnames(joined)){
    out <- joined %>% 
      group_by(across(-any_of(c("ensemble", "predicted")))) %>% 
      summarise(crps = crps_sample(observed[[1]], na_rm(predicted)),
                logs = logs_sample(observed[[1]], na_rm(predicted)),
                
                ## Ensemble stats must be done before collapsing ensemble data
                mean = mean(predicted, na.rm =TRUE),
                sd = sd(predicted, na.rm =TRUE),
                upper95 = stats::quantile(predicted, 0.975, na.rm = TRUE),
                lower95 = stats::quantile(predicted, 0.025, na.rm = TRUE)
               ) %>% ungroup()
    
  } else {
    out <- joined  %>% 
      dplyr::mutate(crps = crps_norm(observed, mean, sd),
                    logs = logs_norm(observed, mean, sd),
                    upper95 = mean + 1.96 * sd,
                    lower95 = mean - 1.96 * sd)
    
  }
  out
}



include_horizon <- function(df){

  interval <- df %>%
    group_by(across(any_of(c("theme", "team", "issue_date", "target", "siteID")))) %>% 
    summarise(interval = min(time-dplyr::lag(time), na.rm=TRUE),
              forecast_start_time = min(time) - interval,
              .groups = "drop")
  
  ## add columns for start_time and horizon
  df %>% 
    left_join(interval) %>% 
    mutate(horizon = time - forecast_start_time)
}



## score_it is batch-oriented: takes a large batch of forecast files,
## outputs scores to disk.  This avoid the need to store all forecasts and
## scores in working RAM at the same time.
score_it <- function(targets_file, 
                     forecast_files, 
                     dir = "scores"
){
  
  theme <- strsplit(basename(targets_file), "[-_]")[[1]][[1]]
  
  ## Target is processed only once
  target <- 
    readr::read_csv(targets_file, show_col_types = FALSE,
                    lazy = FALSE, progress = FALSE) %>% 
    mutate(theme = theme) %>%
    pivot_target()

    ## read, format, and score and write out each forecast file
    furrr::future_walk(forecast_files,
      function(forecast_file, target){
        forecast_file %>%
          read_forecast() %>%
          mutate(filename = forecast_file) %>%
          pivot_forecast() %>%
          crps_logs_score(target) %>% 
          include_horizon() %>%
          write_scores(dir)
      }, 
      target = target
    )
}


## construct filename from columns and write to disk
write_scores <- function(scores, dir = "scores"){
  r <- utils::head(scores,1)
  output <- file.path(dir, 
                      paste0(paste("scores", r$theme, r$time, r$team, sep="-"),
                             ".csv.gz")
  )
  
  readr::write_csv(scores, output)
  
}




utils::globalVariables(c("observed", "predicted", "value",
                         "variable", "statistic", "sd"),
                       "neon4cast")