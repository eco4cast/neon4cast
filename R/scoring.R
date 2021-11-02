#' Compute the CRPS score of your forecast
#' 
#' @param forecast forecast data frame or file
#' @param theme theme name. Note: terrestrial must specify the interval.
#' @importFrom tidync `%>%`
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
  
  theme <- match.arg(theme)
  
  
  if(is.character(forecast))
    forecast <- read_forecast(forecast)

  target_file <- switch(theme,
                     aquatics = "aquatics-targets.csv.gz",
                     beetles = "beetles-targets.csv.gz",
                     phenology = "phenology-targets.csv.gz",
                     terrestrial_daily = "terrestrial_daily-targets.csv.gz",
                     terrestrial_30min = "terrestrial_30min-targets.csv.gz",
                     ticks = "ticks-targets.csv.gz"
                     )
  

  theme <- strsplit(theme, "-")[[1]][[1]]
  download_url <- paste0("https://data.ecoforecast.org/targets/",
                         theme, "/", target_file)
  target <- readr::read_csv(download_url, show_col_types = FALSE, progress = FALSE)
  
  
  target <- format_target(target)
  forecast <- format_forecast(forecast)
  crps_logs_score(forecast, target)
  
}


isoweek <- function(time) {
  ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(time), "-","1"))
  }

format_target <- function(df){
  
  ## Tidy first
  if (all(pull(df,theme)  == "ticks")) {
    df <- df %>% mutate(time = isoweek(time))
  }
  # drop non-standard columns
  df <- dplyr::select(tidyselect::any_of(VARS))
  
  
  
  df <- df %>% 
    tidyr::pivot_longer(tidyselect::any_of(TARGET_VARS), 
                        names_to = "target", 
                        values_to = "observed")
  
  df <- df %>% filter(!is.na(observed))
  
  df
}

format_forecast <- function(df){
  
  ## tidy
  if (all(pull(df,theme)  == "ticks")) {
    df <- df %>% mutate(time = isoweek(time))
  }
  df <- dplyr::select(tidyselect::any_of(VARS))
  
  
  ## pivot
  df <- df %>% 
    tidyr::pivot_longer(tidyselect::any_of(TARGET_VARS), 
                        names_to = "target", 
                        values_to = "predicted")
  
  if("statistic" %in% colnames(df)){
    df <- df %>% 
      tidyr::pivot_wider(names_from = statistic,
                         values_from = predicted)
  }
}

GROUP_VARS = c("siteID", "time", "theme", "team", "issue_date")
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


## FIXME:

## tidy data (drop extraneous columns, fix dates etc)
## pivot targets
## pivot forecasts
## inner_join
## score
crps_logs_score <- function(forecast, target){

  # left join will keep predictions even where we have no observations
  joined <- forecasts %>% 
    forecast_uncertainty_stats() %>%
    dplyr::left_join(targets)
  
  if("ensemble" %in% colnames(joined)){
    out <- joined %>% 
      dplyr::group_by(tidyselect::any_of(GROUP_VARS)) %>% 
      dplyr::summarise(crps = scoringRules::crps_sample(observed[[1]], na_rm(predicted)),
                       logs = scoringRules::logs_sample(observed[[1]], na_rm(predicted)),
                .groups = "drop")
    
  } else {
    out <- joined  %>% 
      dplyr::mutate(crps = scoringRules::crps_norm(observed, mean, sd),
                    logs = scoringRules::logs_norm(observed, mean, sd))
    
  }
  
  out
}

na_rm <- function(x) as.numeric(na.exclude(x))


# Optionally it might be cleaner but maybe slower to compute
# these forecast-only statistics separately
forecast_uncertainty_stats <- function(forecast){
  
  if("ensemble" %in% colnames(forecast)){
    forecast %>% 
      dplyr::group_by(tidyselect::any_of(GROUP_VARS)) %>% 
      dplyr::summarise(mean = mean(predicted, na.rm =TRUE),
                       sd = sd(predicted, na.rm =TRUE),
                       upper95 = quantile(predicted, 0.975, na.rm = TRUE),
                       lower95 = quantile(predicted, 0.025, na.rm = TRUE),
                       .groups = "drop")
  } else {
    forecast %>% 
      dplyr::mutate(upper95 = mean + 1.96 * sd,
                    lower95 = mean - 1.96 * sd)
    
  }
   
  
  
                      
}







utils::globalVariables(c("observed", "predicted", "value", "variable", "statistic", "sd"), "neon4cast")

score_filenames <- function(forecast_files){
  f_name <- tools::file_path_sans_ext(paste0("scores-",
                                             basename(forecast_files)), compression = TRUE)
  file.path("scores", paste0(f_name, ".csv.gz"))
}


score_it <- function(targets_file, 
                     forecast_files, 
                     target_variables,
                     grouping_variables = c("time", "siteID"),
                     reps_col = c("ensemble"),
                     score_files = score_filenames(forecast_files)
){
  
  ## Read in data and compute scores!
  target <- readr::read_csv(targets_file, show_col_types = FALSE, lazy = FALSE,
                            progress = FALSE, id = filename) %>%
    format_target()
  
  
  score_files <- score_filenames(forecast_files)
  
  forecasts <- furrr::future_map(forecast_files, read_forecast)
  
  scores <- furrr::future_map(forecasts, 
                   crps_logs_score, 
                   target = target,  
                   target_variables = target_variables, 
                   grouping_variables = c(grouping_variables),
                   reps_col = reps_col)
  

  
  ## write out score files
  purrr::walk2(scores, score_files, readr::write_csv)
  invisible(score_files)
}



