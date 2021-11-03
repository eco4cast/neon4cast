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
  
  target_file <- switch(theme,
                     aquatics = "aquatics-targets.csv.gz",
                     beetles = "beetles-targets.csv.gz",
                     phenology = "phenology-targets.csv.gz",
                     terrestrial_daily = "terrestrial_daily-targets.csv.gz",
                     terrestrial_30min = "terrestrial_30min-targets.csv.gz",
                     ticks = "ticks-targets.csv.gz"
                     )
  
  
  download_url <- paste0("https://data.ecoforecast.org/targets/",
                         theme, "/", target_file)
  
  score_it(download_url, forecast)
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



## utils 
isoweek <- function(time) { # Not duckdb-compatible
  ISOweek::ISOweek2date(paste0(ISOweek::ISOweek(time), "-","1"))
}
na_rm <- function(x) as.numeric(stats::na.exclude(x))


## Tidy date formats and drop non-standard columns
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
  
  if("statistic" %in% colnames(df)){
    df <- df %>% 
      tidyr::pivot_wider(names_from = statistic,
                         values_from = predicted)
  }
  
  df
  
}

## Assumes forecasts have already been standardized & pivoted!
crps_logs_score <- function(forecast, target){
  
  ## assert either both or none have "theme", "issue_date", "team"
  
  # left join will keep predictions even where we have no observations
  joined <- dplyr::inner_join(forecast, target)
  
  if("ensemble" %in% colnames(joined)){
    out <- joined %>% 
      group_by(across(-any_of(c("ensemble", "predicted")))) %>% 
      summarise(crps = scoringRules::crps_sample(observed[[1]], na_rm(predicted)),
                logs = scoringRules::logs_sample(observed[[1]], na_rm(predicted)),
                
                ## Ensemble stats must be done before collapsing ensemble data
                mean = mean(predicted, na.rm =TRUE),
                sd = sd(predicted, na.rm =TRUE),
                upper95 = stats::quantile(predicted, 0.975, na.rm = TRUE),
                lower95 = stats::quantile(predicted, 0.025, na.rm = TRUE)
               ) %>% ungroup()
    
  } else {
    out <- joined  %>% 
      dplyr::mutate(crps = scoringRules::crps_norm(observed, mean, sd),
                    logs = scoringRules::logs_norm(observed, mean, sd),
                    upper95 = mean + 1.96 * sd,
                    lower95 = mean - 1.96 * sd)
    
  }
  
  out
}










utils::globalVariables(c("observed", "predicted", "value", "variable", "statistic", "sd"), "neon4cast")

score_filenames <- function(forecast_files){
  f_name <- tools::file_path_sans_ext(paste0("scores-",
                                             basename(forecast_files)), compression = TRUE)
  file.path("scores", paste0(f_name, ".csv.gz"))
}


score_it <- function(targets_file, 
                     forecast_files, 
                     score_files = score_filenames(forecast_files)
){
  
  theme <- strsplit(basename(targets_file), "[-_]")[[1]][[1]]
  
  ## Target is processed only once
  target <- 
    readr::read_csv(targets_file, show_col_types = FALSE,
                    lazy = FALSE, progress = FALSE) %>% 
    mutate(theme = theme) %>%
    pivot_target()
  
  scores <- 
    furrr::future_map(forecast_files,
      function(forecast_file, target){
        forecast_file %>%
          read_forecast() %>%
          pivot_forecast() %>%
          crps_logs_score(target)
      }
    )
  
  ## write out score files
  score_files <- score_filenames(forecast_files)
  purrr::walk2(scores, score_files, readr::write_csv)
  invisible(score_files)
}




