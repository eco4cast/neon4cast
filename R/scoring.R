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
  target <- readr::read_csv(download_url)
  crps_logs_score(forecast, target)
  
}

# guess_theme 

crps_logs_score <- function(forecast, 
                       target,
                       grouping_variables = c("siteID", "time"),
                       target_variables = c("oxygen", 
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
                       reps_col = c("ensemble")){
  

  ## drop extraneous columns && make grouping vars into chr ids (i.e. not dates)

  if("ensemble" %in% colnames(forecast)){ 
    reps_col <- "ensemble"
    variables <- c(grouping_variables, target_variables, reps_col)
  }else  if("statistic" %in% colnames(forecast)){ 
    reps_col <- "statistic"
    variables <- c(grouping_variables, target_variables, reps_col) 
  }
  
  forecast <- forecast %>% dplyr::select(tidyselect::any_of(
    c(variables, "forest_start_time", "horizon", "team", "theme")))
  target <- target %>% dplyr::select(tidyselect::any_of(variables))
  
  ## Teach crps to treat any NA observations as NA scores:
  scoring_crps_ensemble <- function(y, dat) {
    tryCatch(scoringRules::crps_sample(y, dat),
             error = function(e) NA_real_, finally = NA_real_)
  }
  
  scoring_crps_stat <- function(y, mean, sd) {
    tryCatch(scoringRules::crps_norm(y, mean = mean, sd = sd),
             error = function(e) NA_real_, finally = NA_real_)
  }
  
  ## Teach crps to treat any NA observations as NA scores:
  scoring_logs_ensemble <- function(y, dat) {
    tryCatch(scoringRules::logs_sample(y, dat),
             error = function(e) NA_real_, finally = NA_real_)
  }
  
  scoring_logs_stat <- function(y, mean, sd) {
    tryCatch(scoringRules::logs_norm(y, mean = mean, sd = sd),
             error = function(e) NA_real_, finally = NA_real_)
  }
  
  ## Make tables into long format
  target_long <- target %>% 
    tidyr::pivot_longer(tidyselect::any_of(target_variables), 
                 names_to = "target", 
                 values_to = "observed")
  forecast_long <- forecast %>% 
    tidyr::pivot_longer(tidyselect::any_of(target_variables), 
                 names_to = "target", 
                 values_to = "predicted")
  
  if(reps_col == "ensemble"){
    
    dplyr::inner_join(forecast_long, target_long, by = c(grouping_variables, "target"))  %>% 
      dplyr::group_by(dplyr::across(tidyselect::any_of(c(grouping_variables, "target", "horizon", "team", "forest_start_time", "theme")))) %>% 
      dplyr::summarise(crps = scoring_crps_ensemble(observed[[1]], predicted),
                       logs = scoring_logs_ensemble(observed[[1]], predicted),
                       
                .groups = "drop")
    
  } else {
    
    forecast_long %>%
      tidyr::pivot_wider(names_from = statistic, values_from = predicted) %>%
      dplyr::inner_join(target_long, by = c(grouping_variables, "target"))  %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(
        c(grouping_variables, "target", "horizon", "team", "forest_start_time", "theme")))) %>% 
      dplyr::summarise(crps = scoring_crps_stat(observed[[1]], mean, sd),
                       logs = scoring_logs_stat(observed[[1]], mean, sd),
                .groups = "drop")
    
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
  target <- read_forecast(targets_file)
  score_files <- score_filenames(forecast_files)
  forecasts <- lapply(forecast_files, read_forecast)
  
  scores <- lapply(forecasts, 
                   crps_logs_score, 
                   target = target,  
                   target_variables = target_variables, 
                   grouping_variables = c(grouping_variables),
                   reps_col = reps_col)
  

  
  ## write out score files
  purrr::walk2(scores, score_files, readr::write_csv)
  invisible(score_files)
}



