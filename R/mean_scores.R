
db_import <- function(data, dir = tempfile()){
  
  df <- data %>% 
    dplyr::select(theme, siteID, target, time, team, forecast_start_time, crps, logs)
  
  ## Hmm, SQLite doesn't have date format, might be better as character than as double...
  df <- df %>% dplyr::mutate(time = as.character(time), 
                forecast_start_time = as.character(forecast_start_time))
  con <- DBI::dbConnect(RSQLite::SQLite(), dir)
  
  ## Cannot use duckdb until tidyr::fill issue resolved in dbplyr
  #con <- DBI::dbConnect(duckdb::duckdb(), dir)  
  DBI::dbWriteTable(con, "combined", df, overwrite=TRUE)
  dplyr::tbl(con, "combined")
  
}

#' fill_scores
#' 
#' To compare teams which issue forecasts of multiple observations, we need 
#' a rule for dealing with cases where one team makes predictions for 
#' certain observations at a given forecast horizon when another team does not.
#' For instance, teams may use different horizon lengths (forecast different
#' number of days into the future).  Alternately, two teams may both predict
#' the same observation (target, time, siteID) but the one team does so 
#' much farther in advance than the other.  These miss-matches create
#' "implicit" missing observations relative to other scores.  If we simply omit
#' missing values, teams have little incentive to make a full set of predictions 
#' -- dropping missing values while making one good prediction could lead to a much
#' much better score.  
#' 
#' To address this, we use a null score as a reference.  Teams are scored for all observations
#' and forecast horizons for which the null forecast contains a prediction.  
#' If the team has made a forecast of a given observation but only on a longer horizon, 
#' we will re-use that prediction when comparing against the prediction on the shorter horizon as well.
#' If no prior forecast is available, we will fill the missing prediction with that of the null
#' forecast.
#' @param null_team name of the null team
#' @param df data.frame of raw scores.  Must have columns:
#' `theme`, `siteID`, `target`, `time`, `team`, `forecast_start_time`,
#'  `crps` & `logs`.
#' @export
fill_scores <- function(df, null_team = "EFInull"){
  raw_scores <- db_import(df)
  
  ## Turn implicit NAs to explicit ones:
  na_filled <- na_fill(raw_scores)
  ## Fill missing values with older forecasts of same observations made by same team
  self_filled <- self_fill(na_filled)
  
  ## Fill remaining missing values with those from the null_team:
  null_filled <- null_fill(self_filled, null_team = null_team)
  
  null_filled
}  
  
  
na_fill <- function(df, null = "EFInull"){
  
  ## expand a table to all possible observations (target, siteID, time)
  ## for each team, for each forecast_start_time:
  predicted_by_null <- df %>% 
    filter(team == null) %>% 
    select(theme, target, siteID, time, forecast_start_time) %>%
    distinct() %>% collect()
  team <- df %>% select(team) %>% distinct() %>% pull(team)
  
  all <- tidyr::expand_grid(predicted_by_null, team)
  
  ## Use this list to make explicit NA for any observation for which a forecast was not provided
  na_filled <- df %>% right_join(all, copy=TRUE)
  
  na_filled
}

self_fill <- function(na_filled){
  ## Fill in any missing observation with the most recent forecast made prior to the start_time
  self_filled <- na_filled %>%
    dbplyr::window_order(theme, team, target, siteID, time, forecast_start_time) %>%
    group_by(theme, team, target, siteID, time) %>% 
    fill(crps, logs, .direction="up") %>% 
    ungroup() %>%
    rename(crps_self = crps, logs_self = logs)
  
  ## Join onto original table so we have the original column of NAs + filled column
  na_filled %>% left_join(self_filled)

  
}

null_fill <- function(self_filled, null_team = "EFInull"){
  ## We will now fill all remaining NAs using the NULL forecast.
  null_score <- self_filled %>% ungroup() %>%
    filter(team == null_team) %>% 
    rename(crps_null = crps, logs_null = logs) %>%  
    select("theme", "target", "siteID", "forecast_start_time",
           "time", "crps_null", "logs_null")
  
  null_filled <- self_filled %>%
    left_join(null_score) %>% # add null-score as a separate column
    ## Add columns which fill any remaining NAs after self-fill was applied:
    mutate(filled_crps = case_when(is.na(crps_self) ~ crps_null,
                                   !is.na(crps_self) ~ crps_self),
           filled_logs = case_when(is.na(logs_self) ~ logs_null,
                                   !is.na(logs_self) ~ logs_self),
  ## Add columns which fill NAs of original expanded data (no self-fill) with null,         
           null_filled_crps = case_when(is.na(crps) ~ crps_null,
                                        !is.na(crps) ~ crps),
           null_filled_logs = case_when(is.na(logs) ~ logs_null,
                                        !is.na(logs) ~ logs)
           )  %>% select(-crps_null, -logs_null)
  
  ## Restore date-time formatting (needs duckdb)
  null_filled
  }

#' mean_scores
#' 
#' Compute the mean scores from [fill_scores()] table.
#' By default, results are sorted by mean_crps,
#' based on self-fill and then null-fill.  For comparisons 
#' across different horizons, scores on null_filled_crps alone
#' (no self-fill step) may be preferred.  
#' The number of missing values filled in for each forecast is also reported.
#' @param df a data frame from fill_scores()
#' @importFrom dplyr group_by summarise arrange case_when left_join right_join
#' @importFrom dplyr rename ungroup select mutate distinct collect filter tbl
#' @importFrom dplyr pull
#' @importFrom tidyr fill expand_grid
#' @export
mean_scores <- function(df){

scores <- df %>% 
  group_by(team, target) %>% 
  summarise(mean_crps = mean(filled_crps, na.rm =TRUE), 
            mean_log = mean(filled_logs, na.rm =TRUE),
            mean_null_filled_crps = mean(null_filled_crps, na.rm =TRUE),
            mean_null_filled_logs = mean(null_filled_logs, na.rm =TRUE),
            na_raw = sum(is.na(crps)),
            na_self  = sum(is.na(crps_self)),
            na_filled = sum(is.na(filled_crps)),
            .groups = "drop") %>%
  collect() %>% arrange(mean_crps)

scores
}


globalVariables(c("crps", "crps_self", "filled_crps", "filled_logs",
                  "forecast_start_time", "logs", "mean_crps", "null_filled_crps",
                  "null_filled_logs", "target", "team", "theme",
                  "crps_null", "logs_null"), "neon4cast")
