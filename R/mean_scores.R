

#' @export
fill_scores <- function(df, null_team = "EFInull") {
  null <- df %>% 
    filter(team == null_team) %>%
    select("theme", "target", "x","y","z", "site", "time",
           "forecast_start_time", "crps", "logs")
  all <- tidyr::expand_grid(null, distinct(df,team))
  na_filled <- left_join(all, df,
                         by = c("theme", "team", "target", "x","y","z",
                                "site", "time", "forecast_start_time"),
                         suffix = c("_null", "_team"))
  null_filled <- na_filled %>% mutate(
    crps = case_when(is.na(crps_team) ~ crps_null,
                     !is.na(crps_team) ~ crps_team),
    logs = case_when(is.na(logs_team) ~ logs_null,
                     !is.na(logs_team) ~ logs_team))%>% 
    select(-crps_null, -logs_null)
  
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

  df %>% 
    group_by(team, target) %>%
    summarise(crps = mean(crps),
              logs = mean(logs),
              sample_crps = mean(crps_team, na.rm=TRUE),
              sample_logs = mean(logs_team, na.rm=TRUE),
              percent_NA = mean(is.na(crps_team)), .groups = "drop") 
  
}


globalVariables(c("crps", "crps_self", "filled_crps", "filled_logs",
                  "forecast_start_time", "logs", "mean_crps", "null_filled_crps",
                  "null_filled_logs", "target", "team", "theme",
                  "crps_null", "logs_null"), "neon4cast")
