library(tidyverse)
library(tools)
library(scales)

multi_team_plot <- function(combined_forecasts, observations, target, theme, date, horizon, siteID = NA, team = NA){
  
  if("plotID" %in% names(observations)){
    observations <- observations %>% mutate(siteID = plotID)
  }
  
  if(!is.na(siteID)){
    siteID_subset <- siteID
  }else{
    siteID_subset <- unique(observations$siteID)
  }
  
  if(!is.na(team)){
    team_subset <- team
  }else{
    team_subset <- unique(combined_forecasts$team)
  }
  
  observations <- observations %>%
    select(time, siteID, target) %>%
    filter(siteID %in% siteID_subset)
  
  names(observations)[names(observations) == target] <- "obs"
  
  target_variable <- target
  curr_theme <- theme
  
  combined_forecasts <- combined_forecasts %>%
    dplyr::filter(target == target_variable,
                  theme == curr_theme,
                  siteID %in% siteID_subset,
                  team %in% team_subset,
                  lubridate::as_date(forecast_start_time) %in% lubridate::as_date(date))
  
  combined_forecasts$max_date <- combined_forecasts$forecast_start_time + lubridate::days(horizon)
  
  combined_forecasts <- combined_forecasts %>%
    dplyr::mutate(max_date = ifelse(time <= max_date, 1, 0)) %>%
    dplyr::filter(max_date == 1) %>%
    dplyr::left_join(observations, by = c("time", "siteID"))
  
  if(theme != "terrestrial_30min"){
    combined_forecasts <- combined_forecasts %>%
      mutate(time = lubridate::as_date(time),
             forecast_start_time = lubridate::as_date(forecast_start_time))
  }
  
  p <- combined_forecasts %>%
    ggplot2::ggplot(aes(x = time, color = team)) +
    ggplot2::geom_line(aes(y = mean)) +
    ggplot2::geom_ribbon(aes(x = time, ymin = lower95, ymax = upper95, fill = team), alpha = 0.2) +
    ggplot2::geom_point(aes(y = obs), color = "black", alpha = 0.4) +
    ggplot2::labs(y = target, x = "time") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = element_text(angle = 90,
                                              hjust = 0.5, vjust = 0.5))
  
  
  if(class(combined_forecasts$time[1])[1] != "Date"){
    #p <- p + ggplot2::scale_x_datetime(date_labels = scales::date_format("%Y-%m-%d"))
  }else{
    p <- p + ggplot2::scale_x_date(labels = scales::date_format("%Y-%m-%d"))
    
  }
  
  if(length(date) > 1  & length(siteID_subset) > 1){
    
    p + facet_grid(rows = vars(forecast_start_time), cols = vars(siteID))
    
  }else if(length(date) > 1  & length(siteID_subset) == 1){
    
    p + facet_wrap(vars(forecast_start_time)) + labs(title = siteID)
    
  }else if(length(date) == 1  & length(siteID_subset) > 1){
    
    p + facet_wrap(vars(siteID))
    
  }else{
    p + labs(title = siteID_subset)
  }
}

combined <- read_csv("https://data.ecoforecast.org/forecasts/combined_forecasts_scores.csv")

theme_name <- "phenology"
target_file <- switch(theme_name,
                      aquatics = "aquatics-targets.csv.gz",
                      beetles = "beetles-targets.csv.gz",
                      phenology = "phenology-targets.csv.gz",
                      terrestrial_daily = "terrestrial_daily-targets.csv.gz",
                      terrestrial_30min = "terrestrial_30min-targets.csv.gz",
                      ticks = "ticks-targets.csv.gz"
)
theme_main <- strsplit(theme_name, "_")[[1]][[1]]
download_url <- paste0("https://data.ecoforecast.org/targets/",
                       theme_main, "/", target_file)
observations <- readr::read_csv(download_url)


avial_dates <- combined %>%
  dplyr::filter(theme == theme_name) %>%
  select(forecast_start_time) %>%
  distinct()

multi_team_plot(combined_forecasts = combined, observations = observations,
                target = "gcc_90", theme = theme_name, date = c("2021-05-01"), horizon = 35)
