# DEPRECATE, mark for removal

#GENERALIZATION: SPECIFC THEMES, TARGET FILES, AND TARGET FILE URL



download_target <- function(theme = c("aquatics", "beetles",
                                      "phenology", "terrestrial_30min",
                                      "terrestrial_daily","ticks"),
                            download_url = NA){  
  
  if(is.na(download_url)){
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
  }
  
  readr::read_csv(download_url, show_col_types = FALSE,
                  lazy = FALSE, progress = FALSE)
  
}

