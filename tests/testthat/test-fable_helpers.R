
library(dplyr)
library(tidyr)
library(readr)
library(fable)
library(tsibble)
test_that("efi_format works", {
  
  target <-  
    read_csv(paste0("https://data.ecoforecast.org/neon4cast-targets/",
                    "aquatics/aquatics-targets.csv.gz"),
             show_col_types = FALSE)
  
  aquatic <- target  %>%
    filter(datetime < Sys.Date() - 35) %>%
    pivot_wider(names_from = "variable", values_from = "observation") %>%
    as_tsibble(index = datetime, key = site_id)
  oxygen_fc <- aquatic %>%
    model(benchmark_mean = MEAN(oxygen)) %>%
    forecast(h = "35 days")
  
  fc_sample <- oxygen_fc %>% efi_format_ensemble()
    
  who <- names(fc_sample)
  expect_true(all(who %in% c("datetime", "site_id", "parameter", 
                      "model_id",
                      "family", "variable", "prediction")))
  expect_equal(unique(fc_sample$family), "sample")
  
  
  suppressWarnings({
  temp_fc <-
    aquatic %>%
    fill_gaps() %>%
    model(benchmark_rw = RW(temperature)) %>%
    forecast(h = "35 days")
  
  fc_dist <-temp_fc  %>% efi_format()
  })
  
  expect_equal(unique(fc_dist$family), "normal")
  who <- names(fc_dist)
  expect_true(all(who %in% c("datetime", "site_id", "parameter", 
                             "model_id",
                             "family", "variable", "prediction")))
  
  
  forecast <- bind_rows(fc_sample, fc_dist)
  scores <- score(forecast, target)
  
  # The resulting data.frame scores each day for each site, but is also easy to summarize:
  summary_scores <- scores %>% 
    group_by(variable) %>% 
    summarise(mean_crps = mean(crps, na.rm=TRUE),
              mean_logs =  mean(logs, na.rm=TRUE)) %>%
    tidyr::drop_na()
  
  
  expect_gte(nrow(summary_scores),2)
  
  })
