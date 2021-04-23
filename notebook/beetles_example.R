#remotes::install_github("eco4cast/neon4cast")

library(neon4cast) # remotes::install_github("eco4cast/neon4cast")
library(tidyverse)
library(fable)

targets <-  read_csv("https://data.ecoforecast.org/targets/beetles/beetles-targets.csv.gz")

## Coerce to a "tsibble" time-series-data-table
## Skip the last 30 days so we have something to score
targets <- as_tsibble(targets, index = time, key = siteID) %>% filter(time < max(time) - 30)

## Compute a simple mean/sd model per site... obviously silly given huge seasonal aspect
fc_richness <- targets  %>% 
  model(null = MEAN(richness)) %>%
  forecast(h = "1 year") %>%
  efi_format()
fc_abundance <- targets  %>%
  model(null = MEAN(abundance)) %>%
  forecast(h = "1 year") %>%
  efi_format()

## Combine richness and abundance forecasts. drop the 'model' column
forecast <- inner_join(fc_richness, fc_abundance) %>% select(!.model)

scores <- neon4cast::score(forecast, "beetles")