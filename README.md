
<!-- README.md is generated from README.Rmd. Please edit that file -->

# neon4cast

<!-- badges: start -->

[![R-CMD-check](https://github.com/eco4cast/neon4cast/workflows/R-CMD-check/badge.svg)](https://github.com/eco4cast/neon4cast/actions)
<!-- badges: end -->

`neon4cast` provides a collection of convenient helper utilities for
anyone entering the EFI NEON Forecasting Challenge.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("eco4cast/neon4cast")
```

## Examples

``` r
library(neon4cast)
library(tidyverse)
library(fable)
library(tsibble)
```

Download and read in the current target file for the Aquatics theme. For
convenience, we read this in as a timeseries object, noting that the
time is in the ‘time’ column, and timeseries are replicated over sites.

``` r
target <- read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz")

aquatic <- target %>% 
    pivot_wider(names_from = "variable", values_from = "observation") %>%
    as_tsibble(index = datetime, key = site_id)
```

Create a 35 day forecast for each variable, `oxygen`, and `temperature`.
For illustrative purposes, we’ll use the `fable` package because it is
concise and well documented. We make separate forecasts for each of the
two variables before reformatting them and combining them. Note the use
of `efi_format` helper function from the `neon4cast` package, which
merely replaces the special `<S3:distribution>` column used by fable
with something we can write to text: either columns with a mean/sd (for
normal distributions) or otherwise random draws from the distributions.

So that we can score our forecast right away instead of waiting for next
month’s data, we will filter out the most recent data available first.

``` r
# drop last 35 days and use explicit NAs for gaps in timeseries
blinded_aquatic <- aquatic %>%
  filter(datetime < max(datetime) - 35) %>% 
  fill_gaps()

# A simple random walk forecast, see ?fable::RW
oxygen_fc <- blinded_aquatic %>%
  model(benchmark_rw = RW(oxygen)) %>%
  forecast(h = "35 days") %>%
  efi_format()

## also use random walk for temperature
temperature_fc <- blinded_aquatic  %>% 
  model(benchmark_rw = RW(temperature)) %>%
  forecast(h = "35 days") %>%
  efi_format_ensemble()

# stack into single table
forecast <- bind_rows(oxygen_fc, temperature_fc) 

## Write the forecast to a file following EFI naming conventions:
forecast_file <- glue::glue("{theme}-{date}-{team}.csv.gz",
                            theme = "aquatics", 
                            date=Sys.Date(),
                            team = "benchmark_rw")
write_csv(forecast, forecast_file)
```

### Score forecast locally

Scores for valid forecasts should appear at
<https://shiny.ecoforecast.org> the day after they are submitted.
However, it is often more convenient to generate scores locally. Note
that the “score” simply the `crps_sample` (for ensemble forecasts) or
`crps_norm` (for summary statistic forecasts) score from the
`scoringRules` R package, for each unique prediction
(i.e. day/site/variable tuple).

Note that scores are only possible once the data becomes available in
the corresponding targets file!

``` r

scores <- score(forecast, target)

# The resulting data.frame scores each day for each site, but is also easy to summarize:
scores %>% 
  group_by(variable) %>% 
  summarise(mean_crps = mean(crps, na.rm=TRUE),
            mean_logs =  mean(logs, na.rm=TRUE))
#> # A tibble: 2 × 3
#>   variable    mean_crps mean_logs
#>   <chr>           <dbl>     <dbl>
#> 1 oxygen          0.509      1.21
#> 2 temperature     1.48       2.40
```

### Validate a forecast file

Validating a forecast file runs the same automated checks as the EFI
server, verifying that the data is in the correct format for the
appropriate challenge. Helpful errors or warnings will displayed on any
invalid formats. Note that the validator accepts files in `.csv`
(optionally compressed as `.csv.gz`) or netcdf.

``` r
forecast_output_validator(forecast_file)
#> [1] TRUE
```

### Access EFI snapshots of NOAA forecasts at NEON sites

Many forecasts will want to make use of weather forecasts as potential
drivers. EFI downscales NOAA GEFS 35-day forecast products at each NEON
site and makes this data available. These helper functions provide
convenient access for downloading and stacking the individual forecast
files.

``` r
aq_sites <- unique(aquatic$site_id)

noaa <- noaa_stage1() 

ref_date <- Sys.Date()-1
# Average temperature forecast across ensembles by site_id, next 240 hours
noaa_temp <- noaa |>
  filter(site_id %in% aq_sites,
         reference_datetime == ref_date,
         variable == "TMP", 
         horizon < 240) |>
  group_by(site_id, reference_datetime, datetime, variable) |>
  summarise(prediction = mean(prediction, na.rm=TRUE)) |>
  collect()

noaa_temp
#> # A tibble: 2,720 × 5
#>    site_id reference_datetime  datetime            variable prediction
#>    <chr>   <dttm>              <dttm>              <chr>         <dbl>
#>  1 FLNT    2022-12-27 00:00:00 2022-12-27 00:00:00 TMP           2.49 
#>  2 FLNT    2022-12-27 00:00:00 2022-12-27 03:00:00 TMP           1.67 
#>  3 FLNT    2022-12-27 00:00:00 2022-12-27 06:00:00 TMP           0.919
#>  4 FLNT    2022-12-27 00:00:00 2022-12-27 09:00:00 TMP           1.01 
#>  5 FLNT    2022-12-27 00:00:00 2022-12-27 12:00:00 TMP           1.08 
#>  6 FLNT    2022-12-27 00:00:00 2022-12-27 15:00:00 TMP           6.03 
#>  7 FLNT    2022-12-27 00:00:00 2022-12-27 18:00:00 TMP          10.8  
#>  8 FLNT    2022-12-27 00:00:00 2022-12-27 21:00:00 TMP          11.7  
#>  9 FLNT    2022-12-27 00:00:00 2022-12-28 00:00:00 TMP           7.04 
#> 10 FLNT    2022-12-27 00:00:00 2022-12-28 03:00:00 TMP           5.74 
#> # … with 2,710 more rows
```

Additional NOAA products are available. `stage2` product includes
downscaling to hourly increments with fluxes corrected for solar
geometries. The stage3 product is assembled from all the 0-hour
horizons, which may be more useful for calibrating models than reliance
on direct measurements of meterological data, as the latter may differ
systematically from the long-term forecast at any given site.

### Submit a forecast

When you are ready to submit your forecast to EFI:

``` r
submit(forecast_file)
```

Ideally you should include the optional `metadata =` argument with your
metadata file.

### Other functions

Encountered a bug? Facing another challenge in participating in the
challenge? Developed a cool approach you would like to share with the
community? Open an [issue](https://github.com/eco4cast/neon4cast/issues)
or [pull request](https://github.com/eco4cast/neon4cast/pulls) here!
