
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
aquatic <- read_csv("https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz") %>% 
  as_tsibble(index=time, key=siteID)
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
blinded_aquatic <- aquatic %>% filter(time < max(time) - 35) %>% fill_gaps()

# A simple random walk forecast, see ?fable::RW
oxygen_fc <- blinded_aquatic %>%
  model(null = RW(oxygen)) %>%
  forecast(h = "35 days") %>%
  efi_format()

## also use random walk for temperature
temperature_fc <- blinded_aquatic  %>%
  model(null = RW(temperature)) %>%
  forecast(h = "35 days") %>%
  efi_format()

# combine into single table, drop the .model column
forecast <- inner_join(oxygen_fc, temperature_fc) %>% select(-.model)

## Write the forecast to a file following EFI naming conventions:
forecast_file <- glue::glue("{theme}-{date}-{team}.csv.gz",
                            theme = "aquatics", 
                            date=Sys.Date(),
                            team = "example_null")
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
forecast$theme <- "aquatics"
scores <- score(forecast)

# The resulting data.frame scores each day for each site, but is also easy to summarize:
scores %>% 
  group_by(siteID, target) %>% 
  summarise(mean_crps = mean(crps, na.rm=TRUE),
            mean_logs =  mean(logs, na.rm=TRUE))
#> # A tibble: 4 × 4
#>   siteID target      mean_crps mean_logs
#>   <chr>  <chr>           <dbl>     <dbl>
#> 1 BARC   oxygen          0.464      1.46
#> 2 BARC   temperature   NaN        NaN   
#> 3 POSE   oxygen          0.607      1.77
#> 4 POSE   temperature     1.48       2.62
```

### Validate a forecast file

Validating a forecast file runs the same automated checks as the EFI
server, verifying that the data is in the correct format for the
appropriate challenge. Helpful errors or warnings will displayed on any
invalid formats. Note that the validator accepts files in `.csv`
(optionally compressed as `.csv.gz`) or netcdf.

``` r
forecast_output_validator(forecast_file)
#> aquatics-2021-09-27-example_null.csv.gz
#> ✓ file name is correct
#> Rows: 140 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (2): siteID, statistic
#> dbl  (2): oxygen, temperature
#> date (1): time
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ✓ target variables found
#> ✓ file has summary statistics column
#> ✓ file has summary statistic: mean
#> ✓ file has summary statistic: sd
#> ✓ file has siteID column
#> ✓ file has time column
#> Rows: 140 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (2): siteID, statistic
#> dbl  (2): oxygen, temperature
#> date (1): time
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ✓ file has correct time column
#> [1] TRUE
```

### Generate forecast metadata in EML

``` r
create_model_metadata(forecast_file)
#> You only need to run this function once to generate the model metadata template.
#> If you model does not change between submittions you will not need change the yml.
#> In this case, use a previously generated yaml in the write_metadata_eml() call
#> If your model does change, save your old yaml under a new name and modify
#> • Edit './aquatics-example_null.yml'
```

### Access EFI snapshots of NOAA forecasts at NEON sites

Many forecasts will want to make use of weather forecasts as potential
drivers. EFI downscales NOAA GEFS 35-day forecast products at each NEON
site and makes this data available. These helper functions provide
convenient access for downloading and stacking the individual forecast
files.

``` r
aq_sites <- unique(aquatic$siteID)
download_noaa(aq_sites)
noaa_fc <- stack_noaa()
noaa_fc
#> # A tibble: 8,590 × 18
#>    model    interval siteID runStartDate  runEndDate    ensemble air_temperature
#>    <chr>    <chr>    <chr>  <chr>         <chr>         <chr>              <dbl>
#>  1 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               298.
#>  2 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               294.
#>  3 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               294.
#>  4 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               304.
#>  5 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               298.
#>  6 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               294.
#>  7 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               293.
#>  8 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               304.
#>  9 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               298.
#> 10 NOAAGEFS 6hr      BARC   2021-09-25T00 2021-10-11T00 ens00               294.
#> # … with 8,580 more rows, and 11 more variables: air_pressure <dbl>,
#> #   relative_humidity <dbl>, surface_downwelling_longwave_flux_in_air <dbl>,
#> #   surface_downwelling_shortwave_flux_in_air <dbl>, precipitation_flux <dbl>,
#> #   specific_humidity <dbl>, cloud_area_fraction <dbl>, wind_speed <dbl>,
#> #   time <dttm>, latitude <dbl>, longitude <dbl>
```

### Submit a forecast

When you are ready to submit your forecast to EFI:

``` r
submit(forecast_file)
#> aquatics-2021-09-27-example_null.csv.gz
#> ✓ file name is correct
#> Rows: 140 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (2): siteID, statistic
#> dbl  (2): oxygen, temperature
#> date (1): time
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ✓ target variables found
#> ✓ file has summary statistics column
#> ✓ file has summary statistic: mean
#> ✓ file has summary statistic: sd
#> ✓ file has siteID column
#> ✓ file has time column
#> Rows: 140 Columns: 5
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (2): siteID, statistic
#> dbl  (2): oxygen, temperature
#> date (1): time
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> ✓ file has correct time column
```

Ideally you should include the optional `metadata =` argument with your
metadata file.

### Other functions

Encountered a bug? Facing another challenge in participating in the
challenge? Developed a cool approach you would like to share with the
community? Open an [issue](https://github.com/eco4cast/neon4cast/issues)
or [pull request](https://github.com/eco4cast/neon4cast/pulls) here!
