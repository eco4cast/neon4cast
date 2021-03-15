
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
```

### Validate a forecast file

``` r
example <- "https://data.ecoforecast.org/forecasts/aquatics/aquatics-2021-03-01-EFInull.csv.gz"
download.file(example, "aquatics-2021-03-01-EFInull.csv.gz")
forecast_output_validator("aquatics-2021-03-01-EFInull.csv.gz")
#> aquatics-2021-03-01-EFInull.csv.gz
#> ● Checking validity of file name...
#> ✓ file name is correct
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   time = col_date(format = ""),
#>   ensemble = col_double(),
#>   siteID = col_character(),
#>   oxygen = col_double(),
#>   temperature = col_double(),
#>   obs_flag = col_double(),
#>   forecast = col_double(),
#>   data_assimilation = col_double()
#> )
#> ● Checking that file contains correct variables...
#> ✓ target variables found
#> ● Checking that file contains either ensemble or statistic column...
#> ✓ file has ensemble members
#> ● Checking that file contains siteID column...
#> ✓ file has siteID column
#> ● Checking that file contains parsable time column...
#> ✓ file has time column
#> ✓ file has correct time column
#> [1] TRUE
```

### Compute forecast scores locally

### Generate forecast metadata in EML

### Access EFI snapshots of NOAA forecasts at NEON sites

### Submit a forecast

### Other functions

Encountered a bug? Facing another challenge in participating in the
challenge? Developed a cool approach you would like to share with the
community? Open an [issue](https://github.com/eco4cast/neon4cast/issues)
or [pull request](https://github.com/eco4cast/neon4cast/pulls) here\!
