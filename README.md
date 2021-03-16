
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
example <- "https://data.ecoforecast.org/forecasts/aquatics/aquatics-2021-02-01-EFInull.csv.gz"
download.file(example, "aquatics-2021-02-01-EFInull.csv.gz")
forecast_output_validator("aquatics-2021-02-01-EFInull.csv.gz")
#> aquatics-2021-02-01-EFInull.csv.gz
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
#> ✓ target variables found
#> ✓ file has ensemble members
#> ✓ file has siteID column
#> ✓ file has time column
#> ✓ file has correct time column
#> [1] TRUE
```

### Compute forecast scores locally

``` r
forecast <- readr::read_csv("aquatics-2021-02-01-EFInull.csv.gz")
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
score(forecast, theme = "aquatics")
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   time = col_date(format = ""),
#>   siteID = col_character(),
#>   oxygen = col_double(),
#>   temperature = col_double(),
#>   oxygen_sd = col_double(),
#>   temperature_sd = col_double(),
#>   depth_oxygen = col_double(),
#>   depth_temperature = col_double(),
#>   neon_product_ids = col_character()
#> )
#> # A tibble: 28 x 4
#>    siteID time       target        score
#>    <chr>  <date>     <chr>         <dbl>
#>  1 BARC   2021-02-01 oxygen       0.0843
#>  2 BARC   2021-02-01 temperature NA     
#>  3 BARC   2021-02-02 oxygen       0.103 
#>  4 BARC   2021-02-02 temperature NA     
#>  5 BARC   2021-02-03 oxygen       0.209 
#>  6 BARC   2021-02-03 temperature NA     
#>  7 BARC   2021-02-04 oxygen       0.224 
#>  8 BARC   2021-02-04 temperature NA     
#>  9 BARC   2021-02-05 oxygen       0.211 
#> 10 BARC   2021-02-05 temperature NA     
#> # … with 18 more rows
```

### Generate forecast metadata in EML

Coming soon\!

### Access EFI snapshots of NOAA forecasts at NEON sites

``` r
download_noaa("ABBY")
abby <- stack_noaa()
abby
#> # A tibble: 4,295 x 13
#>    file  air_temperature air_pressure relative_humidity surface_downwelling_lon…
#>    <chr>           <dbl>        <dbl>             <dbl>                    <dbl>
#>  1 1                286.       97149.             0.495                      NaN
#>  2 1                279.       96964.             0.786                      268
#>  3 1                277.       96738.             0.955                      279
#>  4 1                283.       96376.             0.643                      302
#>  5 1                279.       96320.             0.981                      335
#>  6 1                272.       96824.             0.947                      309
#>  7 1                270.       96937.             0.916                      222
#>  8 1                276.       97024.             0.771                      259
#>  9 1                278.       96950.             0.713                      286
#> 10 1                271.       97160.             0.936                      236
#> # … with 4,285 more rows, and 8 more variables:
#> #   surface_downwelling_shortwave_flux_in_air <dbl>, precipitation_flux <dbl>,
#> #   specific_humidity <dbl>, cloud_area_fraction <dbl>, wind_speed <dbl>,
#> #   time <dbl>, latitude <dbl>, longitude <dbl>
```

### Submit a forecast

``` r
submit("aquatics-2021-02-01-EFInull.csv.gz")
```

### Other functions

Encountered a bug? Facing another challenge in participating in the
challenge? Developed a cool approach you would like to share with the
community? Open an [issue](https://github.com/eco4cast/neon4cast/issues)
or [pull request](https://github.com/eco4cast/neon4cast/pulls) here\!
