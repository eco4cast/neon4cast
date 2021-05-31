
``` r
knitr::opts_chunk$set(message=FALSE)
```

``` r
library(neonstore)
library(neon4cast) # remotes::install_github("eco4cast/neon4cast")
library(tidyverse)
library(tsibble)
library(fable)
```

# A trivial forecast

## Access Target Data

``` r
targets <-
  "https://data.ecoforecast.org/targets/beetles/beetles-targets.csv.gz" %>% 
  read_csv(col_types = "cDdd") %>% 
  as_tsibble(index = time, key = siteID)
```

For illustrative purposes, we will pretend most recent year is “future”
data. In practice, “past” would be the `targets` data.frame downloaded
on the day code is run (i.e. most recent data), while “future” would be
the “targets” data downloaded some period of time (i.e. 1 yr) after when
it is possible to test if the forecast was accurate.

``` r
past <-  targets %>% filter(time < max(time) - 365)
future <- targets %>% filter(time >= max(time) - 365)

forecast_date <- max(targets$time) - 365
```

## Compute a forecast

``` r
## Compute a simple mean/sd model per site... obviously silly given huge seasonal aspect
null_richness <- past  %>% 
  model(null = MEAN(richness)) %>%
  forecast(h = "1 year")

null_abundance <- past  %>%
  model(null = MEAN(abundance)) %>%
  forecast(h = "1 year")
```

## Visualize the forecast

``` r
first4 <- unique(null_richness$siteID)[1:4]

null_richness %>% filter(siteID %in% first4)  %>% autoplot(past) + ggtitle("richness")
```

![](beetles_example_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
null_abundance %>% filter(siteID %in% first4)  %>% autoplot(targets) + ggtitle("abundance")
```

![](beetles_example_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

## Score the forecast

`fable` can compute CRPS, by default computes averages by siteID (`key`)
and `.model` only.  
Optionally, we could include `by = c(".model", "siteID", "time")` to
generate separate forecasts by time. (For some reason `fable`s internal
method is much slower than the EFI code, though results are numerically
equivalent). Note that fable can work across multiple models, (though we
have only one in this case), but not across multiple target variables.
We must handle predictions of `richness` and `abundance` separately.
Recall that smaller CRPS scores are better.

``` r
null_richness %>%
  accuracy(future, list(crps = CRPS))
```

    ## Warning: The future dataset is incomplete, incomplete out-of-sample data will be treated as missing. 
    ## 67 observations are missing between 2019-05-27 and 2020-10-05

    ## # A tibble: 47 x 4
    ##    .model siteID .type    crps
    ##    <chr>  <chr>  <chr>   <dbl>
    ##  1 null   ABBY   Test    1.17 
    ##  2 null   BARR   Test  NaN    
    ##  3 null   BART   Test  NaN    
    ##  4 null   BLAN   Test    3.38 
    ##  5 null   BONA   Test    0.714
    ##  6 null   CLBJ   Test  NaN    
    ##  7 null   CPER   Test    1.17 
    ##  8 null   DCFS   Test  NaN    
    ##  9 null   DEJU   Test    1.64 
    ## 10 null   DELA   Test  NaN    
    ## # … with 37 more rows

## EFI Formatting

EFI requires a flat-file format for forecasts that avoids the use of
complex list columns.  
To convey uncertainty, forecasts must be expressed either by giving mean
and standard deviation (for predictions that are normally distributed)
or must express forecasts as an ensemble of replicate draws from
forecast distribution. The helper function `efi_format()` handles this
transformation.

``` r
## Combine richness and abundance forecasts. drop the 'model' column
null_forecast <- inner_join(efi_format(null_richness), 
                            efi_format(null_abundance)) %>%
  select(!.model) # we have only one model
```

Score the forecast using EFI’s internal method. By default, EFI’s method
reports the score every unique site-time combination (unique grouping
variables). It is easy to later average across times for a by-site
score.

``` r
scores_null <- neon4cast::score(null_forecast, "beetles")
# average richness scores by site
scores_null %>% group_by(target) %>% summarise(mean = mean(score, na.rm=TRUE)) 
```

    ## # A tibble: 2 x 2
    ##   target      mean
    ##   <chr>      <dbl>
    ## 1 abundance 0.0912
    ## 2 richness  2.16

# Richer models: ARIMA

ARIMA models are commonly used to predict future values from historical
values, ([FPP Ch 9](https://otexts.com/fpp3/arima.html)). A simple ARIMA
model does not use any external driver variables, though it is possible
to include regression (e.g. see [FPP Ch 10 on Dynamic
Models](https://otexts.com/fpp3/dynamic.html))

Our ARIMA model needs only one extra step, making implicit missing data
into explicit missing values. In both richness and abundance, we will
treat gaps as missing data, not as zeros, using \`

``` r
gap_filled <- tsibble::fill_gaps(past)
arima_richness <- gap_filled  %>% 
  model(arima = ARIMA(richness)) %>%
  forecast(h = "1 year") %>%
  efi_format()

arima_abundance <- gap_filled  %>%
  model(arima = ARIMA(abundance)) %>%
  forecast(h = "1 year") %>%
  efi_format()
```

    ## Warning in sqrt(diag(best$var.coef)): NaNs produced
    
    ## Warning in sqrt(diag(best$var.coef)): NaNs produced

``` r
## Combine richness and abundance forecasts. drop the 'model' column
arima_forecast <- inner_join(arima_richness, arima_abundance) %>% select(!.model)
```

Score the forecast

``` r
arima_scores <- neon4cast::score(arima_forecast, "beetles")
arima_scores %>% group_by(target) %>% summarise(mean = mean(score, na.rm=TRUE))
```

    ## # A tibble: 2 x 2
    ##   target     mean
    ##   <chr>     <dbl>
    ## 1 abundance 0.638
    ## 2 richness  8.79

# Process vs Noise

As the example plots above show, counts of abundance and richness at
individual sites and weeks are pretty noisy, with lots of gaps and zeros
and no strong visual pattern. In contrast, if we average by month over
all sites nationally, we see a strong and predictable seasonal pattern:

``` r
national_ave <- past %>%
  index_by(Time = ~ yearmonth(.)) %>% # temporal aggregates
  summarise(
    richness = mean(richness, na.rm = TRUE),
    abundance = mean(abundance, na.rm = TRUE)
  ) %>% rename(time = Time)



national_ave %>% 
  pivot_longer(c("richness", "abundance"), 
               names_to = "variable", 
               values_to="mean_observed") %>% 
  ggplot(aes(time, mean_observed)) + geom_line() +
  facet_wrap(~variable, ncol = 1, scales = "free_y")
```

![](beetles_example_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

While a hierarchical forecast could allow us to pool power across sites,
a simple starting point might be to try and back out the site-level
predictions based on a forecast of this strong seasonal pattern.  
An ARIMA model is a good way to capture this periodic trend. We will use
log-transforms as a simple way to avoid the possibility of negative
values in our predictions.

``` r
ave_richness <- national_ave %>%
  fill_gaps() %>%
  model(arima = ARIMA(log(richness))) %>%
  forecast(h = "1 year")

ave_richness %>% autoplot(national_ave) + ggtitle("richness")
```

![](beetles_example_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ave_abund <- national_ave %>%
  fill_gaps() %>%
  model(arima = ARIMA(log(abundance))) %>%
  forecast(h = "1 year")

ave_abund %>% autoplot(national_ave) + ggtitle("abundance")
```

![](beetles_example_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

We treat the sites as merely random draws from this national pool,
weighted by the fraction that each site has contributed to the grand
total observed richness.

``` r
richness_weights <- 
  past %>% as_tibble() %>% 
  group_by(siteID) %>% 
  summarise(richness_share = sum(richness, na.rm=TRUE)) %>% 
  mutate(richness_share = richness_share / sum(richness_share))
richness_weights
```

    ## # A tibble: 47 x 2
    ##    siteID richness_share
    ##    <chr>           <dbl>
    ##  1 ABBY          0.0221 
    ##  2 BARR          0.00152
    ##  3 BART          0.0373 
    ##  4 BLAN          0.0480 
    ##  5 BONA          0.00238
    ##  6 CLBJ          0.0131 
    ##  7 CPER          0.0453 
    ##  8 DCFS          0.0188 
    ##  9 DEJU          0.00628
    ## 10 DELA          0.0208 
    ## # … with 37 more rows

``` r
# Note distribution is normal in log-transformed variable
ave_richness %>% 
    dplyr::mutate(sd = sqrt( distributional::variance( richness ) ) ) %>%
    dplyr::rename(mean = .mean) %>%
    dplyr::select(time, .model, mean, sd) %>%
    tidyr::pivot_longer(c(mean, sd), names_to = "statistic", values_to = "richness") %>%
    dplyr::as_tibble()
```

    ## # A tibble: 24 x 4
    ##        time .model statistic richness
    ##       <mth> <chr>  <chr>        <dbl>
    ##  1 2019 Nov arima  mean         2.37 
    ##  2 2019 Nov arima  sd           0.740
    ##  3 2019 Dec arima  mean         2.96 
    ##  4 2019 Dec arima  sd           1.23 
    ##  5 2020 Jan arima  mean         2.76 
    ##  6 2020 Jan arima  sd           1.21 
    ##  7 2020 Feb arima  mean         2.21 
    ##  8 2020 Feb arima  sd           0.967
    ##  9 2020 Mar arima  mean         3.93 
    ## 10 2020 Mar arima  sd           1.72 
    ## # … with 14 more rows

Combining the forecast for the nation-wide pooled mean richness with the
site-by-site richness share, we could then generate site-level forecasts
for each month (not shown).  
This is still a trivial model which treats differences between sites as
fixed over time, but illustrates the basic idea of deriving the
observational data from the forecast of a latent variable (in this case,
the national means). A more realistic model might similarly seek to
estimate latent variables for ‘true abundance’ and ‘true richness’ from
an explicit observation model, forecast the latent values, and back out
the expected distribution of observational values.

# Including additional driver data

## NOAA data access

A richer forecast may make use of NOAA weather predictions. Default is
to get current forecast going ahead for the next 30 days. Some older
forecasts are available, but only back to 2020-09-25 when regular EFI
archiving began.

``` r
sites <- unique(targets$siteID)
# lapply(sites, neon4cast::download_noaa, dir = neonstore::neon_dir())
# noaa <- neon4cast::stack_noaa(dir = neonstore::neon_dir())

## cache stacked files:
# write_csv(noaa, "noaa.csv.gz")
```

``` r
noaa <- read_csv("noaa.csv.gz")
```

## NEON weather data

We can also use NEON’s local measurements of the meteorological
variables. NEON’s permanent sites make convenient summary weather data
available.

``` r
# One-time commands for import and storage of weather data
# neon_download("DP4.00001.001")
# neon_store(product = "DP4.00001.001")

## Weather files can get big! We'll use a remote database connection
db <- neon_db()

## View available tables under this product ID
tables <- DBI::dbListTables(db)
tables[grepl("DP4.00001.001", tables)]
```

    ## [1] "sensor_positions-DP4.00001.001"        
    ## [2] "wss_daily_humid-basic-DP4.00001.001"   
    ## [3] "wss_daily_precip-basic-DP4.00001.001"  
    ## [4] "wss_daily_pres-basic-DP4.00001.001"    
    ## [5] "wss_daily_shortRad-basic-DP4.00001.001"
    ## [6] "wss_daily_temp-basic-DP4.00001.001"    
    ## [7] "wss_daily_wind-basic-DP4.00001.001"

``` r
## Create remote connections to these tables
temp <- tbl(db, "wss_daily_temp-basic-DP4.00001.001")

# Similarly we could grab other tables of interest
precip <- tbl(db, "wss_daily_precip-basic-DP4.00001.001")
humid <- tbl(db, "wss_daily_humid-basic-DP4.00001.001")

## Determine which sites we have data for
fixed_sites <- temp %>% select(siteID) %>% distinct() %>% pull()
```

Using `duckdb` date manipulation (`strftime`) and dplyr SQL translation,
we can very quickly compute weekly averages on disk:

``` r
temp <- tbl(db, "wss_daily_temp-basic-DP4.00001.001")

weekly_temp <- 
  temp %>% 
  mutate(time = as.Date(date_trunc("week", date))) %>% 
  group_by(time, siteID) %>% 
  summarise(mean_temp = mean(wssTempTripleMean, na.rm=TRUE),
            min_temp = mean(wssTempTripleMinimum, na.rm=TRUE),
            max_temp = mean(wssTempTripleMaximum, na.rm=TRUE),
            .groups = "drop") %>% 
  collect() %>% # Now import summarized results into R, turn into tsibble:
  as_tsibble(index=time, key=siteID)
```

All though the DP4 weather data covers only the permanent sites, we can
just easily compute weekly means over the low-level triple-aspirated
mean temperature data collected at all sites ourselves. (Note this could
be easily modified to group by day first to compute daily minimum and
daily maximums):

``` r
weekly_taat <- 
   tbl(db, "TAAT_30min-basic-DP1.00003.001") %>% 
  mutate(time = as.Date(date_trunc("week", startDateTime))) %>% 
  group_by(time, siteID) %>% 
  summarise(mean_temp = mean(tempTripleMean, na.rm=TRUE),
            .groups = "drop") %>% 
  collect() %>% # Now import summarized results into R, turn into tsibble:
  as_tsibble(index=time, key=siteID)
```

## Regression models

Is there any correlation between abundance and temperature?

``` r
past_w_covars <- left_join(past, weekly_temp)
```

We can try a simple time series linear model regression on min, max, and
mean daily temperatures: (For simplicity we just use fixed sites, though
as noted above it would be straightforward to compute daily min and max
temp for all sites).

``` r
report <- past_w_covars %>% 
  filter(siteID %in% fixed_sites) %>% 
  model(mean = TSLM(abundance ~ mean_temp),
        max = TSLM(abundance ~ max_temp),
        min = TSLM(abundance ~ min_temp)
        ) %>%
  report()
```

    ## Warning in report.mdl_df(.): Model reporting is only supported for individual
    ## models, so a glance will be shown. To see the report for a specific model, use
    ## `select()` and `filter()` to identify a single model.

``` r
report
```

    ## # A tibble: 60 x 16
    ##    siteID .model r_squared adj_r_squared  sigma2 statistic p_value    df log_lik
    ##    <chr>  <chr>      <dbl>         <dbl>   <dbl>     <dbl>   <dbl> <int>   <dbl>
    ##  1 BONA   mean      0.184        0.110   1.34e-3     2.49  0.143       2    25.6
    ##  2 BONA   max       0.264        0.197   1.21e-3     3.95  0.0723      2    26.3
    ##  3 BONA   min       0.0131      -0.0766  1.62e-3     0.146 0.709       2    24.4
    ##  4 CLBJ   mean      0.0495       0.0207  2.51e-4     1.72  0.199       2    96.5
    ##  5 CLBJ   max       0.104        0.0765  2.36e-4     3.82  0.0593      2    97.5
    ##  6 CLBJ   min       0.0231      -0.00646 2.58e-4     0.782 0.383       2    96.0
    ##  7 CPER   mean      0.133        0.117   1.23e-1     8.57  0.00493     2   -20.6
    ##  8 CPER   max       0.142        0.126   1.22e-1     9.24  0.00359     2   -20.3
    ##  9 CPER   min       0.127        0.112   1.24e-1     8.17  0.00598     2   -20.8
    ## 10 GUAN   mean      0.207        0.169   8.03e-6     5.47  0.0293      2   103. 
    ## # … with 50 more rows, and 7 more variables: AIC <dbl>, AICc <dbl>, BIC <dbl>,
    ## #   CV <dbl>, deviance <dbl>, df.residual <int>, rank <int>

``` r
report %>% 
  filter(p_value < 0.05) %>% 
  count(.model)
```

    ## # A tibble: 3 x 2
    ##   .model     n
    ##   <chr>  <int>
    ## 1 max        3
    ## 2 mean       4
    ## 3 min        3

We can try a forecast based on this regression pattern of daily minimum.
This will of course need future values of mean\_temp predictor variable.
Typically, that ‘new data’ would most likely be based on the NOAA
forecast for temperatures at each site, as mentioned above. As we have
withheld the last year of data for this example, we can simply use the
data NEON measured at the site for testing purposes here:

``` r
new_data <- weekly_temp %>% filter(time > forecast_date, siteID %in% fixed_sites)

tslm_abundance <-  past_w_covars %>%
  filter(siteID %in% fixed_sites)  %>% 
  model(mean = TSLM(abundance ~ mean_temp)) %>%
  forecast(h = "1 year", new_data = new_data)
```

    ## Warning: Input forecast horizon `h` will be ignored as `new_data` has been
    ## provided.

The resulting skill seems comparable to the mean; perhaps not surprising
since few of the site-wise correlation were significant.

``` r
tslm_abundance %>% accuracy(future, list(crps = CRPS)) 
```

    ## Warning: The future dataset is incomplete, incomplete out-of-sample data will be treated as missing. 
    ## 77 observations are missing between 2019-10-14 and 2021-03-29

    ## # A tibble: 20 x 4
    ##    .model siteID .type       crps
    ##    <chr>  <chr>  <chr>      <dbl>
    ##  1 mean   BONA   Test    0.0129  
    ##  2 mean   CLBJ   Test    0.00757 
    ##  3 mean   CPER   Test    0.232   
    ##  4 mean   GUAN   Test    0.000894
    ##  5 mean   HARV   Test    0.0466  
    ##  6 mean   KONZ   Test    0.118   
    ##  7 mean   NIWO   Test    0.312   
    ##  8 mean   ONAQ   Test  NaN       
    ##  9 mean   ORNL   Test    0.0281  
    ## 10 mean   OSBS   Test  NaN       
    ## 11 mean   PUUM   Test    0.115   
    ## 12 mean   SCBI   Test    0.0261  
    ## 13 mean   SJER   Test    0.00771 
    ## 14 mean   SRER   Test  NaN       
    ## 15 mean   TALL   Test  NaN       
    ## 16 mean   TOOL   Test    0.163   
    ## 17 mean   UNDE   Test    0.128   
    ## 18 mean   WOOD   Test    0.133   
    ## 19 mean   WREF   Test    0.0267  
    ## 20 mean   YELL   Test  NaN
