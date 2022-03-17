test_that("basic examples run", {

## ------------------------------------------------------------------------------------------------------------
forecast_file <- system.file("extdata/aquatics-2021-02-01-EFInull.csv.gz", 
                             package = "neon4cast")

## ------------------------------------------------------------------------------------------------------------
status <- forecast_output_validator(forecast_file)
expect_true(status)

## ------------------------------------------------------------------------------------------------------------
df <- score(forecast_file, "https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz", theme="aquatics")
expect_true(is(df, "tbl"))


nc_fc <- read_forecast("https://data.ecoforecast.org/forecasts/terrestrial_30min/terrestrial_30min-2022-01-01-hist30min.nc")
score(nc_fc, "https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz", theme="terrestrial_30min")
## ------------------------------------------------------------------------------------------------------------
#download_noaa("ABBY")
#abby <- stack_noaa()
#expect_s3_class(abby, "data.frame")


## ------------------------------------------------------------------------------------------------------------
#status <- submit(forecast_file)
#expect_null(status)

})
