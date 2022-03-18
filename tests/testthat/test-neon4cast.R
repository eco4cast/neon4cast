test_that("basic examples run", {

## ------------------------------------------------------------------------------------------------------------
forecast_file <- system.file("extdata/aquatics-2021-02-01-EFInull.csv.gz", 
                             package = "neon4cast")

## ------------------------------------------------------------------------------------------------------------
status <- forecast_output_validator(forecast_file)
expect_true(status)

df <- score(forecast_file, "https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz", theme="aquatics")
expect_true(is(df, "tbl"))


fc <- "https://data.ecoforecast.org/forecasts/aquatics/aquatics-2020-09-01-EFInull.csv.gz"
df <- score(fc, 
            "https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz")
expect_true(inherits(df, "data.frame"))

})
