test_that("basic examples run", {

fc <- "https://data.ecoforecast.org/neon4cast-forecasts/raw/aquatics/aquatics-2022-11-01-climatology.csv.gz"
status <- forecast_output_validator(fc)
expect_true(status)

df <- score(fc, 
            "https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz")
expect_true(inherits(df, "data.frame"))

})



## Add tests for NOAA 


## Add tests for import