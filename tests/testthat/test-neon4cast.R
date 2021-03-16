test_that("basic examples run", {

## ------------------------------------------------------------------------------------------------------------
forecast_file <- system.file("extdata/aquatics-2021-02-01-EFInull.csv.gz", 
                             package = "neon4cast")

## ------------------------------------------------------------------------------------------------------------
status <- forecast_output_validator(forecast_file)
expect_true(status)

## ------------------------------------------------------------------------------------------------------------
forecast <- readr::read_csv(forecast_file)
df <- score(forecast, theme = "aquatics")
expect_is(df, "data.frame")

## ------------------------------------------------------------------------------------------------------------
download_noaa("ABBY")
abby <- stack_noaa()
expect_is(abby, "data.frame")


## ------------------------------------------------------------------------------------------------------------
status <- submit(forecast_file)
expect_null(status)

})
