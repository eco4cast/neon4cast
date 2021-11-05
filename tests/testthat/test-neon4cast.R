test_that("basic examples run", {

## ------------------------------------------------------------------------------------------------------------
forecast_file <- system.file("extdata/aquatics-2021-02-01-EFInull.csv.gz", 
                             package = "neon4cast")

## ------------------------------------------------------------------------------------------------------------
status <- forecast_output_validator(forecast_file)
expect_true(status)

## ------------------------------------------------------------------------------------------------------------
df <- score(forecast_file, theme = "aquatics")
expect_true(is(df, "tbl"))

## ------------------------------------------------------------------------------------------------------------
download_noaa("ABBY")
abby <- stack_noaa()
expect_s3_class(abby, "data.frame")


## ------------------------------------------------------------------------------------------------------------
#status <- submit(forecast_file)
#expect_null(status)

})
