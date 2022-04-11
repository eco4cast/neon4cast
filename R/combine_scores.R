#' Combining scores from a theme together
#'
#' @param x theme name (optional)
#' @param collect TRUE/FALSE to download results
#' @return a data.frame of scores
#' @export
combined_scores <- function(x = NA, collect = TRUE){
  Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
  Sys.unsetenv("AWS_ACCESS_KEY_ID")
  Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.unsetenv("AWS_S3_ENDPOINT")
  
  #GENERALIZATION: THIS IS A SPECIFIC ENDPOINT
  s3 <- arrow::s3_bucket(bucket = "scores",
                         endpoint_override = "data.ecoforecast.org")
  ds <- arrow::open_dataset(s3, partitioning=c("theme", "year"))
  if (!is.na(x)) {
  ds <- dplyr::filter(ds, theme == {{x}})
  }
  if (collect) {
    ds <- dplyr::collect(ds)
  }
  ds
}
globalVariables("theme", "neon4cast")

