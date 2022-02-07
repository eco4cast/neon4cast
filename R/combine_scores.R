#' Combining scores from a theme together
#'
#' @param x theme name
#' @param collect TRUE/FALSE to download results
#'
#' @return
#' @export
combined_scores <- function(x, collect = TRUE){
  Sys.setenv("AWS_EC2_METADATA_DISABLED"="TRUE")
  Sys.unsetenv("AWS_ACCESS_KEY_ID")
  Sys.unsetenv("AWS_SECRET_ACCESS_KEY")
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.unsetenv("AWS_S3_ENDPOINT")
  
  s <- neon4cast::score_schema()
  s3 <- arrow::s3_bucket(bucket = "scores",
                         endpoint_override = "data.ecoforecast.org",
                         anonymous=TRUE)
  ds <- arrow::open_dataset(s3, schema=s, format = "csv", skip_rows = 1)
  df <- ds %>% filter(theme == x)
  if(collect) {
    df <- df %>% collect()
  }
  df
}