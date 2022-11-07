#' Combining scores from a theme together
#'
#' @param x theme name (optional)
#' @param collect TRUE/FALSE to download results
#' @return a data.frame of scores
#' @export
combined_scores <- function(theme, collect = TRUE){

  vars <- neon4cast:::arrow_env_vars()
  
  #GENERALIZATION: THIS IS A SPECIFIC ENDPOINT
  s3 <- arrow::s3_bucket(bucket = paste0("neon4cast-scores/parquet/", theme),
                         endpoint_override = "data.ecoforecast.org", 
                         anonymous = TRUE)
  ds <- arrow::open_dataset(s3)
  if (collect) {
    ds <- dplyr::collect(ds)
  }
  on.exit(neon4cast:::unset_arrow_vars(vars))
  ds
}
globalVariables("theme", "neon4cast")

