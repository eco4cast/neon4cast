#' Combining scores from a theme together
#'
#' @param theme theme name
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

#' Calculating forecast challenge submission statistics
#'
#' @param themes theme names 
#' @return a data.frame of challenge statistics
#' @export

theme_statistics <- function(themes){
  
theme_stats <- purrr::map_dfr(themes, function(theme){
  
  message(theme)
  
  theme_scores <- neon4cast::combined_scores(theme = theme, collect = FALSE)
  
  teams <- theme_scores |> 
    dplyr::summarise(n = dplyr::n_distinct(model_id)) |> 
    dplyr::collect() |> 
    dplyr::pull(n)
  
  forecasts <- theme_scores |> 
    dplyr::select(model_id,reference_datetime, variable) |> 
    dplyr::distinct() |> 
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |> 
    dplyr::summarise(total = sum(n)) |> 
    dplyr::collect() |> 
    dplyr::pull(total)
  
  forecast_obs <- theme_scores |> 
    dplyr::filter(!is.na(crps)) |> 
    dplyr::summarise(n = n(), .groups = "drop") |> 
    dplyr::summarise(total = sum(n)) |> 
    dplyr::collect() |> 
    dplyr::pull(total)
  
  output <- tibble::tibble(theme = theme, n_teams = teams, n_submissions = forecasts, n_obs_forecasts_pairs = forecast_obs)
  
  return(output)
  })

return(theme_stats)

}
globalVariables("theme", "neon4cast")

