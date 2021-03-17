


#' Format a fable fbl_ts forecast in EFI standard
#' 
#' @param df a fbl_ts forecast
#' @param times number of times to draw from distribution for ensemble method,
#' ignored if distributions are normal.
#' @return A data.frame (`[tsibble]`) in the EFI format
#' @examples 
#' library(dplyr)
#' library(readr)
#' library(fable)
#' aquatic <- read_csv("https://data.ecoforecast.org/targets/aquatics/aquatics-targets.csv.gz") %>% 
#' as_tsibble(index=time, key=siteID)
#' oxygen_fc <- aquatic %>%
#'   model(null = MEAN(oxygen)) %>%
#'   forecast(h = "35 days") %>%
#'   efi_format()
#' 
#' @export
#' @importFrom rlang .data `:=`
efi_format <- function(df, times = 100){
  
  var <- attributes(df)$dist
  is_normal <- vapply(df[[var]], inherits, logical(1L), "dist_normal")
  if(all(is_normal))
    efi_statistic_format(df)  
  else
    efi_ensemble_format(df, times)
  
}

## Helper functions to turn a fable timeseries, which uses a special "distribution" column,
## into a flat-file format.  efi_statistic_format uses a 'statistic' column (indicating either mean or sd),
## while efi_ensemble_format uses an 'ensemble' column, drawing `n` times from the distribution. 
efi_statistic_format <- function(df){
  
  stopifnot(inherits(df, "fbl_ts"))
  
  
  ## determine variable name
  var <- attributes(df)$dist
  ## Normal distribution: use distribution mean and variance
  df %>% 
    dplyr::mutate(sd = sqrt( distributional::variance( .data[[var]] ) ) ) %>%
    dplyr::rename(mean = .mean) %>%
    dplyr::select(time, siteID, .model, mean, sd) %>%
    tidyr::pivot_longer(c(mean, sd), names_to = "statistic", values_to = var) %>%
    dplyr::as_tibble()
}

utils::globalVariables(c("siteID", "time", "sd", ".model",
                         ".mean", "ensemble"), "neon4cast")

efi_ensemble_format <- function(df, times = 10) {
  
  stopifnot(inherits(df, "fbl_ts"))
  
  
  ## determine variable name
  var <- attributes(df)$dist
  n_groups <- nrow(df)
  ## Draw `times` samples from distribution using 
  suppressWarnings({
    expand <- df %>% 
      dplyr::mutate(sample = distributional::generate(  .data[[var]], times) )
  })
  expand %>%
    tidyr::unnest(sample) %>%
    dplyr::mutate(ensemble = rep(1:times, n_groups)) %>%
    dplyr::select(time, siteID, ensemble, {{var}} := sample) %>%
    dplyr::as_tibble()
}


