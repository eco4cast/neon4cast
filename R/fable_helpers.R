


#' Format a fable fbl_ts forecast in EFI standard
#' 
#' @param df a fbl_ts forecast
#' @param times number of times to draw from distribution for ensemble method,
#' ignored if distributions are normal.
#' @return A data.frame (`[tsibble]`) in the EFI format
#' @examples 
#' 
#' \dontrun{
#' library(dplyr)
#' library(readr)
#' library(fable)
#' aquatic <-
#'   read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz") %>%
#'   pivot_wider(names_from = "variable", values_from = "observation") %>%
#'   as_tsibble(index = datetime, key = site_id)
#' oxygen_fc <- aquatic %>%
#'   model(null = MEAN(oxygen)) %>%
#'   forecast(h = "35 days") %>%
#'   efi_format()
#'  }
#' @export
#' @importFrom rlang .data `:=`
#' @importFrom dplyr `%>%`
efi_format <- function(df, times = 10){
  
  df <- drop_degenerate(df)
  var <- attributes(df)$dist
  family <- unique(stats::family(df[[var]]))
  
  if(length(unique(family)) > 1)
    stop("Multiple distributions detected. Please provide only one distribution type at a time.")
  
  if(family %in% c("normal"))
    efi_format_statistic(df, family)  
  else
    efi_format_ensemble(df, times)
  
}

## Helper functions to turn a fable timeseries, which uses a special "distribution" column,
## into a flat-file format.  efi_statistic_format uses a 'statistic' column (indicating either mean or sd),
## while efi_ensemble_format uses an 'ensemble' column, drawing `n` times from the distribution. 
efi_format_statistic <- function(df, family = NULL){
  
  stopifnot(inherits(df, "fbl_ts"))
  ## determine variable name
  var <- attributes(df)$dist
  
  if(is.null(family))
    family <- unique(stats::family(df[[var]]))
  
  
  df %>% 
    dplyr::mutate(family = family) %>%
    dplyr::rename(model_id = .model) %>%
    dplyr::select(-.mean) %>%
    tidyr::pivot_longer(-dplyr::starts_with(standard_vars),
                        names_to = "variable", 
                        values_to = "dist") %>% 
    dplyr::mutate(pars = distributional::parameters(dist)) %>%
    tidyr::unnest(pars) %>%
    dplyr::select(-dist) %>%
    tidyr::pivot_longer(-dplyr::starts_with(standard_vars),
                        names_to = "parameter", values_to = "prediction") %>%
    dplyr::as_tibble()

}


standard_vars <- c("site_id", "datetime", "parameter", "family",
                   "reference_datetime", "prediction", "observation",
                   "model_id", "model_name", "latitude", "longitude",
                   "variable")

utils::globalVariables(c("sd", ".model", "n", "dist", "pars",
                         ".mean", "ensemble", standard_vars),
                       "neon4cast")


#' Format as EFI using ensemble draws
#' @inheritParams efi_format
#' @param times number of ensemble members to draw
#' Supports unrecognized distributions by drawing samples
#' @export
efi_format_ensemble <- function(df, times = 10) {
  
  stopifnot(inherits(df, "fbl_ts"))
  
  var <- attributes(df)$dist
  df <- drop_degenerate(df)

  ## determine variable name
  n_groups <- nrow(df)
  ## Draw `times` samples from distribution using 
  suppressWarnings({
    expand <- df %>% 
      dplyr::mutate(sample = distributional::generate(  .data[[var]], times) )
  })
  expand %>%
    tidyr::unnest(sample) %>%
    dplyr::mutate(parameter = as.character( rep(1:times, n_groups))) %>%
    dplyr::select(datetime, site_id, parameter,
                  {{var}} := sample, model_id = .model) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(family = "ensemble") %>% 
    tidyr::pivot_longer(-dplyr::starts_with(standard_vars),
                        names_to = "variable", values_to = "prediction")
}

drop_degenerate <- function(df) { 
  var <- attributes(df)$dist
  family <- family(df[[var]])
  if("degenerate" %in% family) {
    warning("dropping degenerate distributions")
    df <- df %>% dplyr::filter(family != "degenerate") 
    
    family <- family(df[[var]])
  }
  df
}
