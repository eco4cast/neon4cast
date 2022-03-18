#' @importFrom read4cast read_forecast
#' @export
read4cast::read_forecast

#' @importFrom score4cast score
#' @export
score4cast::score

#' @importFrom score4cast crps_logs_score
#' @export
score4cast::crps_logs_score

#' @importFrom score4cast include_horizon
#' @export
score4cast::include_horizon

#' @importFrom score4cast pivot_forecast
#' @export
score4cast::pivot_forecast

#' @importFrom score4cast pivot_target
#' @export
score4cast::pivot_target

# @importFrom dplyr `%>%`

#globalVariables("forecast_start_time", "horizon", "observed",
#                "predicted","statistic", "target_url", "theme")