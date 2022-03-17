#' @importFrom read4cast read_forecast
#' @export
read_forecast <- read4cast::read_forecast

#' @importFrom score4cast score
#' @export
score <- score4cast::score

#' @importFrom score4cast crps_logs_score
#' @export
crps_logs_score <- score4cast::crps_logs_score

#' @importFrom score4cast include_horizon
#' @export
include_horizon <- score4cast::include_horizon

#' @importFrom score4cast pivot_forecast
#' @export
pivot_forecast <- score4cast::pivot_forecast

#' @importFrom score4cast pivot_target
#' @export
pivot_target <- score4cast::pivot_target