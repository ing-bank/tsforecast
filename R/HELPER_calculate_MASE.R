#' Calculate MASE values
#'
#' \code{calculate_MASE} is a function to calculate the Mean Absolute Scaled
#' Error (MASE) values for a specific forecast with a given horizon. To do this,
#' the time series object that was used for training is reused to create a
#' seasonal naive forecast, which is then used as a base line in the comparison
#' with the specified set of fc_errors. This results in a set of scaled errors
#' (the MASE values), which were proposed by Hyndman & Koehler (2006) as an
#' alternative to using percentage errors when comparing forecast accuracy
#' across series with different units. For more information, check out [this
#' page](https://otexts.com/fpp2/accuracy.html).
#'
#' @param ts_object_train A time series object, which contains only the training
#'   data.
#' @param fc_error A numeric vector, which corresponds to the forecast error
#'   values for which the MASE values need to be calculated.
#'
#' @return A numeric vector of MASE values
#' 
#' @importFrom tstools unlist_if_required
#'
#' @examples
#' main_forecasting_table <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table() %>%
#'    head(1) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("linear")
#'    )
#' calculate_MASE(
#'    ts_object_train = main_forecasting_table$ts_object_train[[1]],
#'    fc_error = main_forecasting_table$fc_errors[[1]] %>% 
#'       dplyr::filter(fc_model == "fc_linear_trend_seasonal") %>% 
#'       dplyr::pull(fc_error)
#' )
calculate_MASE <- function(ts_object_train, fc_error){
  # Unlist ts_object_train if required
  ts_object_train <- ts_object_train %>% 
    tstools::unlist_if_required()
  # Check parameters
  if (!is.ts(ts_object_train)) {
    message <- paste0("Object 'ts_object_train' is of class ",paste0(class(ts_object_train), collapse = "/")," ... \n\n Put in a time series object!")
    stop(message)
  }
  if (!is.numeric(fc_error)) {
    message <- paste0("Object 'fc_error' is of class ",paste0(class(fc_error), collapse = "/")," ... \n\n Put in a numeric vector of forecast error values!")
    stop(message)
  }
  # Determine frequency of the time series
  freq <- frequency(ts_object_train)
  # Create an empty vector for the first full season
  forecastsNaiveSD <- rep(NA, freq)
  # Loop over the next seasons to apply seasonal naive forecasting
  for (j in (freq + 1):length(ts_object_train)) {
    forecastsNaiveSD <- c(forecastsNaiveSD, ts_object_train[j - freq])
  }
  # Calculate Mean Absolute Scaled Error (MASE)
  dividend <- abs(fc_error)
  divisor <- mean(abs(ts_object_train - forecastsNaiveSD), na.rm = TRUE)
  MASE <- (dividend / divisor)
  # For dividend == divisor, make MASE = 1
  MASE[which(dividend == divisor)] <- 1
  return(MASE)
}