#' Determine forecasting periods for ts object
#'
#' \code{get_fc_periods} is a function which creates a vector of periods to be
#' forecasted given the training data and intended forecasting periods ahead.
#'
#' @param ts_object_train A time series object, representing the training data
#'   for the forecast.
#'
#' @param periods_ahead A positive integer value indicating the number of
#'   periods ahead to be forecasted (e.g. 24 for 2 years ahead forecasting using
#'   monthly data).
#'
#' @return A vector of periods in numeric yyyymm format, of length periods_ahead
#'   and starting after the periods available in ts_object_train.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools date_to_period period_to_first_day
#'   transform_data_to_ts_object
#'
#' @examples
#'   ts_object_train <- tstools::initialize_ts_forecast_data(
#'         data = dummy_gasprice,
#'         date_col = "year_month",
#'         col_of_interest = "gasprice",
#'         group_cols = c("state", "oil_company"),
#'         xreg_cols = c("spotprice", "gemprice")
#'      ) %>%
#'      dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'      tstools::transform_data_to_ts_object(seasonal_periods = 3)
#'   get_fc_periods(ts_object_train = ts_object_train, periods_ahead = 10)
get_fc_periods <- function(ts_object_train, periods_ahead) {
  # Check whether data is time series or not
  if (!is.ts(ts_object_train)) {
    message <- paste0("The parameter 'ts_object_train' should be a time series object of class ts or msts, instead of class(es) ", paste0(class(ts_object_train), collapse = "/")," ... ")
    stop(message)
  }
  # Get ts frequency, if not in months then stop
  ts_frequency <- max(frequency(ts_object_train))
  if (ts_frequency != 12) {
    message <- paste0("The frequency of the time series object specified for 'ts_object_train' is ",ts_frequency," instead of the required frequency of 12 for monthly data ... ")
    stop(message)
  }
  # Check to make sure periods_ahead is a non-negative whole number
  if (!(is.numeric(periods_ahead) & periods_ahead > 0 & periods_ahead == suppressWarnings(as.integer(periods_ahead)))) {
    message <- paste0("The parameter 'periods_ahead' should be a positive integer value, instead of '",periods_ahead,"' ... ")
    stop(message)
  }
  # Get the existing date column into YYYY-MM-DD first day form 
  ts_dates <- ts_object_train %>% 
    ts_object_to_periods() %>% 
    tstools::period_to_first_day()
  # Create vector of period given periods_ahead
  seq.Date(
      from = max(ts_dates) + months(1),
      to = max(ts_dates) + months(periods_ahead),
      by = "month"
    ) %>% 
    tstools::date_to_period() %>% 
    return()
}