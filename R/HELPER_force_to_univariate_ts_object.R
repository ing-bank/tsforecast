#' Transform a time series object to be univariate
#'
#' \code{force_to_univariate_ts_object} is a function to transform a time series
#' object into a univariate time series object. This is needed to run i.a. the
#' linear forecasting functions, since they require a univariate time series
#' object as an input. The external regressors are dropped so that only the
#' column of interest remains in the resulting univariate time series object.
#'
#' @param ts_object A time series object that can be univariate or multivariate.
#'
#' @return A univariate time series object.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools period_to_first_day transform_data_to_ts_object
#' @import dplyr
#'
#' @examples
#' ts_object <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
#' force_to_univariate_ts_object(ts_object = ts_object)
force_to_univariate_ts_object <- function(ts_object) {
  # If not ts object, then stop running
  if (!is.ts(ts_object)) {
    message <- paste0("The input should be a time series object of class ts or msts, instead of class(es) ", paste0(class(ts_object), collapse = "/")," ... ")
    stop(message)
  }
  # If univariate, return ts_object as is
  if (ncol(ts_object) == 1) {
    return(ts_object)
  }
  # If multivariate, force it into a univariate object
  if (ncol(ts_object) > 1) {
    # Extract data
    ts_object %>% 
      ts_object_to_tibble() %>% 
      dplyr::select(period, col_of_interest) %>% 
      dplyr::mutate(
        period = tstools::period_to_first_day(period),
        grouping = attr(ts_object, "grouping")
      ) %>% 
      tstools::transform_data_to_ts_object(seasonal_periods = attr(ts_object, "seasonality")) %>% 
      return()
  }
}