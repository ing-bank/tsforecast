#' Cleans a multivariate time series object
#'
#' \code{cleanup_multivariate_ts_object} is a function to clean a multivariate
#' time series object by removing all unnecessary columns (e.g.
#' original_col_of_interest). This is needed to make sure that no information
#' besides the external regressors is used when training the forecast models.
#'
#' @param ts_object A multivariate time series object
#'
#' @return A cleaned multivariate time series object.
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
#'    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
#'    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
#' cleanup_multivariate_ts_object(ts_object = ts_object)
cleanup_multivariate_ts_object <- function(ts_object) {
  # If not ts object, then stop running
  if (!is.ts(ts_object)) {
    message <- paste0("The input should be a time series object of class ts or msts, instead of class(es) ", paste0(class(ts_object), collapse = "/")," ... ")
    stop(message)
  }
  # Extract information on external regressors
  xreg_cols <- attr(ts_object, "xreg_cols")
  # If already clean, return ts_object as is
  if (length(colnames(ts_object)) == (length(xreg_cols) + 1)) {
    if (all(colnames(ts_object) == c("col_of_interest", xreg_cols))) {
      return(ts_object)
    }
  }
  # Remove unnecessary columns
  ts_object %>% 
    ts_object_to_tibble() %>% 
    dplyr::select(period, col_of_interest, xreg_cols) %>% 
    dplyr::mutate(
      period = tstools::period_to_first_day(period),
      grouping = attr(ts_object, "grouping")
    ) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = attr(ts_object, "seasonality")) %>% 
    return()
}