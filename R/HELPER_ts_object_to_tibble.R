#' Converts time series object into tibble
#'
#' \code{ts_object_to_tibble} is a function to convert a time series object into
#' a tibble with a period column in yyyymm numeric format and a column of
#' interest for the data in the time series object. If applicable, also the
#' external regressors in the time series object are converted to individual
#' columns.
#'
#' @param ts_object A time series object.
#'
#' @return A tibble object with a period column in yyyymm numeric format and or
#'   more other columns corresponding to the data in the time series object.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @import tibble
#' @importFrom tstools transform_data_to_ts_object
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
#'    tstools::transform_data_to_ts_object(seasonal_periods = 3)
#' ts_object_to_tibble(ts_object = ts_object)
ts_object_to_tibble <- function(ts_object) {
  dplyr::bind_cols(
      tibble::tibble(period = ts_object_to_periods(ts_object)),
      tibble::as_tibble(ts_object)
    ) %>% 
    return()
}