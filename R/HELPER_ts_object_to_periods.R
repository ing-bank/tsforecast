#' Gets available periods from time series object
#' 
#' \code{ts_object_to_periods} is a function to transform a given time series 
#' object into a vector of periods in yyyymm numeric format.
#' 
#' @param ts_object A time series object.
#'   
#' @return A vector of periods in yyyymm numeric format.
#'   
#' @importFrom magrittr '%>%'
#' @importFrom zoo as.yearmon
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
#' ts_object_to_periods(ts_object)
ts_object_to_periods <- function(ts_object) {
  if (!is.ts(ts_object)) {
    message <- paste0("Object is of class ",paste0(class(ts_object), collapse = "/")," ... \n\n Put in a time series object!")
    stop(message)
  }
  # This is ugly, but its fast
  dates <- ts_object %>% 
    time() %>% 
    as.numeric()
  years <- floor(dates + 0.001)
  months <- ((dates - years) * 12) + 1
  periods <- as.numeric(years * 100 + months)
  return(periods)
}