#' Transform a time series object to be uniseasonal
#' 
#' \code{force_to_uniseasonal_ts_object} is a function to transform a time 
#' series object into a uniseasonal time series object. This is needed to run 
#' i.a. the linear forecasting functions, since they require a uniseasonal time 
#' series object as an input.
#' 
#' @param ts_object A time series object that can be uniseasonal or
#'   multiseasonal.
#'   
#' @return A uniseasonal time series object. If the original ts_object was
#'   multiseasonal, then the highest seasonality is selected.
#'   
#' @importFrom magrittr '%>%'
#' @importFrom tstools transform_data_to_ts_object
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
#' force_to_uniseasonal_ts_object(ts_object = ts_object)
force_to_uniseasonal_ts_object <- function(ts_object) {
  # If not ts object, then stop running
  if (!is.ts(ts_object)) {
    message <- paste0("The input should be a time series object of class ts or msts, instead of class(es) ", paste0(class(ts_object), collapse = "/")," ... ")
    stop(message)
  }
  # If uniseasonal, return ts_object as is
  if (!("msts" %in% class(ts_object))) {
    return(ts_object)
  }
  # If MSTS, force it into a uniseasonal object, choosing first seasonality
  if ("msts" %in% class(ts_object)) {
    # Get first seasonality
    single_seasonality <- ts_object %>% 
      attr("seasonality") %>%
      head(1)
    # Remove msts attribute
    attr(ts_object, "msts") <- NULL
    # Create new ts object to return
    ts_object %>% 
      change_ts_object_seasonality(
        seasonality = single_seasonality
      ) %>% 
      return()
  }
}