#' Decompose time series object
#'
#' \code{decompose_ts_object_for_ML} Add columns to a time series object that
#' contains features of the time variable, including a periods column. This is
#' in order to prepare the dataset for models that are not suited to handle time
#' series objects like CART trees or RandomForest forests.
#'
#' @param ts_object A time series object, with the column of interest. It can
#'   also contain external regressor columns.
#' @param filter_stationary Boolean, indicating whether to filter out stationary
#'   features of the decomposed time series. For instance, if data contains
#'   monthly data, then the "minute" feature will be the same across all
#'   periods. Setting \code{filter_stationary} to TRUE will eliminate all
#'   columns which have the same value in all rows.
#' @param filter_date_features Boolean, indicating whether to filter out any
#'   column that contains a date feature (TRUE) or not (FALSE). This can be used
#'   for forecast methods which are not able to handle date objects as features.
#' @param add_xreg_deltas Boolean, indicating whether to add as additional
#'   column(s) the deltas between consecutive rows for the external regressor
#'   column(s) (TRUE) or not (FALSE).
#'
#' @return A tibble that contains the original variable(s) along with date
#'   featurs as additional columns
#'
#' @importFrom magrittr '%>%'
#' @importFrom timetk tk_augment_timeseries_signature
#' @importFrom lubridate is.Date
#' @importFrom purrr map
#' @import dplyr
#' @importFrom tstools period_to_last_day transform_data_to_ts_object
#'
#' @examples
#' tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'   ) %>%
#'   dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'   tstools::transform_data_to_ts_object() %>%
#'   decompose_ts_object_for_ML()
decompose_ts_object_for_ML <- function(ts_object, filter_stationary = T, filter_date_features = F, add_xreg_deltas = T) {
  # Check ts_object
  if (!any(class(ts_object) == "ts")) {
    message <- paste0("The specified ts_object should be time series object format, instead of: '", paste0(class(ts_object), collapse = "/"),"'")
    stop(message)
  }
  # Convert ts_object to data
  data <- ts_object %>% 
    ts_object_to_tibble()
  # Check data
  if (!"col_of_interest" %in% colnames(data)) stop(paste0("The required column 'col_of_interest' is not available in the data, are you sure you used the 'tstools::initialize_ts_forecast_data()' function to create it?\n"))
  if (!"period" %in% colnames(data)) stop(paste0("The required column 'period' is not available in the data, are you sure you used the 'tstools::initialize_ts_forecast_data()' function to create it?\n"))
  if (!is.numeric(data$period)) stop(paste0("The period column is not of numeric class, but of '",paste0(class(data$period), collapse = "/"),"' ... \n"))
  # Decompose time series
  decomp_ts <- data %>% 
    dplyr::mutate(
      period = tstools::period_to_last_day(period)
    ) %>% 
    timetk::tk_augment_timeseries_signature()
  # Remove stationary columns if required
  if (filter_stationary) {
    decomp_ts <- decomp_ts %>% 
      filter_stationary_columns()
  }
  # Remove Date columns if required
  if (filter_date_features) {
    cols_to_delete <- decomp_ts %>% 
      dplyr::select_if(lubridate::is.Date) %>% 
      colnames()
    cols_to_keep <- colnames(decomp_ts)[!colnames(decomp_ts) %in% cols_to_delete]
    decomp_ts <- decomp_ts %>%
      dplyr::select(cols_to_keep)
  }
  # Add xreg_cols deltas if required
  if (add_xreg_deltas) {
    # Determine length of the lags
    lags <- 1:max(attr(ts_object, "seasonality"))
    # Loop over each xreg column
    for (xreg_col in attr(ts_object, "xreg_cols")) {
      # Create dplyr::quosures for all lags
      add_all_lags <- setNames(
        object = purrr::map(lags, ~dplyr::quo(dplyr::lag(!!dplyr::sym(xreg_col), n = !!.x))),
        nm = paste0(xreg_col, "_delta", lags)
      )
      # Add each lag
      decomp_ts <- decomp_ts %>% 
        dplyr::mutate(!!! add_all_lags)
    }
  }
  
  # Return
  return(decomp_ts)
}