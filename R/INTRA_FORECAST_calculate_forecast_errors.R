#' Calculate forecast errors for fc_models.
#'
#' \code{calculate_forecast_errors} is a function to combine the fc_data from
#' each forecast model within the named list fc_models and combine it with
#' actuals data from the validation time series object, in order to calculate
#' the forecast errors.
#'
#' @param fc_models A named list of forecast models, with for each forecast
#'   model a list with the model itself and a table with forecast values.
#' @param ts_object_train A time series object, which contains only the training
#'   data.
#' @param ts_object_valid A time series object, which contains only the
#'   validation data.
#' @param allow_negative_fc Boolean, which is to be set to TRUE if negative
#'   forecast values are allowed, or set to FALSE if negative forecast values
#'   should be overwritten by a value of zero.
#'
#' @return A tibble with data on the fc errors for all the forecast models in
#'   fc_models.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools months_between_periods transform_data_to_ts_object
#'   unlist_if_required
#' @import tibble
#' @import dplyr
#'
#' @examples
#' ts_object_both <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    tstools::transform_data_to_ts_object() %>%
#'    split_ts_object(ts_split_date = 200301)
#' calculate_forecast_errors(
#'    fc_models = add_all_univariate_forecast_models(
#'       ts_object_train = ts_object_both$train,
#'       periods_ahead = 12,
#'       fc_methods = c("basic", "linear", "prophet")
#'    ),
#'    ts_object_train = ts_object_both$train,
#'    ts_object_valid = ts_object_both$valid
#' )
calculate_forecast_errors <- function(fc_models, ts_object_train, ts_object_valid, allow_negative_fc = F) {
  # Check to make sure fc_models is a list
  if (!is.list(fc_models) | is.data.frame(fc_models)) stop(paste0("Object 'fc_models' is of class ",paste0(class(fc_models), collapse = "/")," ... \n\n Put in a list!"))
  # Unlist ts_objects if required
  ts_object_train <- tstools::unlist_if_required(ts_object_train)
  ts_object_valid <- tstools::unlist_if_required(ts_object_valid)
  # Check to make sure ts_object_valid is a times series object
  if (!is.ts(ts_object_train)) stop(paste0("Object 'ts_object_train' is of class ",paste0(class(ts_object_train), collapse = "/")," ... \n\n Put in a time series object!"))
  # Check if ts_object_valid is empty
  if (is.list(ts_object_valid) & length(ts_object_valid) == 0) {
    # Create empty tibble
    actuals <- tibble::tibble(
      period = numeric(), 
      actual = numeric()
    )
  } else {
    # Check to make sure ts_object_valid is a times series object
    if (!is.ts(ts_object_valid)) stop(paste0("Object 'ts_object_valid' is of class ",paste0(class(ts_object_valid), collapse = "/")," ... \n\n Put in a time series object!"))
    # Extract actuals
    actuals <- ts_object_to_tibble(ts_object_valid) %>% 
      dplyr::select(period, actual = col_of_interest)
  }
  
  # Extract grouping information
  grouping <- attr(ts_object_valid, "grouping")
  # Create empty table to add fc_data to
  fc_data <- tibble::tibble()
  # Add fc_data from each model
  for (fc_model in names(fc_models)) {
    if (!'fc_data' %in% names(fc_models[[fc_model]])) stop(paste0("There is no 'fc_data' available in fc_models for the fc_model '",fc_model,"'"))
    fc_data <- fc_models[[fc_model]][['fc_data']] %>% 
      dplyr::mutate(
        fc_value = as.numeric(fc_value),
        fc_model = fc_model,
        grouping = grouping
      ) %>% 
      dplyr::bind_rows(fc_data,.)
  }
  # Return empty if there is not fc_data available
  if (nrow(fc_data) == 0) {
    return(tibble::tibble())
  }
  # Calculate fc_periods_ahead once per fc_date and period combination
  fc_periods_ahead <- fc_data %>% 
    dplyr::select(fc_date, period) %>% 
    dplyr::distinct(.keep_all = T) %>% 
    dplyr::mutate(fc_periods_ahead = tstools::months_between_periods(fc_date, period))
  
  # Add actuals, fc_periods_ahead and calculate fc_value and fc_error
  fc_errors <- fc_data %>% 
    dplyr::left_join(
      y = actuals,
      by = "period"
    ) %>% 
    dplyr::left_join(
      y = fc_periods_ahead,
      by = c("period", "fc_date")
    ) %>% 
    dplyr::arrange(fc_date, fc_model, period) %>% 
    dplyr::mutate(
      fc_value = dplyr::case_when(
        !allow_negative_fc & fc_value < 0 ~ 0,
        TRUE ~ fc_value
      ),
      fc_error = fc_value - actual
    ) %>% 
    dplyr::group_by(grouping, fc_model, fc_date) %>% 
    dplyr::mutate(
      MASE = calculate_MASE(
        ts_object_train = ts_object_train,
        fc_error = fc_error
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(grouping, fc_model, fc_date, period, fc_periods_ahead, fc_value, actual, fc_error, MASE)
  # Return results
  return(fc_errors)
}