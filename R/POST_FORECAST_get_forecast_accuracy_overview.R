#' Get forecast accuracy overview.
#'
#' \code{get_forecast_accuracy_overview} is a function to create an overview of
#' the accuracy of the different forecast models in the main forecasting table.
#' The function combines the fc_errors data from all rows within the
#' main_forecasting_table to calculate the overall accuracy of each forecast
#' model for each number of periods ahead that has been forecasted.
#'
#' @param main_forecasting_table A tibble containing several columns of data
#'   required for time series forecasting, which has been created using the
#'   \code{create_main_forecasting_table} function and which has been extended
#'   with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function.
#' @param sort_by Should the fc models be sored by the mean error values
#'   ("mean"), the median error values ("median"), the minimum error values
#'   ("min") or the maximum error values ("max").
#' @param abs_error Boolean, which is to be set to TRUE if the absolute error
#'   values should be used to calculate the forecast accuracy, or set to FALSE
#'   if the error values as is should be used to calculate the forecast accuracy
#'   (which has the problem that positive and negative errors can cancel each
#'   other out, leading to non-intuitive conclusions about forecast accuracy).
#'
#' @return A tibble containing an overview of the overall forecast accuracy of
#'   each forecast model in the main_forecasting_table.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools check_data_format
#' @import dplyr
#'
#' @examples
#' data <- tstools::initialize_ts_forecast_data(
#'    data = dummy_gasprice,
#'    date_col = "year_month",
#'    col_of_interest = "gasprice",
#'    group_cols = c("state", "oil_company")
#' )
#' main_forecasting_table <- create_main_forecasting_table(
#'       data = data,
#'       seasonal_periods = c(12,3)
#'    ) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("basic", "linear")
#'    )
#' get_forecast_accuracy_overview(main_forecasting_table)
get_forecast_accuracy_overview <- function(main_forecasting_table, metric = c("MAE", "MAPE", "MASE")) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "get_forecast_accuracy_overview",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_errors"
    )
  )
  # Check sort_by
  metric <- match.arg(metric)
  
        # TEMP CODE FOR BACKWARDS COMPATIBILITY, TO BE REMOVED IN THE FUTURE
        if (!("MASE" %in% colnames(main_forecasting_table$fc_errors[[1]]))) {
          if ("fc_models" %in% colnames(main_forecasting_table)) {
            main_forecasting_table <- main_forecasting_table %>% 
              add_fc_errors_to_main_forecasting_table()
          } else {
            message <- "This new version of the tsforecast package makes use of MASE values within the fc_errors tables to be able to assess forecast accuracy. Your main_forecasting_table does not seem to have the MASE values available within the fc_errors table and also doesn't have the fc_models column available (probably because it was removed to reduce the size of the object) in the main_forecasting_table (which could have been used to run add_fc_errors_to_main_forecasting_table() again so that this time the MASE values would have been added) ..."
            stop(message)
          }
        }
  
  # Extract fc_errors from the main_forecasting_table
  fc_errors <- main_forecasting_table %>% 
      dplyr::pull(fc_errors) %>% 
      dplyr::bind_rows() %>% 
      dplyr::filter(!is.na(fc_error))
  # Add missing metrics
  fc_errors <- fc_errors %>% 
    dplyr::mutate(
      MAE = abs(fc_error),
      MAPE = dplyr::case_when(
        actual != 0 ~ abs(fc_error / actual),
        TRUE ~ 1
      )
    )
  # Prepare fc_errors, summarize and return
  fc_errors %>% 
    dplyr::mutate(
      value = !! dplyr::sym(metric)
    ) %>% 
    dplyr::group_by(grouping, fc_periods_ahead, fc_model) %>% 
    dplyr::summarise(
      n_data_point = length(value),
      MAE = mean(MAE),
      MAPE = mean(MAPE),
      MASE = mean(MASE),
      min = min(value),
      q1 = quantile(value,0.25),
      metric = mean(value),
      q3 = quantile(value,0.75),
      max = max(value),
      sd = dplyr::case_when(
        length(value) > 1 ~ sd(value),
        TRUE ~ 0
      ),
      order = 1
    ) %>% 
    dplyr::arrange(grouping, fc_periods_ahead, abs(metric)) %>% 
    dplyr::mutate(order = cumsum(order)) %>% 
    dplyr::ungroup() %>% 
    return()
}