#' Add fc errors to main forecasting table
#'
#' \code{add_fc_errors_to_main_forecasting_table} is a function to extend the
#' main forecasting table with an additional column, called fc_errors, which
#' contains the forecast values, actuals and resulting forecast errors for every
#' forecast model in the fc_models column. By default, this function is called
#' within the add_fc_models_to_main_forecasting_table function.
#'
#' @param main_forecasting_table A tibble containing several columns of data
#'   required for time series forecasting, which has been created using the
#'   \code{create_main_forecasting_table} function and which has been extended
#'   with the fc_models column using the
#'   \code{add_fc_models_to_main_forecasting_table} function.
#' @param allow_negative_fc Boolean, which is to be set to TRUE if negative
#'   forecast values are allowed, or set to FALSE if negative forecast values
#'   should be overwritten by a value of zero.
#'
#' @return A tibble containing several columns of data required for time series
#'   forecasting, extended with as an additional column, called fc_errors, which
#'   contains the forecast values, actuals and resulting forecast errors.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr pmap
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
#'       fc_methods = c("basic", "linear"),
#'       add_fc_errors = F
#'    ) %>%
#'    add_fc_errors_to_main_forecasting_table()
add_fc_errors_to_main_forecasting_table <- function(main_forecasting_table, allow_negative_fc = FALSE) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "add_fc_errors_to_main_forecasting_table",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_models"
    )
  )
  # Remove previous results if available
  if ("fc_errors" %in% names(main_forecasting_table)) {
    main_forecasting_table <- main_forecasting_table %>% 
      dplyr::select(-fc_errors)
  }
  # Calculate forecast errors, add them and return
  main_forecasting_table %>% 
    dplyr::select(fc_models, ts_object_train, ts_object_valid) %>% 
    dplyr::mutate(
      fc_errors = purrr::pmap(
        .l = list(
          "fc_models" = fc_models,
          "ts_object_train" = ts_object_train,
          "ts_object_valid" = ts_object_valid
        ),
        .f = calculate_forecast_errors,
        allow_negative_fc = allow_negative_fc
      )
    ) %>% 
    dplyr::select(fc_errors) %>% 
    dplyr::bind_cols(main_forecasting_table, .) %>% 
    return()
}