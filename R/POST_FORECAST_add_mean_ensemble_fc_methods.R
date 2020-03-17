#' Adds an mean ensemble of provided forecast methods.
#'
#' \code{add_mean_ensemble_fc_methods} is a function to add a new forecast
#' method after other forecast methods have been added to the
#' main_forecast_table. It takes the average value of the given forecast methods
#' as the forecast value.
#'
#' @param main_forecasting_table A tibble containing a single row and several
#'   columns of data required for time series forecasting, which has been
#'   created using the \code{create_main_forecasting_table} function and which
#'   has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function.
#' @param fc_models A character vector specifying which forecast models to use
#'   in the ensemble.
#' @param fc_model_name A character string specifying the name of the mean
#'   ensamble method that is added to the main_forecasting_table.
#'
#' @return The main_forecasting_table with a mean ensemble method added.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools check_data_format
#' @import dplyr
#'
#' @examples
#' tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table() %>%
#'    head(1) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("linear")
#'    ) %>%
#'    add_mean_ensemble_fc_methods(
#'       fc_models = c("fc_linear_trend", "fc_linear_trend_seasonal")
#'    )
add_mean_ensemble_fc_methods <- function(main_forecasting_table, fc_models, fc_model_name = "fc_mean_ensemble"){
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "add_mean_ensemble_fc_methods",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"
    )
  )
  # Check that all supplied fc_models are available in the main_forecasting_table
  unavailable_fc_models <- fc_models[!fc_models %in% names(main_forecasting_table$fc_models[[1]])]
  if (length(unavailable_fc_models) > 0) {
    message <- paste0("The following specified fc_models are not available:\n", paste0("\t", unavailable_fc_models, collapse = "\n"))
    stop(message)
  }
  # Check fc_model_name
  if (nchar(fc_model_name) == 0 | !is.character(fc_model_name)) stop("The specified 'fc_model_name' should be a non-empty character string ... \n")
  
  # Combine forecasts in both fc_errors and fc_data
  for (i in 1:nrow(main_forecasting_table)) {
    fc_errors <- main_forecasting_table$fc_errors[[i]]
    ensemble_data <- fc_errors %>% 
      dplyr::filter(fc_model %in% fc_models) %>% 
      dplyr::select(grouping, fc_date, period, fc_periods_ahead, actual, fc_value) %>% 
      dplyr::group_by(grouping, fc_date, period, fc_periods_ahead) %>% 
      dplyr::summarise(
        fc_value = mean(fc_value),
        actual = mean(actual)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        fc_model = fc_model_name,
        fc_error = fc_value - actual
      )
    fc_errors <- dplyr::bind_rows(fc_errors, ensemble_data)
    main_forecasting_table$fc_errors[[i]] <- fc_errors
    fc_data <- list(fc_data = ensemble_data %>% dplyr::select(fc_date, period, fc_value))
    main_forecasting_table$fc_models[[i]][model_name] <- fc_data
  }
  # Return results
  return(main_forecasting_table)
}