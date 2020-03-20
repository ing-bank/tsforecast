#' Add computed hierarchical forecast values, for all split dates
#'
#' \code{add_fc_values_all_split_dates} is a wrapper function, that adds previously
#' computed hierarchical forecasts into the main forecasting table.
#'
#' @param main_forecasting_table A tibble object that contains run forecasts for
#'   all split dates and all groups. It is assumed that this is the output of the
#'   \code{add_fc_models_to_main_forecasting_table} or
#'   \code{update_main_forecasting_table} function.
#' @param best_model_per_group A tibble that contains the best fc_model for
#'   each group. It is assumed that this tibble is created using the
#'   \code{get_forecast_accuracy_overview} and \code{get_best_forecast_methods}
#'   functions (see example).
#' @return A tibble with the consistent hierarchical forecast value for all
#'   groups in all forecast dates, at the specified forecast split date
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools check_data_format
#' @import dplyr
#'
#' @examples
#' main_forecasting_table <- dummy_hierarchical_gasprice %>%
#'   dplyr::filter(oil_company == "CompanyA") %>%
#'   tstools::initialize_ts_forecast_data(
#'     date_col = "year_month",
#'     col_of_interest = "gasprice",
#'     group_cols = "currency",
#'     hierarchical_cols = c("location")
#'   ) %>%
#'   dplyr::filter(period >= as.Date("2004-06-30")) %>%
#'   create_main_forecasting_table() %>%
#'   add_fc_models_to_main_forecasting_table(
#'     fc_methods = c("basic", "linear")
#'   )
#' best_model_per_group <- main_forecasting_table %>%
#'   get_forecast_accuracy_overview() %>%
#'   get_best_forecast_methods()
#' add_fc_values_all_split_dates(
#'   main_forecasting_table = main_forecasting_table,
#'   model_types = c("consistent", "bottom-up"),
#'   best_model_per_group = best_model_per_group
#' )
add_fc_values_all_split_dates <- function(main_forecasting_table, model_types = c("consistent", "bottom-up"), best_model_per_group) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "add_fc_values_all_split_dates",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "hierarchy", "fc_models"
    )
  )
  # Check model_types
  invalid_model_types <- model_types[!model_types %in% c("consistent", "bottom-up")]
  if (length(invalid_model_types) > 0) {
    message <- paste0("The following specified model_types are not valid:\n", paste0("\t", invalid_model_types, collapse = "\n"))
    stop(message)
  }
  model_types <- match.arg(model_types, several.ok = T)
  # Check best_model_per_group
  tstools::check_data_format(
    data = best_model_per_group,
	  func_name = "add_fc_values_all_split_dates",
    req_cols = c("fc_model", "grouping")
  )
  
  # Add hierarchical fc values for each split date
  for (split_date in unique(main_forecasting_table$ts_split_date)) {
    # Filter main_forecasting_table for that split date
    main_forecasting_table_split_date <- main_forecasting_table %>% 
      dplyr::filter(ts_split_date == split_date)
    # Add consistent model type if required
    if ("consistent" %in% model_types) {
      grouped_fc_data <- get_consistent_fc_values(
        main_forecasting_table = main_forecasting_table_split_date, 
        best_model_per_group = best_model_per_group
      )
      main_forecasting_table <- add_fc_values_per_split_date(
        main_forecasting_table = main_forecasting_table, 
        grouped_hierarchical_fc_values = grouped_fc_data
      )
    }  
    # Add bottom-up model type if required
    if ("bottom-up" %in% model_types) {
      grouped_fc_data <- get_bottom_up_fc_values(
        main_forecasting_table = main_forecasting_table_split_date,
        best_model_per_group = best_model_per_group
      )
      main_forecasting_table <- add_fc_values_per_split_date(
        main_forecasting_table = main_forecasting_table,
        grouped_hierarchical_fc_values = grouped_fc_data
      )
    }
  }
  
  # Return results
  return(main_forecasting_table)
}