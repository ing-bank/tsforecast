#' Add computed hierarchical forecast values, per split date
#'
#' \code{add_fc_values_per_split_date} is a function that adds previously
#' computed hierarchical forecasts into the main forecasting table, for a single
#' split date.
#'
#' @param main_forecasting_table A tibble object that contains run forecasts for
#'   one split date and all groups. It is assumed that this is the output of the
#'   \code{add_fc_models_to_main_forecasting_table} or
#'   \code{update_main_forecasting_table} function.
#' @param grouped_hierarchical_fc_values A tibble that containts the best
#'   fc_model for each group. It is assumed that this tibble is created using
#'   the \code{get_forecast_accuracy_overview} and
#'   \code{get_best_forecast_methods} functions (see example).
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
#'   dplyr::filter(ts_split_date == 200605) %>%
#'   add_fc_models_to_main_forecasting_table(
#'     fc_methods = c("basic", "linear")
#'   )
#' best_model_per_group <- main_forecasting_table %>%
#'   get_forecast_accuracy_overview() %>%
#'   get_best_forecast_methods()
#' grouped_hierarchical_fc_values <- get_consistent_fc_values(
#'   main_forecasting_table = main_forecasting_table,
#'   best_model_per_group = best_model_per_group
#' )
#' add_fc_values_per_split_date(
#'   main_forecasting_table = main_forecasting_table,
#'   grouped_hierarchical_fc_values = grouped_hierarchical_fc_values
#' )
add_fc_values_per_split_date <- function(main_forecasting_table, grouped_hierarchical_fc_values) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "add_fc_values_per_split_date",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_models", "hierarchy"
    )
  )
  # Check grouped_hierarchical_fc_values
  tstools::check_data_format(
    data = grouped_hierarchical_fc_values,
    func_name = "add_fc_values_per_split_date",
    req_cols = c("fc_date", "period", "fc_value", "grouping", "best_model")
  )

  # Loop over each grouping
  for (group in unique(grouped_hierarchical_fc_values$grouping)) {
    # Filter group data
    fc_data <- grouped_hierarchical_fc_values %>% 
      dplyr::filter(grouping == group)
    # Get split data
    split_date <- fc_data %>% 
      dplyr::pull(fc_date) %>% 
      unique()
    # Get model name
    fc_name <- fc_data %>% 
      dplyr::pull(best_model) %>% 
      unique()
    # Cleanup fc_data
    fc_data <- fc_data %>% 
      dplyr::select(fc_date, period, fc_value)
    # Combine data for new forecast model
    fc_model <- list(
      model = fc_name,
      fc_data = fc_data
    )
    # Get index of row to insert values in
    row_index <- which(
      (main_forecasting_table$ts_split_date == split_date) &
      (main_forecasting_table$grouping == group)
    )
    # Insert fc_model data into this row
    main_forecasting_table$fc_models[[row_index]][fc_name] <- list(fc_model)
  }
  
  # Return results
  return(main_forecasting_table)
}