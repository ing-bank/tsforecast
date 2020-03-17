#' Add hierarchical forecast values to main forecasting table
#'
#' \code{add_hierarchical_fc_models_to_main_forecasting_table} is a wrapper
#' function, that combines previously computed forecasts according to their
#' hierarchy and adds them to the main forecasting table.
#'
#' @param main_forecasting_table A tibble object that contains run forecasts for
#'   all split dates and all groups. It is assumed that this is the output of
#'   the \code{add_fc_models_to_main_forecasting_table} or
#'   \code{update_main_forecasting_table} function. Data needs to be
#'   hierarchical (see example).
#' @return The main_forecasting_table (input) with the addition of the
#'   consistent and bottom up hierarchical forecast models (within the fc_model
#'   column), and their errors (within the fc_errors column.)
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @importFrom tstools check_data_format split_grouping_column
#' @import dplyr
#'
#' @examples
#' dummy_hierarchical_gasprice %>%
#'    tstools::initialize_ts_forecast_data(
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = "currency",
#'       hierarchical_cols = c("location", "oil_company")
#'    ) %>%
#'    dplyr::filter(period >= as.Date("2004-10-31")) %>%
#'    create_main_forecasting_table() %>%
#'    add_fc_models_to_main_forecasting_table(
#'       fc_methods = c("basic", "linear")
#'    ) %>%
#'    add_hierarchical_fc_models_to_main_forecasting_table()
add_hierarchical_fc_models_to_main_forecasting_table <- function(main_forecasting_table) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "add_hierarchical_fc_models_to_main_forecasting_table",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "hierarchy", "fc_models", "fc_errors"
    )
  )
  # Check if consistent and bottom_up fc models exist in main_forecasting_table
  for (i in 1:nrow(main_forecasting_table)) {
    # Filter consistent and bottom_up models from models to keep
    models_to_keep <- names(main_forecasting_table$fc_models[[i]])
    models_to_keep <- models_to_keep[!grepl("consistent", models_to_keep)]
    models_to_keep <- models_to_keep[!grepl("bottom_up", models_to_keep)]
    # Remove from fc_model column
    main_forecasting_table$fc_models[[i]] <- main_forecasting_table$fc_models[[i]][models_to_keep]
    # Remove from fc_errors column
    main_forecasting_table$fc_errors[[i]] <- main_forecasting_table$fc_errors[[i]] %>% 
      dplyr::filter(fc_model %in% models_to_keep)
  }

  # If hierarchy_matrix is unique (which it should be), extract it
  hierarchy_matrix <- main_forecasting_table$hierarchy[[1]]$matrix
  # Extract hierarchy_group_identifier
  hierarchical_cols <- comment(hierarchy_matrix)
  # Extract all information about non-hierarchical groups
  all_other_groups <- main_forecasting_table %>% 
    dplyr::select(grouping) %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(-grouping, -hierarchical_cols) %>% 
    dplyr::distinct()
    
  # Create empty tibble to store all loop run results
  final_forecasting_table <- tibble::tibble()
  # robust solution
  for (i in 1:nrow(all_other_groups)) {
    # Extract the relevant row from all_other_groups
    group_combination <- all_other_groups[i,]
    # Filter main forecasting table for this group combination
    if (ncol(group_combination) > 0) {
      sub_forecasting_table <- main_forecasting_table %>% 
        tstools::split_grouping_column() %>%
        dplyr::left_join(
          x = group_combination,
          y = .,
          by = colnames(group_combination)
        ) %>% 
        # Revert back to original column selection
        dplyr::select(colnames(main_forecasting_table))
    } else {
      sub_forecasting_table <- main_forecasting_table
    }
    # Get best model per group, excluding incomplete forecast methods
    best_model_per_group <- sub_forecasting_table %>% 
      get_forecast_accuracy_overview(metric = "MASE") %>% 
      get_best_forecast_methods(filter_incomplete_fc = T)
    # Add in consistent and bottom-up fc values
    sub_forecasting_table <- add_fc_values_all_split_dates(
      main_forecasting_table = sub_forecasting_table, 
      model_types = c("consistent", "bottom-up"), 
      best_model_per_group = best_model_per_group
    )
    # Put into final_forecasting_table
    final_forecasting_table <- final_forecasting_table %>% 
      dplyr::bind_rows(sub_forecasting_table)
  }
  # Compute fc_errors and return
  final_forecasting_table %>% 
    add_fc_errors_to_main_forecasting_table() %>%
    return()
}