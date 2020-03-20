#' Calculate consistent hierarchical forecast values
#'
#' \code{get_consistent_fc_values} is a function that calculates the consistent
#' hierarchical forecasting values, based on already calculated forecasts.
#'
#' @param main_forecasting_table A tibble object that contains run forecasts for
#'   one split date and all groups. It is assumed that this is the output of the
#'   \code{add_fc_models_to_main_forecasting_table} or
#'   \code{update_main_forecasting_table} function.
#' @param best_model_per_group A tibble that containts the best fc_model for
#'   each group. It is assumed that this tibble is created using the
#'   \code{get_forecast_accuracy_overview} and \code{get_best_forecast_methods}
#'   functions (see example).
#' @return A tibble with the consistent hierarchical forecast value for all
#'   groups in all forecast dates, at the specified forecast split date
#'
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @importFrom tstools add_grouping_column check_data_format
#'   split_grouping_column unlist_if_required
#' @import dplyr
#'
#' @examples
#' main_forecasting_table <- dummy_hierarchical_gasprice %>%
#'    dplyr::filter(oil_company == "CompanyA") %>%
#'    tstools::initialize_ts_forecast_data(
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = "currency",
#'       hierarchical_col = "location"
#'    ) %>%
#'    dplyr::filter(period >= as.Date("2004-06-30")) %>%
#'    create_main_forecasting_table() %>%
#'    dplyr::filter(ts_split_date == 200606) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       fc_methods = c("basic", "linear")
#'    )
#' best_model_per_group <- main_forecasting_table %>%
#'    get_forecast_accuracy_overview() %>%
#'    get_best_forecast_methods()
#' get_consistent_fc_values(
#'    main_forecasting_table = main_forecasting_table,
#'    best_model_per_group = best_model_per_group
#' )
get_consistent_fc_values <- function(main_forecasting_table, best_model_per_group) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "get_consistent_fc_values",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_models", "hierarchy"
    ),
    unique_value_cols = "ts_split_date"
  )
  # Check best_model_per_group
  tstools::check_data_format(
    data = best_model_per_group,
    func_name = "get_consistent_fc_values",
    req_cols = c("fc_model", "grouping")
  )

  # If hierarchy_matrix is unique (which it should be), extract it
  hierarchy_matrix <- main_forecasting_table$hierarchy[[1]]$matrix
  if (!is.matrix(hierarchy_matrix)) {
    stop("The hierarchy_matrix must be a matrix!")
  }
  # Extract hierarchical_cols
  hierarchical_cols <- comment(hierarchy_matrix)
  
  # Initialize data frame for new fc_data
  data <- tibble::tibble()
  # Loop over each group to add it to data
  for (group in unique(main_forecasting_table$grouping)) {
    # Filter that group
    group_table <- main_forecasting_table %>% 
      dplyr::filter(grouping == group)
    # Get the best model for that group
    best_model <- best_model_per_group %>% 
      dplyr::filter(grouping == group) %>% 
      dplyr::pull(fc_model)
    # Get fc_errors table
    fc_errors <- group_table %>% 
      dplyr::pull(fc_errors) %>% 
      tstools::unlist_if_required()
    # Adjust data for best model and add to data
    data <- group_table$fc_models[[1]][[best_model]][["fc_data"]] %>% 
      dplyr::mutate(
        grouping = group,
        best_model = best_model,
        # Put in inverse of MASE
        MASE = fc_errors %>% 
          dplyr::filter(fc_model == best_model) %>% 
          dplyr::pull(MASE)
      ) %>% 
      dplyr::mutate(
        MASE = case_when(
          is.na(MASE) ~ 1,
          TRUE ~ 1/MASE
        )
      ) %>% 
      dplyr::bind_rows(data, .)
  }
  
  # Get non-hierarchical grouping columns
  group_cols <- data %>% 
    dplyr::select(grouping) %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(-grouping) %>% 
    colnames()
  non_hierarchical_cols <- group_cols[!group_cols %in% hierarchical_cols]
  
  # Arrange data in the right order (all groups)
  data <- data %>%  
    tstools::split_grouping_column() %>% 
    dplyr::rename(total_grouping = grouping) %>% 
    tstools::add_grouping_column(group_cols = hierarchical_cols) %>% 
    dplyr::arrange(fc_date, period, match(grouping, rownames(hierarchy_matrix))) %>% 
    dplyr::select(fc_date, period, fc_value, grouping = total_grouping, best_model, MASE)
  
  # OLS solver: https://cran.r-project.org/web/packages/hts/vignettes/hts.pdf#page=6
  # y_tilde = S (S' S)^-1 S' y_hat = S beta_hat y_hat
  # Revise forecasts to create consistent forecast
  data <- data %>%  
    tstools::split_grouping_column() %>% 
    dplyr::group_by(fc_date, period, !!! dplyr::syms(non_hierarchical_cols)) %>% 
    dplyr::mutate(
      # WLS estimator
      fc_value = hierarchy_matrix %*% 
        solve(t(hierarchy_matrix) %*% diag(MASE) %*% hierarchy_matrix) %*% 
        t(hierarchy_matrix) %*% diag(MASE) %*% fc_value # y_tilde = S beta_hat y_hat
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::select(fc_date, period, fc_value, grouping, best_model)

  # Rename best_model observations and return
  data %>% 
    dplyr::mutate(best_model = paste0(best_model, "_consistent")) %>% 
    return()
}