#' Create fc_error table by combination of two groups
#'
#' \code{create_combined_fc_errors_table} is a function to create a tibble, that
#' has the same structure as the fc_errors table. It is formed by combining two
#' groups via one of the four basic operators.
#'
#' @param first_main_forecasting_table A tibble containing a single row and
#'   several columns of data required for time series forecasting, which has
#'   been created using the \code{create_main_forecasting_table} function and
#'   which has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function. This will be used
#'   on the left side of the operation.
#' @param second_main_forecasting_table A tibble containing a single row and
#'   several columns of data required for time series forecasting, which has
#'   been created using the \code{create_main_forecasting_table} function and
#'   which has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function. This will be used
#'   on the right side of the operation.
#' @param first_fc_model A character specifying which forecast model to filter
#'   from first_main_forecasting_table. Only one specified forecast model may be
#'   selected!
#' @param second_fc_model A character specifying which forecast model to filter
#'   from second_main_forecasting_table. Only one specified forecast model may
#'   be selected!
#' @param new_fc_model The name for the fc_model column in the new fc_error
#'   table.
#' @param operator A character that specifies what operation you want to take
#'   place between the two values. It can be either "addition", "subtraction",
#'   "multiplication" or "division".
#' @param group_variable The name of the grouping column where the two
#'   main_forecasting_tables differ
#' @param new_group_name The name of the new group you have created!
#' @return A tibble object, structured almost identical to a typical fc_errors
#'   table. The difference is that this object also has the historical actuals
#'   data
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools add_grouping_column check_data_format
#'   split_grouping_column unlist_if_required
#' @import dplyr
#'
#' @export
#'
#' @examples
#' main_forecasting_table <- dummy_gasprice %>%
#'       tstools::initialize_ts_forecast_data(
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    dplyr::filter(period >= as.Date("2004-01-31")) %>%
#'    dplyr::filter(grouping %in% c(
#'       "state = New York   &   oil_company = CompanyB",
#'       "state = New York   &   oil_company = CompanyA"
#'    )) %>%
#'    create_main_forecasting_table() %>%
#'    add_fc_models_to_main_forecasting_table(
#'       fc_methods = c("basic", "linear", "prophet")
#'    )
#' first_main_forecasting_table <- main_forecasting_table %>%
#'    dplyr::filter(ts_split_date == 200512) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB")
#' second_main_forecasting_table <- main_forecasting_table %>%
#'    dplyr::filter(ts_split_date == 200512) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
#' create_combined_fc_errors_table(
#'    first_main_forecasting_table = first_main_forecasting_table,
#'    second_main_forecasting_table = second_main_forecasting_table,
#'    first_fc_model = "fc_linear_trend",
#'    second_fc_model = "fc_prophet_050cps",
#'    new_fc_model = "fc_division",
#'    operator = "division",
#'    group_variable = "oil_company",
#'    new_group_name = "Company-B over CompanyA"
#' )
create_combined_fc_errors_table <- function(first_main_forecasting_table, second_main_forecasting_table, first_fc_model = "", second_fc_model = "", new_fc_model = "", operator = c("addition", "subtraction", "division", "multiplication"), group_variable = "", new_group_name = "") {
  # Check main_forecasting_tables
  for (main_forecasting_table in list(first_main_forecasting_table, second_main_forecasting_table)) {
    tstools::check_data_format(
      data = main_forecasting_table,
      func_name = "create_combined_fc_errors_table",
      req_cols = c(
        "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
        "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"
      )
    )
  }
  # Check specified fc_models
  if (length(first_fc_model) != 1 || length(second_fc_model) != 1) stop("Specify only one fc_model ... \n")
  if (!first_fc_model %in% names(first_main_forecasting_table$fc_models[[1]])) {
    message <- paste0("The specified first_fc_model is not available in first_main_forecasting_table")
    stop(message)
  }
  if (!second_fc_model %in% names(second_main_forecasting_table$fc_models[[1]])) {
    message <- paste0("The specified second_fc_model is not available in second_main_forecasting_table")
    stop(message)
  }
  if (length(new_fc_model) != 1) stop("Specify only one new_fc_model ... \n")
  # Check that group_variable is withing one of the grouping categories
  cols <- colnames(first_main_forecasting_table)
  first_grouping <- first_main_forecasting_table %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(colnames(.)[!colnames(.) %in% cols])
  second_grouping <- second_main_forecasting_table %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(colnames(.)[!colnames(.) %in% cols])
  if (!all(group_variable %in% colnames(first_grouping), group_variable %in% colnames(second_grouping))) {
    stop("The input group_variable must be one of the grouping colums")
  }
  # Check that column names for the grouping datasets are the same
  if (!all(colnames(first_grouping) == colnames(second_grouping))) {
    stop("The groupings are different, are the two main_forecating_tables from the same main table?")
  }
  # Check that the group_variable has a different observation in both grouping tables
  if (any(first_grouping %>% dplyr::pull(group_variable) == second_grouping %>% dplyr::pull(group_variable))) {
    stop("The groupings have the same observation for the group_variable")
  }
  operator <- match.arg(operator)
  
  # Get fc_error for LHS data
  lhs_fc_error_data <- first_main_forecasting_table$fc_errors[[1]] %>% 
    dplyr::filter(fc_model == !! first_fc_model) %>% 
    dplyr::mutate(fc_model = new_fc_model)
  # Create lhs_data
  lhs_data <- get_actuals_from_main_forecasting_table(first_main_forecasting_table) %>% 
    # Take out periods that are already in fc_error table
    dplyr::filter(period < min(lhs_fc_error_data$period, 999999)) %>%
    # Rename col_of_interest to actual
    dplyr::rename(actual = col_of_interest) %>%
    # Fill in some variables that are in fc_error table
    dplyr::mutate(
      fc_model = unique(lhs_fc_error_data$fc_model),
      fc_date = unique(lhs_fc_error_data$fc_date),
      fc_periods_ahead = NA_real_
    ) %>% 
    # Bind rows and sort variable order
    dplyr::bind_rows(lhs_fc_error_data) %>% 
    dplyr::select(colnames(lhs_fc_error_data))
  # Create lhs_ts_object_train
  lhs_ts_object_train <- first_main_forecasting_table %>% 
    dplyr::pull(ts_object_train) %>% 
    tstools::unlist_if_required()
    
  # Get fc_error for RHS data
  rhs_fc_error_data <- second_main_forecasting_table$fc_errors[[1]] %>% 
    dplyr::filter(fc_model == !! second_fc_model) %>% 
    dplyr::mutate(fc_model = new_fc_model)
  # Create rhs_data
  rhs_data <- get_actuals_from_main_forecasting_table(second_main_forecasting_table) %>% 
    # Take out periods that are already in fc_error table
    dplyr::filter(period < min(rhs_fc_error_data$period, 999999)) %>%
    # Rename col_of_interest to actual
    dplyr::rename(actual = col_of_interest) %>%
    # Fill in some variables that are in fc_error table
    dplyr::mutate(
      fc_model = unique(rhs_fc_error_data$fc_model),
      fc_date = unique(rhs_fc_error_data$fc_date),
      fc_periods_ahead = NA_real_
    ) %>% 
    # Bind rows and sort variable order
    dplyr::bind_rows(rhs_fc_error_data) %>% 
    dplyr::select(colnames(rhs_fc_error_data))
  # Create rhs_ts_object_train
  rhs_ts_object_train <- second_main_forecasting_table %>% 
    dplyr::pull(ts_object_train) %>% 
    tstools::unlist_if_required()
  
  # Get all group variable names
  all_groups <- lhs_data %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(colnames(.)[!colnames(.) %in% colnames(lhs_data)]) %>% 
    colnames()
  # Create the new grouping name
  new_grouping <- rhs_data %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(all_groups) %>% 
    dplyr::mutate(
      !! sym(group_variable) := new_group_name
    ) %>% 
    tstools::add_grouping_column(group_cols = all_groups) %>% 
    dplyr::pull(grouping)
  # Adjust rhs_data and lhs_data for combining
  rhs_data <- rhs_data %>% 
    dplyr::select(-fc_error, -MASE) %>% 
    dplyr::rename(
      rhs_actual = actual,
      rhs_fc_value = fc_value
    )
  lhs_data <- lhs_data %>% 
    dplyr::select(-fc_error, -MASE) %>% 
    dplyr::rename(
      lhs_actual = actual,
      lhs_fc_value = fc_value
    )
  # Create combined data
  combined_data <- lhs_data %>% 
    dplyr::select(-grouping) %>% 
    dplyr::left_join(
      x = .,
      y = rhs_data,
      by = colnames(rhs_data)[
        !grepl("rhs_actual" , colnames(rhs_data)) & 
        !grepl("rhs_fc_value" , colnames(rhs_data)) &
        !grepl("grouping", colnames(rhs_data))
      ]
    ) %>% 
    # Add new grouping
    dplyr::mutate(grouping = new_grouping)
  # Create new actual and fc_value
  if (operator == "addition") {
    combined_ts_object_train <- (lhs_ts_object_train + rhs_ts_object_train)
    combined_data <- combined_data %>% 
      dplyr::mutate(
        actual = rhs_actual + lhs_actual,
        fc_value = rhs_fc_value + lhs_fc_value
      ) %>% 
      dplyr::select(-rhs_actual, -lhs_actual, -rhs_fc_value, -lhs_fc_value)
  }
  if (operator == "subtraction") {
    combined_ts_object_train <- (lhs_ts_object_train - rhs_ts_object_train)
    combined_data <- combined_data %>% 
      dplyr::mutate(
        actual = lhs_actual - rhs_actual,
        fc_value = lhs_fc_value - rhs_fc_value
      ) %>% 
      dplyr::select(-rhs_actual, -lhs_actual, -rhs_fc_value, -lhs_fc_value)
  }
  if (operator == "multiplication") {
    combined_ts_object_train <- (lhs_ts_object_train * rhs_ts_object_train)
    combined_data <- combined_data %>% 
      dplyr::mutate(
        actual = case_when(
          lhs_actual == 0 | rhs_actual == 0 ~ 0,
          TRUE ~ lhs_actual * rhs_actual
        ),
        fc_value = case_when(
          lhs_fc_value == 0 | rhs_fc_value == 0 ~ 0,
          TRUE ~ lhs_fc_value * rhs_fc_value
        )
      ) %>% 
      dplyr::select(-rhs_actual, -lhs_actual, -rhs_fc_value, -lhs_fc_value)
  }
  if (operator == "division") {
    combined_ts_object_train <- (lhs_ts_object_train / rhs_ts_object_train)
    combined_data <- combined_data %>% 
      dplyr::mutate(
        actual = case_when(
          lhs_actual == 0 ~ 0,
          is.na(lhs_actual) & !is.na(rhs_actual) ~ 0,
          TRUE ~ lhs_actual / rhs_actual
        ),
        fc_value = case_when(
          lhs_fc_value == 0 ~ 0,
          is.na(lhs_fc_value) & !is.na(rhs_fc_value) ~ 0,
          TRUE ~ lhs_fc_value / rhs_fc_value
        )
      ) %>% 
      dplyr::select(-rhs_actual, -lhs_actual, -rhs_fc_value, -lhs_fc_value)
  }
  # Put in fc_errors, arrange and return
  combined_data %>% 
    dplyr::mutate(fc_error = fc_value - actual) %>% 
    dplyr::group_by(grouping, fc_model, fc_date) %>% 
    dplyr::mutate(
      MASE = calculate_MASE(
        ts_object_train = combined_ts_object_train,
        fc_error = fc_error
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(colnames(lhs_fc_error_data)) %>% 
    return()
}