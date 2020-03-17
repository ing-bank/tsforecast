#' Sample main forecasting table for package validation
#'
#' \code{store_main_forecasting_table_for_fforma} is a function to sample a
#' subset of the available rows from the main_forecasting_table, to be used in
#' the validation of the tsforecast package.
#'
#' The function will, for each available grouping, get the first number of rows
#' specified and the last number of rows specified (to test the edge cases) as
#' well as a random sample of the rows in between of the specified size.
#'
#' @param main_forecasting_table A tibble containing several columns of data
#'   required for time series forecasting, which has been created using the
#'   \code{create_main_forecasting_table} function and which has been extended
#'   with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function.
#' @param name A character string specifying the name of the training data that
#'   is generated from the the main_forecasting_table, to be used for training
#'   FFORMA.
#' @param dir_fforma A character string specifying the path to the directory
#'   where the training data is stored to do FFORMA training.
#' @param overwrite Boolean, which is set to TRUE if the file with the specified
#'   name should be overwritten if it already exists, or set to FALSE if it is
#'   not.
#'
#' @return Nothing.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools check_data_format
#' @import dplyr
#'
#' @examples
#' main_forecasting_table <- dummy_gasprice %>%
#'      tstools::initialize_ts_forecast_data(
#'      date_col = "year_month",
#'      col_of_interest = "gasprice",
#'      group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table(
#'       seasonal_periods = NULL,
#'       min_train_periods = 188
#'    ) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       fc_methods = c("basic", "linear")
#'    )
#' store_main_forecasting_table_for_fforma(
#'    main_forecasting_table = main_forecasting_table,
#'    name = "dummy_gasprice",
#'    dir_fforma = tempdir()
#' )
store_main_forecasting_table_for_fforma <- function(main_forecasting_table, name = "", dir_fforma = "//Ad.ing.net/wps/NL/P/GD/200353/Models/FFORMA", overwrite = FALSE) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "store_main_forecasting_table_for_fforma",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_errors"
    )
  )
  # Check project_name
  if (!is.character(name) || length(name) > 1 ||nchar(name) == 0) {
    message <- paste0("The parameter 'name' should be single a non-empty character string ... \n")
    stop(message)
  }
  # Check dir_fforma
  if (!is.character(dir_fforma)) {
    message <- paste0("The parameter 'dir_fforma' should be a character string indicating a valid path to an existing directory ... \n")
    stop(message)
  }
  if (!dir.exists(dir_fforma)) {
    message <- paste0("It seems you do NOT have access to the specified location where the training data for FFORMA is stored:\n", dir_fforma, "\n")
    stop(message)
  }
  # Determine file_path
  file_path <- file.path(dir_fforma, paste0("training_data_", toupper(name), ".rds"))
  # Check if already exists and no overwrite is allowed
  if (file.exists(file_path) && !overwrite) {
    message <- paste0("There already exists a set of training data with the name '",name,"' and the parameter overwrite is set to FALSE, so no data has been stored!")
    stop(message)
  }
  # Prepare FFORMA training data
  data_to_store <- main_forecasting_table %>% 
    prepare_fforma_training_data()
  # Store results
  saveRDS(
    object = data_to_store,
    file = file_path
  )
}