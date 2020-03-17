#' Prepare training data for FFORMA
#'
#' \code{prepare_fforma_training_data} is function to prepare a set of training
#' data for the FFORMA meta model, based on a main_forecasting_table. From each
#' row of the main_forecasting_table the ts_object_train is used to generate a
#' set of time series features (using the \code{get_ts_features} function),
#' while the fc_errors from that same row are used to determine the
#' best_fc_model. The best_fc_model will be used as the label when training the
#' FFORMA meta model.
#'
#' @param main_forecasting_table A tibble containing a single row and several
#'   columns of data required for time series forecasting, which has been
#'   created using the \code{create_main_forecasting_table} function and which
#'   has been extended with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function.
#'
#' @return A tibble that contains the ts_object_train, grouping and
#'   ts_split_date columns from the input main_forecasting_table. An additional
#'   column that gives the name of the best forecast model (based on MASE) is
#'   also included.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tidyr unnest
#' @importFrom purrr pmap
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
#' prepare_fforma_training_data(main_forecasting_table)
prepare_fforma_training_data <- function(main_forecasting_table) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "prepare_fforma_training_data",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_errors"
    )
  )
  # Thin main_forecasting_table
  thinned_mft <- main_forecasting_table %>% 
    dplyr::select(grouping, ts_object_train, ts_split_date)
  # Pull fc_errors
  main_forecasting_table %>% 
    dplyr::pull(fc_errors) %>% 
    dplyr::bind_rows() %>% 
    # Filter specific forecast models
    dplyr::filter(!fc_model %in% c("fc_fforma", "fc_bottom_up")) %>% 
    dplyr::filter(!grepl("_consistent", fc_model)) %>% 
    # Get mean MASE
    dplyr::group_by(grouping, fc_date, fc_model) %>%
    dplyr::summarise(MASE = mean(MASE, na.rm = T)) %>% 
    # Filter to find lowest mean MASE
    dplyr::group_by(grouping, fc_date) %>% 
    dplyr::filter(MASE == min(MASE)) %>% 
    dplyr::ungroup() %>% 
    # Thin and rename for eventual left join
    dplyr::select(
      grouping, 
      ts_split_date = fc_date, 
      best_fc_model = fc_model
    ) %>% 
    # Left join with thinned main forecasting table
    dplyr::left_join(
      x = thinned_mft,
      y = .,
      by = c("grouping", "ts_split_date")
    ) %>% 
    # Return
    return()
}