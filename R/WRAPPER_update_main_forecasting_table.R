#' Update main forecasting table.
#'
#' \code{update_main_forecasting_table} is a wrapper function around the
#' following functions to create, fill and/or update the main forecasting table
#' and then store it on disk to prevent recalculation:
#' \itemize{\item{\code{create_main_forecasting_table}}
#' \item{\code{add_fc_models_to_main_forecasting_table}}
#' \item{\code{fast_write_RData_to_network}} from tstools}
#'
#' @param file_path A character string specifying a valid file path.
#' @param data A tibble containing the data to be used for time series
#'   forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function from tstools.
#' @param seasonal_periods A vector of postive integer values indicating the
#'   number of data points that together compose a season (e.g. c(12,3) for
#'   quarterly and yearly seasonality when using monthly data).
#' @param min_train_periods A positive integer value indicating the minimum
#'   number of periods of data required for the training time series objects.
#' @param max_train_periods A positive integer value indicating the maximum
#'   number of periods of data to be used for the training time series objects.
#' @param periods_ahead A positive integer value indicating the number of
#'   periods to forecast ahead.
#' @param fc_methods A character vector specifying the forecast methods to add.
#'   For more info \code{`?supported_fc_methods`}.
#' @param overwrite_fc Boolean, which is set to TRUE if all existing forecasts
#'   should be overwritten, or set to FALSE if only unavailable forecasts should
#'   be added.
#' @param allow_negative_fc Boolean, which is to be set to TRUE if negative
#'   forecast values are allowed, or set to FALSE if negative forecast values
#'   should be overwritten by a value of zero.
#' @param verbose Boolean, which is set to TRUE if status updates are valued, or
#'   set to FALSE if they are not.
#'
#' @return An updated tibble containing several columns of data required for
#'   time series forecasting, as well as a fc_models column with named lists
#'   with forecast models and forecast data, extended with a fc_errors column
#'   with the forecast values, actuals and resulting forecast errors.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr map_lgl
#' @import dplyr
#' @importFrom crayon make_style bold red green
#' @importFrom tstools check_data_format fast_write_RData_to_network
#'   unlist_if_required
#'
#' @examples
#' data <- tstools::initialize_ts_forecast_data(
#'    data = dummy_gasprice,
#'    date_col = "year_month",
#'    col_of_interest = "gasprice",
#'    group_cols = c("state", "oil_company"),
#'    xreg_cols = c("spotprice", "gemprice")
#' )
#' update_main_forecasting_table(
#'    file_path = file.path(tempdir(), "main_forecasting_table.RData"),
#'    data = data,
#'    verbose = T
#' )
update_main_forecasting_table <- function(file_path = "", data, seasonal_periods = c(12,3), 
                                          min_train_periods = 25, max_train_periods = Inf, periods_ahead = 12, 
                                          fc_methods = supported_fc_methods_uni_var(), 
                                          overwrite_fc = FALSE, 
                                          allow_negative_fc = FALSE, add_hierarchical_fc = FALSE, 
                                          verbose = FALSE, parallel = TRUE) {
  # Create style
  ING_orange <- crayon::make_style("#FF6200")
  # Initialize indicator of whether there was an update
  table_was_updated <- FALSE
  # Message
  if (verbose) cat(crayon::bold(ING_orange(format(Sys.time(), "\n%H:%M:%S -"),"CREATING REQUIRED MAIN FORECASTING TABLE\n")))
  # Create to be filled table
  required_main_forecasting_table <- create_main_forecasting_table(
    data = data,
    seasonal_periods = seasonal_periods,
    min_train_periods = min_train_periods,
    max_train_periods = max_train_periods
  )
  
  # Load main forecasting table if it exists
  if (file.exists(file_path)) {
    # Load available table
    loaded_main_forecasting_table <- readRDS(file_path)
    # Check to make sure the table is valid
    tstools::check_data_format(
      data = loaded_main_forecasting_table,
      func_name = "update_main_forecasting_table",
      req_cols = c(
        "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
        "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"
      )
    )
    # Check of all ts object attributes are the same
    prev_ts_object_train <- loaded_main_forecasting_table$ts_object_train[[1]]
    new_ts_object_train <- required_main_forecasting_table$ts_object_train[[1]]
    for (attribute in c("seasonality", "grouping", "xreg_cols")) {
      if (!identical(attr(prev_ts_object_train, attribute),attr(new_ts_object_train, attribute))) {
        message <- paste0("\nAttribute '",attribute,"' in the specified file is different from what is specified in the parameters/data:\n")
        message <- paste0(message,"\tIn specified file:\t(", paste0(attr(prev_ts_object_train, attribute), collapse = ", "),")\n")
        message <- paste0(message,"\tIn parameters/data:\t(", paste0(attr(new_ts_object_train, attribute), collapse = ", "),")\n")
        stop(message)
      }
    }
    # Keep only required columns
    loaded_main_forecasting_table <- loaded_main_forecasting_table %>% 
      dplyr::select(grouping, ts_split_date, ts_object_train_prev = ts_object_train, fc_models, fc_errors)
    # Join what is available to the required table
    available_main_forecasting_table <- dplyr::left_join(
      x = required_main_forecasting_table,
      y = loaded_main_forecasting_table,
      by = c("grouping", "ts_split_date")
    )
    # Detect whether ts_object_train has changed due to data adjustments
    available_main_forecasting_table <- available_main_forecasting_table %>% 
      dplyr::group_by(grouping, ts_split_date) %>% 
      dplyr::mutate(
        prev_sum = sum(tstools::unlist_if_required(ts_object_train_prev)),
        new_sum = sum(tstools::unlist_if_required(ts_object_train)),
        prev_available = !is.null(tstools::unlist_if_required(ts_object_train_prev)),
        ts_object_train_changed = ((sum(prev_sum != new_sum) > 0) && prev_available)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-prev_sum, -new_sum, -prev_available)
    # Determine available split dates
    available_split_dates <- available_main_forecasting_table %>% 
      dplyr::filter(!purrr::map_lgl(fc_models, is.null)) %>% 
      dplyr::filter(!ts_object_train_changed) %>% 
      dplyr::select(grouping, ts_split_date) %>% 
      dplyr::distinct()
    # Determine missing split dates
    missing_split_dates <- available_main_forecasting_table %>% 
      dplyr::select(grouping, ts_split_date, ts_object_train_changed) %>% 
      dplyr::distinct() %>% 
      dplyr::anti_join(
        x = .,
        y = available_split_dates,
        by = c("grouping", "ts_split_date")
      )
    # Fix missing fc_models and remove unnecessary columns
    available_main_forecasting_table <- available_main_forecasting_table %>% 
      dplyr::group_by(grouping, ts_split_date) %>% 
      dplyr::mutate(
        fc_models = dplyr::case_when(
          is.null(unlist(fc_models, recursive = F)) ~ list(list()),
          ts_object_train_changed ~ list(list()),
          TRUE ~ fc_models
        )
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-ts_object_train_prev, -ts_object_train_changed)
    
    # Message
    if (verbose) {
      cat(crayon::bold(ING_orange(format(Sys.time(), "%H:%M:%S -"),"FORECASTS ALREADY AVAILABLE FOR"), crayon::green(nrow(available_split_dates)), ING_orange("COMBINATIONS OF GROUPING AND SPLIT DATE\n")))
      actuals_changed <- sum(missing_split_dates$ts_object_train_changed)
      new_rows <- (nrow(missing_split_dates) - actuals_changed)
      cat(crayon::bold(ING_orange(format(Sys.time(), "%H:%M:%S -"),"FORECASTS ARE ADDED FOR"), crayon::red(new_rows), ING_orange("NEW COMBINATIONS OF GROUPING AND SPLIT DATE, AS WELL AS ANY MISSING FORECAST METHODS\n")))
      if (actuals_changed > 0) {
        cat(crayon::bold(ING_orange(format(Sys.time(), "%H:%M:%S -"),"ACTUALS HAVE CHANGED FOR"), crayon::red(actuals_changed), ING_orange("COMBINATIONS OF GROUPING AND SPLIT DATE, FORECASTS WILL BE OVERWRITTEN\n")))
      }
    } 
    # Extend the available table if required
    main_forecasting_table <- add_fc_models_to_main_forecasting_table(
      main_forecasting_table = available_main_forecasting_table,
      periods_ahead = periods_ahead,
      fc_methods = fc_methods,
      overwrite_fc = overwrite_fc,
      allow_negative_fc = allow_negative_fc,
      verbose = verbose,
      parallel = parallel
    )
    # Determine extended rows
    final_split_dates <- main_forecasting_table %>% 
      dplyr::filter(!purrr::map_lgl(fc_models, is.null)) %>% 
      dplyr::select(grouping, ts_split_date) %>% 
      dplyr::distinct()
    # Determine new forecast methods
    prev_fc_methods <- available_main_forecasting_table %>% 
      dplyr::group_by(grouping, ts_split_date) %>% 
      dplyr::summarise(
        existing_fc_models = list(names(unlist(fc_models, recursive = F)))
      ) %>% 
      dplyr::ungroup()
    new_fc_methods <- main_forecasting_table %>% 
      dplyr::group_by(grouping, ts_split_date) %>% 
      dplyr::summarise(
        new_fc_models = list(names(unlist(fc_models, recursive = F)))
      ) %>% 
      dplyr::ungroup()
    compare_fc_methods <- dplyr::full_join(
        x = prev_fc_methods,
        y = new_fc_methods,
        by = c("grouping", "ts_split_date")
      ) %>% 
      dplyr::group_by(grouping, ts_split_date) %>% 
      dplyr::filter(!is.null(unlist(existing_fc_models, recursive = F))) %>% 
      dplyr::filter(!identical(existing_fc_models, new_fc_models)) %>% 
      dplyr::ungroup()

    # Message
    if (verbose) {
      cat(crayon::bold(ING_orange(format(Sys.time(), "%H:%M:%S -"),"FORECASTS HAVE BEEN EXTENDED WITH NEW SLIDING WINDOWS FOR"), crayon::green(nrow(final_split_dates) - nrow(available_split_dates)), ING_orange("COMBINATIONS OF GROUPING AND SPLIT DATE\n")))
      cat(crayon::bold(ING_orange(format(Sys.time(), "%H:%M:%S -"),"FORECASTS HAVE BEEN EXTENDED WITH NEW FORECAST METHODS FOR"), crayon::green(nrow(compare_fc_methods)), ING_orange("COMBINATIONS OF GROUPING AND SPLIT DATE\n")))
    }
    # Update indicator
    table_was_updated <- (((nrow(final_split_dates) - nrow(available_split_dates)) > 0) | (nrow(compare_fc_methods) > 0))
  } else {
    # Message
    if (verbose) {
      cat(crayon::bold(ING_orange(format(Sys.time(), "%H:%M:%S -"),"FORECASTS NOT YET AVAILABLE, FORECASTS ARE ADDED FOR"), crayon::red(nrow(required_main_forecasting_table)), ING_orange("COMBINATIONS OF GROUPING AND SPLIT DATE\n")))
    }
    # All rows still need to be filled
    main_forecasting_table <- add_fc_models_to_main_forecasting_table(
      main_forecasting_table = required_main_forecasting_table,
      periods_ahead = periods_ahead,
      fc_methods = fc_methods,
      overwrite_fc = overwrite_fc,
      allow_negative_fc = allow_negative_fc,
      verbose = verbose,
      parallel = parallel
    )
    # Update indicator
    table_was_updated <- TRUE
  }
  
  # Only run if table has been updated
  if (table_was_updated | overwrite_fc) {
    # Add hierarchical forecasts if required
    if (add_hierarchical_fc && "hierarchy" %in% colnames(main_forecasting_table)) {
      if (verbose) {
        cat(crayon::bold(ING_orange(format(Sys.time(), "%H:%M:%S -"),"HIERARCHICAL FORECASTS ARE (RE)ADDED FOR ALL COMBINATIONS OF GROUPING AND SPLIT DATE\n")))
      }
      main_forecasting_table <- main_forecasting_table %>% 
        add_hierarchical_fc_models_to_main_forecasting_table()
    }
    # Save for quick loading next time
    if (verbose) {
      cat(crayon::bold(ING_orange(format(Sys.time(), "%H:%M:%S -"),"SAVING THE (EXTENDED) MAIN FORECASTING TABLE FOR REUSE LATER ON :)\n")))
    }
    tstools::fast_write_RData_to_network(
      data_to_save = main_forecasting_table,
      file_path = file_path
    )
  } else {
    # Make sure fc_errors is the last column
    main_forecasting_table <- main_forecasting_table %>% 
      dplyr::mutate(last_column = fc_errors) %>% 
      dplyr::select(-fc_errors) %>% 
      dplyr::rename(fc_errors = last_column)
  }
  
  # Return the table
  return(main_forecasting_table)
}