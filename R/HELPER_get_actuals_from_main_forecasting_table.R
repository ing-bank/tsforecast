#' Get actuals from main_forecasting_table.
#'
#' \code{get_actuals_from_main_forecasting_table} is a function to extract the
#' actuals from the main forecasting table, e.g. to be used for plotting.
#'
#' @param main_forecasting_table A tibble containing several columns of data
#'   required for time series forecasting, which has been created using the
#'   \code{create_main_forecasting_table} function.
#' @param for_plot Boolean, which is to be set to TRUE if specific
#'   transformations should be performed to make the data ready for one of the
#'   plotting functions available within the package, or set to FALSE if these
#'   additional transformation should not be applied.
#'
#' @return A tibble containing the actual values of the column of interest per
#'   period.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom tstools check_data_format unlist_if_required
#'
#' @examples
#' main_forecasting_table <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table(
#'       data = data,
#'       seasonal_periods = c(12,3)
#'    )
#' get_actuals_from_main_forecasting_table(main_forecasting_table)
get_actuals_from_main_forecasting_table <- function(main_forecasting_table, for_plot = F) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "get_actuals_from_main_forecasting_table",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid"
    )
  )
  # Initialize empty actuals tibble
  actuals <- tibble::tibble()
  # Determine groupings
  groups <- main_forecasting_table %>% 
    dplyr::select(grouping) %>% 
    dplyr::distinct() %>% 
    dplyr::pull()
  # Loop over groupings
  for (group in groups) {
    # Get last main_forecasting_table_row
    main_forecasting_table_row <- main_forecasting_table %>% 
      dplyr::filter(grouping == !! group) %>% 
      tail(1)
    # Extract ts_objects from last row
    ts_object_train <- main_forecasting_table_row %>% 
      dplyr::pull(ts_object_train) %>% 
      tstools::unlist_if_required()
    ts_object_valid <- main_forecasting_table_row %>% 
      dplyr::pull(ts_object_valid) %>% 
      tstools::unlist_if_required() 
    # Add actuals from each object, if available
    for (object in list(ts_object_train, ts_object_valid)) {
      # Check for ts_object
      if (any(class(object) == "ts")) {
        # Add to actuals
        actuals <- dplyr::bind_rows(
          actuals,
          object %>% 
            ts_object_to_tibble() %>% 
            dplyr::mutate(
              grouping = group
            )
        )
      }
    }
  }
  # Test if actuals are available
  if (nrow(actuals) == 0) {
    message <- paste("There are no actuals available in the supplied main_forecasting_table, please have a look above!")
    print(head(main_forecasting_table))
    stop(message)
  }
  # Transform for plot if required
  if (for_plot) {
    plot_actuals <- actuals %>% 
      dplyr::transmute(
        grouping = grouping,
        period = period,
        fc_model = "actuals",
        value = col_of_interest
      )
    # Add original col_of_interest if available
    if ("original_col_of_interest" %in% names(actuals)) {
      original_plot_actuals <- actuals %>% 
        dplyr::transmute(
          grouping = grouping,
          period = period,
          fc_model = "actuals_original",
          value = original_col_of_interest
        )
      plot_actuals <- dplyr::bind_rows(
        plot_actuals,
        original_plot_actuals
      )
    }
    # Now overwrite the returned object
    actuals <- plot_actuals
  }
  # Return results
  return(actuals)
}