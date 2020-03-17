#' Create ungrouped main forecasting table
#'
#' \code{create_ungrouped_main_forecasting_table} is a function to create a
#' table in which every row represents a different split of the data for time
#' series forecasting. Every row contains an overview of parameters used for
#' splitting the data into time series objects for training and validation, as
#' well as the training and validation time series objects themselves.
#'
#' @param data A tibble containing the data to be used for time series
#'   forecasting, which has been created using the
#'   \code{initialize_ts_forecast_data} function from tstools.
#' @param seasonal_periods A vector of positive integer values indicating the
#'   number of data points that together compose a season (e.g. c(12,3) for
#'   quarterly and yearly seasonality when using monthly data).
#' @param min_train_periods A positive integer value indicating the minimum
#'   number of periods of data required for the training time series objects.
#' @param max_train_periods A positive integer value indicating the maximum
#'   number of periods of data to be used for the training time series objects.
#' @return A tibble containing several columns of data required for time series
#'   forecasting.
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr pmap
#' @importFrom tstools check_data_format date_to_period get_date_range
#'   months_between_periods period_to_first_day
#' @import tibble
#' @import dplyr
#'
#' @examples
#' data <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
#' create_ungrouped_main_forecasting_table(
#'    data = data,
#'    seasonal_periods = c(12, 3),
#'    min_train_periods = 25,
#'    max_train_periods = Inf
#' )
create_ungrouped_main_forecasting_table <- function(data, seasonal_periods = c(12,3), min_train_periods = 32, max_train_periods = Inf) {
  # Check data
  tstools::check_data_format(
    data = data,
    func_name = "create_ungrouped_main_forecasting_table",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  
  # Check whether seasonality should be detected
  if (is.null(seasonal_periods)) {
    detect_seasonality <- TRUE
    seasonal_periods <- 1
  } else {
    detect_seasonality <- FALSE
    if (!is.numeric(seasonal_periods) && !is.null(seasonal_periods)) {
      message <- paste0("The parameter 'seasonal_periods' should be either NULL or a (a vector of) positive integer value(s), instead of '",paste0(seasonal_periods, collapse = "/"),"' ... ")
      stop(message)
    }
  }
  
  # Overwrite train_periods if invalid
  min_train_periods <- max(1, min_train_periods, na.rm = T)
  max_train_periods <- min(Inf, max_train_periods, na.rm = T)
  if (min_train_periods > max_train_periods) stop(paste0("The parameter min_train_periods (= ",min_train_periods,") can't be greater than the parameter max_train_periods (= ",max_train_periods,") ... \n"))
  req_train_periods <- ((2 * max(seasonal_periods)) + 1)
  if (min_train_periods < req_train_periods) stop(paste0("The parameter min_train_periods (= ",min_train_periods,") should be at least 2 times as big (= ",req_train_periods,") as the maximum seasonal periods (= ",max(seasonal_periods),") ... \n\nPlease increase the min_train_periods or set the (maximum) seasonal_periods to a lower value!"))
  req_train_periods <- max(min_train_periods, req_train_periods)
  # Get date range from data
  date_range <- data %>% 
    dplyr::filter(!is.na(col_of_interest)) %>% 
    tstools::get_date_range(date_col = "period")

  # Determine start of validation period (which starts after at least 2 seasonal periods of data)
  validation_start <- tstools::period_to_first_day(date_range$start_ym)
  validation_start <- validation_start + months(req_train_periods - 1)
  validation_start <- tstools::date_to_period(validation_start)
  # Check if validation start is in the data range
  if (validation_start <= date_range$start_ym | validation_start >= date_range$end_ym) {
    message <- paste0("The required value for the first validation split ('YYYYMM' format is: '", validation_start,"') is not WITHIN the period column in the data, which ranges from '",date_range$start_ym,"' to '",date_range$end_ym,"' ... \n\nPlease extend the data or set the seasonal_periods to a lower value!")
    stop(message)
  }
  # Create time series data
  ts_object <- tstools::transform_data_to_ts_object(
    data = data, 
    seasonal_periods = seasonal_periods
  )
  # Create overview of different possible splits
  validation_splits <- seq.Date(
      from = tstools::period_to_first_day(validation_start),
      to = tstools::period_to_first_day(date_range$end_ym),
      by = "month"
    ) %>%
    format("%Y%m") %>% 
    as.numeric()
  
  # Create main forecasting table
  main_forecasting_table <- tibble::tibble(
      ts_start = date_range$start_ym,
      ts_split_date = validation_splits,
      ts_end = date_range$end_ym
    ) %>%
    dplyr::mutate(
      train_length = tstools::months_between_periods(ts_start, ts_split_date) + 1,
      valid_length = tstools::months_between_periods(ts_split_date, ts_end) 
    ) %>% 
    # Overwrite max train length if required
    dplyr::mutate(train_length = pmin(max_train_periods, train_length)) %>% 
    # Filter rows outside minimum train lengths
    dplyr::filter(train_length >= min_train_periods)
  
  # Add training and validation data
  main_forecasting_table <- main_forecasting_table %>% 
    add_ts_object_split(
      type = "train",
      ts_object = ts_object
    ) %>% 
    add_ts_object_split(
      type = "valid",
      ts_object = ts_object
    )
    
  # Detect seasonality if required to overwrite in ts_object_train and ts_object_valid
  if (detect_seasonality) {
    main_forecasting_table <- main_forecasting_table %>%
      dplyr::mutate(
        seasonality = purrr::pmap(
          .l = list(
            "ts_object" = ts_object_train 
          ),
          .f = get_seasonality_additive,
          top_num = 1
        )
      ) %>% 
      dplyr::mutate(
        ts_object_train = purrr::pmap(
          .l = list(
            "ts_object" = ts_object_train, 
            "seasonality" = seasonality
          ),
          .f = change_ts_object_seasonality
        ),
        ts_object_valid = purrr::pmap(
          .l = list(
            "ts_object" = ts_object_valid, 
            "seasonality" = seasonality
          ),
          .f = change_ts_object_seasonality
        )
      ) %>% 
      dplyr::select(-seasonality)
  }
  
  # Return main forecasting table
  return(main_forecasting_table)
}