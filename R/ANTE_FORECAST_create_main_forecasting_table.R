#'Create main forecasting table.
#'
#'\code{create_main_forecasting_table} is a function to create a table in which
#'every row represents a different split of the data for time series
#'forecasting. Every row contains an overview of parameters used for splitting
#'the data into time series objects for training and validation, as well as the
#'training and validation time series objects themselves.
#'
#'@param data A tibble containing the data to be used for time series
#'  forecasting, which has been created using the
#'  \code{initialize_ts_forecast_data} function from tstools.
#'@param seasonal_periods A vector of postive integer values indicating the
#'  number of data points that together compose a season (e.g. c(12,3) for
#'  quarterly and yearly seasonality when using monthly data). If there is no
#'  seasonality in your data, simply put in 1. If the vector is written as NULL,
#'  seasonality is detected automatically
#'@param min_train_periods A positive integer value indicating the minimum
#'  number of periods of data required for the training time series objects.
#'@param max_train_periods A positive integer value indicating the maximum
#'  number of periods of data to be used for the training time series objects.
#'
#'@return A tibble containing several columns of data required for time series
#'  forecasting, which correspond to:\tabular{ccl}{ grouping \tab - \tab
#'  Indicate for which grouping the forecast is performed\cr ts_start \tab -
#'  \tab The start date of the time series object used for training the forecast
#'  models\cr ts_split_date \tab - \tab The end date of the time series object
#'  used for training the forecast models, therefore corresponding to the split
#'  date that is used to divide the available time series into training and
#'  validation sets\cr ts_end \tab - \tab The end date of the time series object
#'  used for validating the forecast models, which corresponds to the latest
#'  period that is available in the dataset for this grouping\cr train_length
#'  \tab - \tab The length of the time series object (in number of
#'  observations/periods) that is used for training the forecast models, which
#'  is also the time difference between ts_start and ts_split_date\cr
#'  valid_length \tab - \tab The length of the time series object (in number of
#'  observations/periods) that is available for validating the forecast models,
#'  which is also the time difference between ts_split_date and ts_end\cr
#'  ts_object_train \tab - \tab The time series object used for training the
#'  forecast models\cr ts_object_valid \tab - \tab The time series object
#'  available for validating the forecast models\cr }
#'@export
#'
#'@importFrom magrittr '%>%'
#'@importFrom purrr pmap
#'@importFrom tstools check_data_format create_hierarchy_list
#'@import dplyr
#'
#' @examples
#' data <- tstools::initialize_ts_forecast_data(
#'    data = dummy_gasprice,
#'    date_col = "year_month",
#'    col_of_interest = "gasprice",
#'    group_cols = c("state", "oil_company"),
#'    xreg_cols = c("spotprice", "gemprice")
#' )
#' create_main_forecasting_table(
#'    data = data,
#'    seasonal_periods = c(12,3),
#'    min_train_periods = 25,
#'    max_train_periods = Inf
#' )
create_main_forecasting_table <- function(data, seasonal_periods = c(12,3), min_train_periods = 25, max_train_periods = Inf) {
  # Check data
  tstools::check_data_format(
    data = data,
    func_name = "create_main_forecasting_table",
    req_cols = c("period", "col_of_interest", "grouping")
  )
  
  # If applicable, get hierarchy matrix
  if (any(grepl("level_", colnames(data)))) {
    hierarchy <- tstools::create_hierarchy_list(data)
    data <- data %>% 
      dplyr::select(-dplyr::starts_with("level_"))
  }
  
  # Create table with data for each grouping
  grouping_data <- data %>% 
    dplyr::select(group = grouping) %>% 
    dplyr::distinct(.keep_all = T) %>% 
    dplyr::group_by(group) %>% 
    dplyr::mutate(
      data = data %>%
        dplyr::filter(grouping == group) %>%
        list()
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(grouping = group)
  
  # Create main_forecasting_table for each grouping
  main_forecasting_table <- grouping_data %>% 
    dplyr::select(data) %>% 
    dplyr::mutate(
      main_forecasting_table = purrr::pmap(
        .l = list(
          "data" = data
        ),
        .f = create_ungrouped_main_forecasting_table,
        seasonal_periods = seasonal_periods,
        min_train_periods = min_train_periods,
        max_train_periods = max_train_periods
      )
    ) %>% 
    dplyr::select(main_forecasting_table)
  
  # Add grouping for each main_forecasting_table and unnest
  main_forecasting_table <- grouping_data %>% 
    dplyr::select(grouping) %>% 
    dplyr::bind_cols(main_forecasting_table) %>% 
    tidyr::unnest(main_forecasting_table) 
  
  # Add hierarchy information if available
  if (exists("hierarchy")) {
    main_forecasting_table <- main_forecasting_table %>% 
      dplyr::mutate(hierarchy = list(hierarchy))
  }
  
  # Return results
  return(main_forecasting_table)
}