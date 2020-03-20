#' Add a ts_object split to the main_forecasting_table
#'
#' \code{add_ts_object_split} is a function to split a time series object into
#' training or validation data and add them as a separate column to the
#' main_forecasting_table.
#'
#' @param main_forecasting_table A tibble containing five columns: ts_start,
#'   ts_split_date, ts_end, train_length and valid_length. ts_start column
#'   stores starting dates of the time series object and ts_end column stores
#'   end date. ts_split_date column give the date point where to split the time
#'   series object to be training or validation data. train_length and
#'   valid_length columns represent number of training data points and
#'   validation data points after each splitting. Thus, each row in the
#'   main_forecasting_table is one way of splitting the time series object.#'
#' @param type A character string indicates obtaining training data or
#'   validation data, which can be either "train" or "valid".
#' @param ts_object A time series object containing the values for the
#'   column_of_interest, which has been created using the
#'   \code{transform_data_to_ts_object} function from tstools.
#'
#' @return main_forecasting_table, the same tibble but extended with a new
#'   column named either "ts_object_train" or "ts_object_valid" which contains a
#'   time series object to be used for training or validation.
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr pmap
#' @importFrom tstools check_data_format transform_data_to_ts_object
#' @import dplyr
#' 
#' @examples
#' main_forecasting_table <- tibble::tibble(
#'    ts_start = 199101,
#'    ts_split_date = 199308,
#'    ts_end = 200611,
#'    train_length = 32,
#'    valid_length = 159
#' )
#' ts_object <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>%
#'    dplyr::select(period, col_of_interest, grouping) %>%
#'    tstools::transform_data_to_ts_object(seasonal_periods = c(12, 3))
#' main_forecasting_table %>%
#'    add_ts_object_split(
#'       type = "train",
#'       ts_object = ts_object
#'    ) %>%
#'    add_ts_object_split(
#'       type = "valid",
#'       ts_object = ts_object
#'    )
add_ts_object_split <- function(main_forecasting_table, type = c("train", "valid"), ts_object){
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "add_ts_object_split",
    req_cols = c("ts_start", "ts_split_date", "ts_end", "train_length", "valid_length")
  )
  # Check other arguments
  type <- match.arg(type)
  if (!is.ts(ts_object)) stop("The specified ts_object must be a time series object!")
  # Specify column names
  max_length_col <- dplyr::sym(paste0(type, "_length"))
  ts_object_col <- dplyr::sym(paste0("ts_object_", type))
  # For validation data, always keep all data
  if (type == "valid") max_length_col <- Inf
  # Add split of ts_object to main_forecasting_table and return
  main_forecasting_table %>% 
    dplyr::mutate(
      !! ts_object_col := purrr::pmap(
        .l = list(
          "ts_split_date" = ts_split_date, 
          "max_length" = !! max_length_col
        ),
        .f = split_ts_object,
        ts_object = ts_object,
        output = type
      )
    ) %>% 
    return()
}