#' Union two ts objects into a single ts object
#'
#' \code{union_ts_objects} is a function to union two separate time series
#' object into a single ts object, which is comparable to a row bind.
#'
#' @param ts_object_1 The first time series object to be unioned with the
#'   second.
#' @param ts_object_2 The second time series object to be unioned with the
#'   first.
#'
#' @return A unioned time series object.
#'
#' @importFrom magrittr '%>%'
#' @importFrom zoo as.Date.yearmon
#' @importFrom forecast msts
#' @importFrom tstools date_to_period get_date_range period_to_first_day
#'   transform_data_to_ts_object
#' @import tibble
#' @import dplyr
#'
#' @examples
#' ts_object <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    tstools::transform_data_to_ts_object(seasonal_periods = 3)
#' ts_part_one <- ts_object %>%
#'    trim_ts_object(
#'       max_length = 179,
#'       from_left = T
#'    )
#' ts_part_two <- ts_object %>%
#'    trim_ts_object(
#'       max_length = 12,
#'       from_left = F
#'    )
#' ts_unioned <- union_ts_objects(
#'    ts_object_1 = ts_part_one,
#'    ts_object_2 = ts_part_two
#' )
union_ts_objects <- function(ts_object_1, ts_object_2) {
  # Check to make sure ts_objects are a times series object
  if (!is.ts(ts_object_1)) {
    message <- paste0("Object 'ts_object_1' is of class ",paste0(class(ts_object_1), collapse = "/")," ... \n\n Put in a time series object!")
    stop(message)
  }
  if (!is.ts(ts_object_2)) {
    message <- paste0("Object 'ts_object_2' is of class ",paste0(class(ts_object_2), collapse = "/")," ... \n\n Put in a time series object!")
    stop(message)
  }
  # Make sure they have the same attributes
  for (attr_to_check in c("class", "seasonality", "grouping", "xreg_cols")) {
    if (!identical(attr(ts_object_1, attr_to_check), attr(ts_object_2, attr_to_check))) {
      message <- paste0(attr_to_check," of 'ts_object_1' is '",paste0(attr(ts_object_1, attr_to_check), collapse = ", "),"', which is different from ",attr_to_check," of 'ts_object_2' which is '",paste0(attr(ts_object_2, attr_to_check), collapse = ", "),"'")
      stop(message)
    }
  }
  # Determine which ts_object starts first
  if (min(time(ts_object_1)) <= min(time(ts_object_2))) {
    start_ts_object <- ts_object_1
    end_ts_object <- ts_object_2
  } else {
    start_ts_object <- ts_object_2
    end_ts_object <- ts_object_1
  }
  # Check for overlap
  overlap <- intersect(time(start_ts_object), time(end_ts_object))
  if (length(overlap) > 0) {
    message <- paste0("The ts objects 'ts_object_1' and 'ts_object_2' can't be joined because of an overlap for the following periods:\n",paste0("\t", tstools::date_to_period(zoo::as.Date.yearmon(overlap)), collapse = ", "))
    stop(message)
  }
  # Combine the two ts_objects as tibbles
  data <- dplyr::bind_rows(
      ts_object_to_tibble(start_ts_object),
      ts_object_to_tibble(end_ts_object)
    ) %>% 
    dplyr::mutate(
      period = tstools::period_to_first_day(period),
      grouping = attr(start_ts_object, "grouping")
    )
  # Determine start of time series
  date_range <- tstools::get_date_range(data, "period")
  # Check for missing periods
  req_dates <- seq.Date(
      from = tstools::period_to_first_day(date_range$start_ym),
      to = tstools::period_to_first_day(date_range$end_ym),
      by = "month"
    ) %>% 
    tstools::date_to_period()
  # Make sure there are no missing periods
  missing_periods <- req_dates[!req_dates %in% tstools::date_to_period(data$period)]
  if (length(missing_periods) > 0) {
    message <- paste0("\nThe ts objects 'ts_object_1' and 'ts_object_2' can't be joined because of missing values for the following periods:\n",paste0(missing_periods, collapse = ", "))
    stop(message)
  }
  # Transform back to ts_object and return
  tstools::transform_data_to_ts_object(
      data = data,
      seasonal_periods = attr(start_ts_object, "seasonality")
    ) %>% 
    return()
}