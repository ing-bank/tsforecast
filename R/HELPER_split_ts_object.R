#' Split ts object into two parts
#'
#' \code{split_ts_object} is a function to split a time series object into two
#' parts based on a specified split date. The two parts can then be used
#' separately, e.g. one part for training a time series model and the other part
#' for validation of the model.
#'
#' @param ts_object A time series object to be split into two.
#' @param ts_split_date A date object or period value (in yyyymm numeric format)
#'   indicating the date at which to split the time series object into two.
#' @param output What kind of output is wanted from the split, either 'train'
#'   (which is the time range before and on the split date), 'valid' (which is
#'   the time range after the split date) or 'both'.
#' @param max_length A positive integer value indicating the maximum length the
#'   resulting ts object should be. This is in case the ts object requires
#'   trimming, e.g. to limit the historic data used for training.
#'
#' @return Depending on the value for the 'output' parameter: \itemize{
#'   \item{'train'}{ - ts object containing the time range before and on the
#'   split date} \item{'valid'}{ - ts object containing the time range after the
#'   split date} \item{'both'}{ - named list containing both of the above
#'   objects} }
#'
#' @importFrom magrittr '%>%'
#' @importFrom lubridate year month
#' @importFrom tstools date_to_period period_to_first_day
#'   transform_data_to_ts_object
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
#' ts_train_data <- split_ts_object(
#'    ts_object = ts_object,
#'    ts_split_date = 200101,
#'    output = "train",
#'    max_length = Inf
#' )
split_ts_object <- function(ts_object, ts_split_date, output = c("both","train","valid"), max_length = Inf) {
  # Check arguments
  output <- match.arg(output)
  # Check to make sure ts_object is a times series object
  if (!is.ts(ts_object)) {
    message <- paste0("Object 'ts_object' is of class ",paste0(class(ts_object), collapse = "/")," ... \n\n Put in a time series object!")
    stop(message)
  }
  # Transform ts_split_date if required
  if (is.numeric(ts_split_date) & nchar(ts_split_date) == 6) {
    ts_split_date <- tstools::period_to_first_day(ts_split_date)
  }
  # Check that ts_split_date a date
  if (!class(ts_split_date) %in% c("Date", "POSIXct", "POSIXt", "POSIXlt")) {
    message <- paste0("The input '",ts_split_date,"' for parameter ts_split_date must be either a Date or a period value (in yyyymm numeric format), but is of class ",paste0(class(ts_split_date), collapse = "/")," ... \n")
    stop(message) 
  }
  # Make sure the ts_split_date is in the ts_object
  ts_periods <- ts_object %>% 
    ts_object_to_periods() %>% 
    tstools::period_to_first_day() 
  min_ts_period <- min(ts_periods)
  max_ts_period <- max(ts_periods)
  if (!(ts_split_date >= min_ts_period & ts_split_date <= max_ts_period)) {
    message <- paste0("The specified value for ts_split_date ('YYYYMM' format is: '", tstools::date_to_period(ts_split_date),"') is not WITHIN the time series data specified for 'ts_object', which ranges from '",min_ts_period,"' to '",max_ts_period,"' ... \n")
    stop(message)
  }
  # Split of training data based on specified ts_split_date.
  split_year <- lubridate::year(ts_split_date)
  split_moment <- lubridate::month(ts_split_date)
  ts_object_training <- window(
      x = ts_object, 
      end = c(split_year,split_moment)
    ) %>% 
    trim_ts_object(
      max_length = max(1, max_length, na.rm = T),
      from_left = F
    ) %>% 
    add_missing_attributes_to_ts_object(prev_ts_object = ts_object)
  
  # Split of validation data (next month after training data)
  val_start <- max(time(ts_object_training)) + (1/12)
  val_end <- max(time(ts_object))
  if (val_start <= val_end) {
    ts_object_validation <- window(
        x = ts_object, 
        start = val_start,
        end = val_end
      ) %>% 
      trim_ts_object(
        max_length = max(1, max_length, na.rm = T),
        from_left = T
      ) %>% 
      add_missing_attributes_to_ts_object(prev_ts_object = ts_object)
  } else {
    ts_object_validation <- list()
    attr(ts_object_validation, "grouping") <- attr(ts_object, "grouping")
  }
  
  # Create output as required
  if (output == "train") return(ts_object_training)
  if (output == "valid") return(ts_object_validation)
  if (output == "both") {
    list(
        train = ts_object_training, 
        valid = ts_object_validation
      ) %>% 
      return()
  }
}