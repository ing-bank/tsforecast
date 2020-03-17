#' Reduces the length of a time series object
#' 
#' \code{trim_ts_object} is a function to trim a time series object, either from
#' the left (beginning) or from the right (end).
#' 
#' @param ts_object Time series object to be trimmed.
#' @param max_length A positive integer value indicating the maximum length the
#'   resulting ts object should be.
#' @param from_left Boolean on whether to trim from the left/beginning 
#'   (from_left = T) or right/end (from_left = F) of the time series object.
#'   
#' @return A trimmed time series object.
#'   
#' @importFrom magrittr '%>%'
#' @importFrom tstools transform_data_to_ts_object
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
#'    tstools::transform_data_to_ts_object()
#' trim_ts_object(ts_object = ts_object, max_length = 42, from_left = F)
#' trim_ts_object(ts_object = ts_object, max_length = 42, from_left = T)
trim_ts_object <- function(ts_object, max_length = Inf, from_left = T) {
  # Check to make sure ts_object is a times series object
  if (!is.ts(ts_object)) {
    message <- paste0("Object 'ts_object' is of class ",paste0(class(ts_object), collapse = "/")," ... \n\n Put in a time series object!")
    stop(message)
  }
  # Check to make sure max_length is a non-negative whole number
  if (max_length != Inf) {
    if (!(is.numeric(max_length) & max_length > 0 & max_length == suppressWarnings(as.integer(max_length)))) {
      message <- paste0("The parameter 'max_length' should be a positive integer value, instead of '",max_length,"' ... ")
      stop(message)
    }
  }
  # Determine if ts_object needs to be trimmed
  if (length(ts_object) > max_length) {
    # Determine end and start dates, based on max_length and from_left values provided
    if (from_left) {
      ts_start <- min(time(ts_object))
      ts_end <- ts_start + ((1/12) * (max_length - 1))
      ts_end <- min(ts_end,max(time(ts_object)))
    } else {
      ts_end <- max(time(ts_object))
      ts_start <- ts_end - ((1/12) * (max_length - 1))
      ts_start <- max(ts_start,min(time(ts_object)))
    }
    # Create new ts_object and return
    window(
        x = ts_object, 
        start = ts_start, 
        end = ts_end
      ) %>% 
      add_missing_attributes_to_ts_object(prev_ts_object = ts_object) %>% 
      return()
  } else {
    # Return as is
    return(ts_object)
  }
}