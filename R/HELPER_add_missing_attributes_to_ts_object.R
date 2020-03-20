#' Add missing attributes to time series object
#' 
#' \code{add_missing_attributes_to_ts_object} is a function to add a set of 
#' missing attributes to a time series object. This is usually required after 
#' transforming a time series object, e.g. by applying the \code{window} 
#' function, during which manually added attributes are removed.
#' 
#' @param new_ts_object A time series object, to which the missing attributes 
#'   need to be added.
#' @param prev_ts_object A time series object, which contains the attributes 
#'   that are missing.
#'   
#' @importFrom tstools transform_data_to_ts_object
#'   
#' @return A time series object with an extended set of attributes.
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
#' new_ts_object <- window(
#'    x = ts_object, 
#'    end = c(2001, 1)
#' )
#' attributes(new_ts_object)
#' new_ts_object <- add_missing_attributes_to_ts_object(
#'    new_ts_object = new_ts_object,
#'    prev_ts_object = ts_object
#' )
#' attributes(new_ts_object)
add_missing_attributes_to_ts_object <- function(new_ts_object, prev_ts_object) {
  # Check to make sure ts_objects are times series objects
  if (!is.ts(new_ts_object)) {
    message <- paste0("Object 'new_ts_object' is of class ",paste0(class(new_ts_object), collapse = "/")," ... \n\n Put in a time series object!")
    stop(message)
  }
  if (!is.ts(prev_ts_object)) {
    message <- paste0("Object 'prev_ts_object' is of class ",paste0(class(prev_ts_object), collapse = "/")," ... \n\n Put in a time series object!")
    stop(message)
  }
  # Define missing attributes
  prev_attributes <- names(attributes(prev_ts_object))
  new_attributes <- names(attributes(new_ts_object))
  missing_attributes <- prev_attributes[!prev_attributes %in% new_attributes]
  # Add each of them and return
  for (attribute in missing_attributes) {
    attr(new_ts_object, attribute) <- attr(prev_ts_object, attribute)
  }
  return(new_ts_object)
}