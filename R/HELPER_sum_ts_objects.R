#' Sum two ts objects to get a single ts object
#'
#' \code{sum_ts_objects} is a function to sum two separate time series object to
#' get a single ts object.
#'
#' @param ts_objects A list of time series object to be summed.
#' @param new_grouping A character vector containing the grouping of the newly
#'   created time series object.
#'
#' @return A summed time series object.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools transform_data_to_ts_object unlist_if_required
#' @importFrom purrr map
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
#' sum_ts_objects(
#'    ts_objects = list(
#'       ts_object,
#'       ts_object,
#'       ts_object
#'    ),
#'    new_grouping = "state = Trice New York   &   oil_company = Trice CompanyA"
#' )
sum_ts_objects <- function(ts_objects, new_grouping = "") {
  # Check to make sure ts_objects is a list
  if (!is.list(ts_objects)) {
    message <- paste0("Object 'ts_objects' is of class ",paste0(class(ts_objects), collapse = "/")," ... \n\n Put in a list object!")
    stop(message)
  }
  # Which is not empty
  if (length(ts_objects) == 0) {
    stop("The specified 'ts_objects' is empty ...!")
  }
  # Check if it contains time series objects
  if (!is.ts(ts_objects[[1]])) {
    if (is.null(attr(ts_objects[[1]], "grouping"))) {
      print("check 4 fail")
      message <- paste0("The specified list for 'ts_objects' contains ",paste0(class(ts_objects[[1]]), collapse = "/")," ... \n\n Put in time series objects!")
      stop(message)
    } else {
      # Extract empty object
      ts_object <- ts_objects[[1]] %>% 
        tstools::unlist_if_required()
      # Overwrite grouping attribute
      attr(ts_object, "grouping") <- new_grouping
      # Return empty object as is
      return(ts_object)
    }
  }
  if (length(ts_objects) == 1) {
    # Extract single time series
    ts_object <- ts_objects %>% 
      tstools::unlist_if_required()
    # Overwrite grouping attribute
    attr(ts_object, "grouping") <- new_grouping
    # Return time series as is
    return(ts_object)
  }
  # Check new_grouping
  if (!is.character(new_grouping) || nchar(new_grouping) == 0) {
    stop("The specified parameter 'new_grouping' should be a non-emtpy character vector!")
  }
  # Combine all time series objects
  ts_data <- ts_objects %>% 
    purrr::map(ts_object_to_tibble) %>% 
    dplyr::bind_rows()
  # Detect which data columns are present
  data_cols <- colnames(ts_data)[colnames(ts_data) != "period"]
  # Define quosures to sum data columns
  summary_quosures <- setNames(
    object = purrr::map(
      .x = data_cols, 
      .f = ~dplyr::quo(sum(!! dplyr::sym(.x), na.rm = T))
    ),
    nm = data_cols
  )
  # Apply summary quosures and add new grouping
  ts_data <- ts_data %>% 
    dplyr::group_by(period) %>% 
    dplyr::summarise(!!! summary_quosures) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      grouping = new_grouping
    )
  # Transform back to ts_object and return
  ts_data %>% 
    tstools::transform_data_to_ts_object(
      seasonal_periods = attr(ts_objects[[1]], "seasonality")
    ) %>% 
    return()
}