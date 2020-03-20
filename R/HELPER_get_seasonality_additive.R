#' Get seasonality period for a time series object
#'
#' \code{get_seasonality_additive} is a function to get seasonality for a time
#' series object and the seasonality is returned as a vector of integer values.
#'
#' The function will, for a time series object (mostly, one group of
#' data), get seasonality pattern returned as a vector. The number of
#' seasonality pattern detected is defined as a input parameter of the function,
#' and it represents the top n significant seasonality pattern found.
#'
#' @param ts_object A time series object containing the values for the
#'   column_of_interest, which has been created using the
#'   \code{tstools::transform_data_to_ts_object}
#' @param top_num An int number to specify how many seasonality patterns that
#'   need to be detected, the default value is 2.
#'
#' @return seasonality: A vector containing seasonality patterns discovered. For
#'   example, (12, 3). If the ts_object is monthly time series object, it means
#'   the seasonality is 12 months and 3 months.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom tstools transform_data_to_ts_object
#'
#' @examples
#' ts_object <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    )%>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>%
#'    dplyr::select(period, col_of_interest, grouping) %>%
#'    tstools::transform_data_to_ts_object(seasonal_periods = 1)
#' get_seasonality_additive(ts_object, top_num = 2)
get_seasonality_additive <- function(ts_object, top_num = 2){
  # Check arguments
  if (!is.ts(ts_object)) stop('The specified data should be an time series object!')
  if (!(is.numeric(top_num))) stop('Given top_num is not a number, please assign an integer number to it!')
  if (top_num <= 0) stop('Given top_num is a negative number, please assign a positive integer number to it!')
  if (!(min(abs(c(top_num %% 1, top_num %% 1 - 1))) < 1e-12)) stop('Given top_num is not an integer number, please assign an integer number to it!')
  # Convert time series object to a univariate tibble
  ts_object_table <- ts_object %>% 
    force_to_univariate_ts_object() %>% 
    ts_object_to_tibble()
  # Check the required columns in the data
  if (!"period" %in% colnames(ts_object_table)) stop(paste0("The required column 'period' is not available in the data\n"))
  if (!"col_of_interest" %in% colnames(ts_object_table)) stop(paste0("The required column 'col_of_interest' is not available in the data\n"))
  if ("grouping" %in% colnames(ts_object_table)) {
    ts_data <- ts_object_table %>% 
      dplyr::select(period, col_of_interest, grouping)
  } else {
    ts_data <- ts_object_table %>% 
      dplyr::select(period, col_of_interest) %>% 
      dplyr::mutate(grouping = "ALL")
  }
  # Overwrite ts_object to make sure it has seasonality of 1
  ts_object <- tstools::transform_data_to_ts_object(
  	data = ts_data,
  	seasonal_periods = 1
  )
  # Get seasonal_periods from periodogram
  p <- get_periodogram(
    ts_object = ts_object,
    type = "additive"
  )
  dd <- tibble::tibble(freq = p$freq, spec = p$spec)
  order <- dd[order(-dd$spec),]
  time_order <- 1/order$freq
  # Remove seasonality value that is not integer
  tol <- 1e-12
  time_order_int <- time_order[sapply(time_order, function(y) min(abs(c(y %% 1, y %% 1 - 1))) < tol)]
  # Only keep the seasonality value that is smaller than half times the total number of data points in the time series object
  time_order_int <- time_order_int[time_order_int < (0.5 * nrow(ts_object))]
  # If top_num is larger than the length of time_order_int, top_num is assigned the length of time_order_int
  if (top_num > length(time_order_int)) {
    top_num <- length(time_order_int)
    warning('top_num is larger than the total number of seasonality patterns detected, so seasonal_periods contains all patterns')
  }
  # Check if any seasonality patterns have been detected
  if (length(time_order_int) > 0) {
    # Select required top number of seasonalities
    seasonality <- head(time_order_int, top_num)
  } else {
    # If no seasonality has been detected, use a default value
    seasonality <- 1
    warning('No seasonality patterns have been detected, so seasonal_periods is set to default value of 1')
  }
  # Return results
  return(seasonality)
}