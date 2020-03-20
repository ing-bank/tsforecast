#' Add or modify the seasonality attribute of a time series object
#'
#' \code{change_ts_object_seasonality} is a function to modify or add a
#' seasonality attribute to an existing time series object.
#'
#' @param ts_object A time series object containing the values for the
#'   column_of_interest, which has been created using the
#'   \code{transform_data_to_ts_object} from tstools, or alternatively an empty list.
#' @param seasonality A numerical vector indicating the seasonality to employ
#'   for the time series object, for example c(12, 3).
#'
#' @return A time series object with the specified seasonality.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tstools transform_data_to_ts_object
#' @import dplyr
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
#'    tstools::transform_data_to_ts_object(seasonal_periods = 1)
#' change_ts_object_seasonality(ts_object, seasonality = c(12, 3, 4))
change_ts_object_seasonality <- function(ts_object, seasonality) {
  # Check arguments
  if (is.list(ts_object) & length(ts_object) == 0) return(ts_object)
  if (!is.ts(ts_object)) stop('ts_object must be a time series object')
  if (!is.numeric(seasonality) || any(seasonality < 1)) stop("The parameter 'seasonality' should be a vector of positive integer values")
  if (!(min(abs(c(seasonality %% 1, seasonality %% 1 - 1))) < 1e-12)) stop("The parameter 'seasonality' should be a vector of positive integer values")
  # Revert ts_object to a tibble and then back to a ts_object with a new seasonality
  ts_object %>%
    ts_object_to_tibble() %>%
    dplyr::mutate(
      grouping = attr(ts_object, "grouping")
    ) %>%
    tstools::transform_data_to_ts_object(
      seasonal_periods = unique(seasonality)
    ) %>%
    add_missing_attributes_to_ts_object(
      new_ts_object = .,
      prev_ts_object = ts_object
    ) %>% 
    return()
}