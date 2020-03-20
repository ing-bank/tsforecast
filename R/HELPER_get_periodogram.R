#' Get an object of class "spec"
#'
#' \code{get_periodogram} is a function to get an object of class "spec" for a
#' univariate time series.
#'
#' The function will, for a univariate time series, compute the periodogram and
#' returns an object of class "spec". Components of "spec" is shown in
#' names(sp). In this package, components of "freq" and "spec" is utilised.
#'
#' @param ts_object A univariate time series.
#' @param type A character string indicating whether an additive or
#'   multiplicative seasonal component should be used to create the periodogram.
#'
#' @importFrom tstools transform_data_to_ts_object
#'
#' @return sp: an object of class "spec". We mainly use freq and spec components
#'   of sp. "freq" is Vector of frequencies at which the spectral density is
#'   estimated. "spec" is Vector of estimates of the periodogram at frequencies
#'   corresponding to freq.
#'
#' @examples
#' ts_object <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>%
#'    dplyr::select(period, col_of_interest, grouping) %>%
#'    tstools::transform_data_to_ts_object(seasonal_periods = 1)
#' get_periodogram(ts_object)
get_periodogram <- function(ts_object, type = c("additive", "multiplicative")) {
  # Check arguments
  if (!is.ts(ts_object)) stop('ts_object must be a time series object')
  if (is.matrix(ts_object) && (ncol(ts_object) > 1)) stop('ts_object must be a univariate time series')
  type <- match.arg(type)
  # Decompose col_of_interest with additive way to three components and select only seasonal component
  ts_seasonal <- decompose(ts_object, type)$seasonal
  # Neutralize the unit of time
  x <- ts(ts_seasonal, frequency = 1)
  # Do Fourier transformation
  sp <- spec.pgram(x, taper = 0, detrend = FALSE, demean = TRUE, plot = FALSE) 
  sp$spec <- 2 * sp$spec
  temp <- sp$spec[sp$freq == .5]
  sp$spec[sp$freq == .5] <- temp/2
  # Return results
  return(sp)
}