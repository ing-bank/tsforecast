#' Extract time series features
#'
#' \code{get_ts_features} gets a time series object and extracts multiple
#' features from it.
#'
#' @param ts_object A time series object. It is assumed to have been the outcome
#'   of a process from initializing the original dataset with
#'   \code{initialize_ts_forecast_data} from tstools and eventually into ts_object_train in
#'   \code{create_main_forecasting_table}.
#'
#' @return A tibble that has 60 features of the input. See Hyndman's
#'   \href{https://cran.r-project.org/web/packages/tsfeatures/vignettes/tsfeatures.html}{tsfeatures
#'    package vignette} for information on the features themselves
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @import tsfeatures
#' @importFrom tstools unlist_if_required
#'
#' @examples
#' ts_object <- dummy_gasprice %>%
#'       tstools::initialize_ts_forecast_data(
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("oil_company", "state"),
#'       xreg_cols = c("gemprice")
#'    ) %>%
#'    create_main_forecasting_table(
#'       min_train_periods = 25
#'    ) %>%
#'    dplyr::slice(1:1) %>%
#'    dplyr::pull(ts_object_train)
#' get_ts_features(ts_object)
get_ts_features <- function(ts_object) {
  # Check ts_object
  ts_object <- ts_object %>%
    tstools::unlist_if_required()
  if (!is.ts(ts_object)) stop("The specified ts_object must be a time series object!")
  # Force to univariate and uniseasonal ts object
  ts_object <- ts_object %>% 
    force_to_univariate_ts_object() %>% 
    force_to_uniseasonal_ts_object()
  # Scale ts_object
  scaled_ts_object <- scale(
    x = ts_object,
    center = TRUE,
    scale = TRUE
  ) 
  # Small helper function
  parse_data <- function(x) tibble::as_tibble(as.list(x))
  # Set seed for reproducibility
  set.seed(42)
  # Combine all time series features
  ts_features <- dplyr::bind_cols(
    # Generic time series features
    tibble::tibble(
      # Number of samples per unit time
      frequency = frequency(scaled_ts_object),
      # Length of the time series,
      series_length = nrow(scaled_ts_object),
      # Standard deviation of the first derivative of the time series
      diff_std_err = tsfeatures::std1st_der(scaled_ts_object),
      # Kwiatkowski et al. unit root test with linear trend and lag 1
      unitroot_kpss = tsfeatures::unitroot_kpss(scaled_ts_object),
      # Z-alpha version of Phillips & Perron unit root test with constant trend and lag 1
      unitroot_pp = tsfeatures::unitroot_pp(scaled_ts_object)
    ),
    # Various measures of trend and seasonality
    parse_data(tsfeatures::stl_features(scaled_ts_object)),
    # (Relative) number of times a time series crosses the median
    parse_data(tsfeatures::crossing_points(scaled_ts_object) / nrow(scaled_ts_object)),
    # (Relative) number of flat spots in a time series
    parse_data(tsfeatures::flat_spots(scaled_ts_object) / nrow(scaled_ts_object)),
    # Lumpiness is the variance of the variances on tiled (non-overlapping) windows
    parse_data(tsfeatures::lumpiness(scaled_ts_object)),
    # Stability is the variance of the means on tiled (non-overlapping) windows
    parse_data(tsfeatures::stability(scaled_ts_object)),
    # The spectral entropy of a time series
    parse_data(tsfeatures::entropy(scaled_ts_object)),
    # Hurst coefficient indicating the level of fractional differencing
    parse_data(tsfeatures::hurst(scaled_ts_object)),
    # Nonlinearity statistic based on Teräsvirta's nonlinearity test
    parse_data(tsfeatures::nonlinearity(scaled_ts_object)),
    # Lagrange Multiplier (LM) test for autoregressive conditional heteroscedasticity (ARCH)
    parse_data(tsfeatures::arch_stat(scaled_ts_object)),
    # Various measures based on autocorrelation coefficients
    parse_data(tsfeatures::acf_features(scaled_ts_object)),
    # Various measures based on partial autocorrelation coefficients
    parse_data(tsfeatures::pacf_features(scaled_ts_object)),
    # Various measures of heterogeneity of a time series
    parse_data(tsfeatures::heterogeneity(scaled_ts_object)),
    # Smoothing parameter for the level-alpha and trend-beta
    parse_data(tsfeatures::holt_parameters(scaled_ts_object)),
    # Smoothing parameter for the level-alpha, trend-beta and seasonal-gamma
    parse_data(tsfeatures::hw_parameters(scaled_ts_object))
  )
  # Return results
  return(ts_features)
}