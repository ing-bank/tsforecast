#' Determine whether a parallel run will be faster
#'
#' \code{decide_on_parallel_run} is a function to determine whether running the
#' forecasting in parallel is going to be faster that not running it parallel.
#' Because of the initialization time of parallel forecast runs, it could be
#' faster for limited forecasting efforts to run without parallelization.
#'
#' @param fc_methods A character vector specifying the forecast methods to run. 
#' For more info \code{`?supported_fc_methods`}.
#' 
#' @param nrows A positive integer value indicating the number of rows in the
#'   main_forecasting_table that needs to be filled with forecasts.
#' @return A boolean indicating whether a parallel run is expecting to be faster
#'   (TRUE) or not (FALSE)
#'
#' @importFrom parallel detectCores
#'
#' @examples
#' decide_on_parallel_run(fc_methods = c("basic"), nrows = 100)
#' decide_on_parallel_run(fc_methods = c("basic", "prophet"), nrows = 100)
decide_on_parallel_run <- function(fc_methods = supported_fc_methods_uni_var(), nrows) {
  ## Some assumptions with this function
  #   We assume no effect from length of the dataset
  #   We assume initialization time as on a dev laptop
  #   ...
  # Check inputs
  invalid_fc_methods <- fc_methods[!fc_methods %in% supported_fc_methods_uni_var()]
  if (length(invalid_fc_methods) > 0) {
    message <- paste0("The following specified fc_methods are not implemented in 'decide_on_parallel_run()':\n", paste0("\t", invalid_fc_methods, collapse = "\n"))
    stop(message)
  }
  fc_methods <- match.arg(fc_methods, several.ok = T)
  # Check to make sure nrows is a non-negative whole number
  if (!(is.numeric(nrows) & nrows > 0 & nrows == suppressWarnings(as.integer(nrows)))) {
    message <- paste0("The parameter 'nrows' should be a positive integer value, instead of '",nrows,"' ... ")
    stop(message)
  }
  
  # Are there resursive trees and forests?
  recursive <- ("recursive" %in% fc_methods)
  # Detect n_cores
  n_cores <- parallel::detectCores()
  # Initialisation time
  parallel <- 6.5 + (n_cores - 1) * 0.5
  non_parallel <- 0
  # Basic model time
  if ("basic" %in% fc_methods) {
    parallel <- parallel + (0.21931 * nrows)
    non_parallel <- non_parallel + (0.25260 * nrows)
  }
  # Linear model time
  if ("linear" %in% fc_methods) {
    parallel <- parallel + (0.11373 * nrows)
    non_parallel <- non_parallel + (0.11548 * nrows)
  }
  # kalman model time
  if ("kalman" %in% fc_methods) {
    parallel <- parallel + (4.21 * nrows)
    non_parallel <- non_parallel + (4.97 * nrows)
  }
  # Holt-Winters model time
  if ("holt_winters" %in% fc_methods) {
    parallel <- parallel + (0.12895 * nrows)
    non_parallel <- non_parallel + (0.13824 * nrows)
  }
  # BATS model time
  if ("bats" %in% fc_methods) {
    parallel <- parallel + (2.19837 * nrows)
    non_parallel <- non_parallel + (4.99792 * nrows)
  }
  # ETS model time
  if ("ets" %in% fc_methods) {
    parallel <- parallel + (0.75675 * nrows)
    non_parallel <- non_parallel + (1.62320 * nrows)
  }
  # ARIMA model time
  if ("arima" %in% fc_methods) {
    parallel <- parallel + (0.39705 * nrows)
    non_parallel <- non_parallel + (0.46816 * nrows)
  }
  # Neural Network model time
  if ("nn" %in% fc_methods) {
    parallel <- parallel + (1.77943 * nrows)
    non_parallel <- non_parallel + (4.08072 * nrows)
  }
  # Prophet model time
  if ("prophet" %in% fc_methods) {
    parallel <- parallel + (0.89623 * nrows)
    non_parallel <- non_parallel + (2.23884 * nrows)
  }
  # Tree model time
  if ("tree" %in% fc_methods) {
    parallel <- parallel + (0.51474 * nrows) + (0.84878 * nrows * recursive)
    non_parallel <- non_parallel + (1.07708 * nrows) + (3.73360 * nrows * recursive)
  }
  # Forest model time
  if ("forest" %in% fc_methods) {
    parallel <- parallel + (0.51527 * nrows) + (0.92392 * nrows * recursive)
    non_parallel <- non_parallel + (0.92112 * nrows) + (3.06972 * nrows * recursive)
  }
  # Ensemble model time
  if ("ensemble" %in% fc_methods) {
    parallel <- parallel + (1.95457 * nrows)
    non_parallel <- non_parallel + (3.46844 * nrows)
  }
  # Recursive machine learning model time
  if ("svm" %in% fc_methods) {
    parallel <- parallel + (1.83167 * nrows * recursive)
    non_parallel <- non_parallel + (1.98416 * nrows * recursive)
  }

  # Decide
  return(parallel < non_parallel)
}