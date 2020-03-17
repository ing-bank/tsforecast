#' Add kalman forecast model
#'
#' \code{add_kalman_forecast_model} is a function to add a single kalman
#' forecast model to a (named) list of forecast models.
#'
#' @param fc_models A named list of forecast models, with for each forecast
#'   model a list with the model itself and a table with forecast values.
#' @param ts_object_train A time series object, which contains only the training
#'   data.
#' @param fc_name A character string specifying the name to be used for the new
#'   model that is added to the list of existing forecast models.
#' @param periods_ahead A positive integer value indicating the number of
#'   periods to forecast ahead.
#' @param periods_history A positive integer value indicating the number of
#'   historic datapoints to use for training, which is only relevant for
#'   specific forecast methods such as drift and mean.
#' @param verbose Boolean, which is set to TRUE if status updates are valued, or
#'   set to FALSE if they are not.
#'
#' @return A named list of forecast models, with for each forecast model a list
#'   with the model itself and a table with forecast values.
#'
#' @importFrom magrittr '%>%'
#' @import tibble
#' @import dplyr
#' @importFrom dlm dlmModPoly dlmModSeas dlmMLE dlmFilter dlmForecast dlmSum
#' @importFrom tstools period_delta transform_data_to_ts_object
#'
#' @examples
#' ts_object_train <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    tstools::transform_data_to_ts_object()
#' add_kalman_forecast_model(
#'    fc_models = list(),
#'    ts_object_train = ts_object_train,
#'    fc_name = "fc_kalman_poly",
#'    periods_ahead = 12,
#'    verbose = T
#' )    
add_kalman_forecast_model <- function(fc_models, ts_object_train, fc_name, periods_ahead = 1, periods_history = Inf, verbose = FALSE, log_message = "") {
  # Check to make sure fc_models is a list
  if (!is.list(fc_models) | is.data.frame(fc_models)) stop(paste0("Object 'fc_models' is of class ",paste0(class(fc_models), collapse = "/")," ... \n\n Put in a list!"))
  # Check to make sure ts_object_train is a times series object
  if (!is.ts(ts_object_train)) stop(paste0("Object 'ts_object_train' is of class ",paste0(class(ts_object_train), collapse = "/")," ... \n\n Put in a time series object!"))
  # Check to make sure fc_name is a string
  if (!is.character(fc_name)) stop(paste0("Parameter 'fc_name' is of class ",paste0(class(fc_name), collapse = "/")," ... \n\n Put in a character string!"))
  # Check to make sure periods_ahead is a non-negative whole number
  if (!(is.numeric(periods_ahead) & periods_ahead > 0 & periods_ahead == suppressWarnings(as.integer(periods_ahead)))) {
    message <- paste0("The parameter 'periods_ahead' should be a positive integer value, instead of '",periods_ahead,"' ... ")
    stop(message)
  }
  # Check to make sure periods_history is a non-negative whole number
  if (periods_history != Inf) {
    if (!(is.numeric(periods_history) & periods_history > 0 & periods_history == suppressWarnings(as.integer(periods_history)))) {
      message <- paste0("The parameter 'periods_history' should be a positive integer value, instead of '",periods_history,"' ... ")
      stop(message)
    }
  }
  
  # Return fc_models as is if forecast is already available
  if (fc_name %in% names(fc_models)) {
    return(fc_models)
  }
  # Record start time
  start_time <- Sys.time()
  # Determine the periods for which a forecast needs to be made
  fc_periods <- get_fc_periods(
    ts_object_train = ts_object_train, 
    periods_ahead = periods_ahead
  )
  # Reduce length of the training set
  ts_object_train <- trim_ts_object(
    ts_object = ts_object_train,
    max_length = periods_history,
    from_left = F
  )
  # Set seed to enable reproduction of results
  set.seed(42)
  # create function to build kalman filter model
  if (fc_name == "fc_kalman_poly") {
    n_par <- 3
    build_model <- function(p) {
      mod <- dlm::dlmModPoly(
        order = 2, 
        dV = exp(p[1]), 
        dW = exp(p[2:3])
      )
      return(mod)
    }
  } else if (fc_name == "fc_kalman_seas_12") {
    n_par <- 6
    build_model <- function(p) {
      # Init only 3 values on diag(dW) to optimilize runtime MLE while balancing performance
      mod <- dlm::dlmModPoly(
        order = 2, 
        dV = exp(p[1]), 
        dW = exp(p[2:3])
      )
      mod_seas <- dlm::dlmModSeas(
        frequency = 12, 
        dW = c(exp(p[4:6]), rep(0, 8))
      )
      return(mod + mod_seas)
    }
  } 
  # set starting parameters for MLE (reuse previous run if possible)
  kalman_key <- paste0(fc_name, attr(ts_object_train, "grouping"))
  if (!exists("kalman_param")) {
    kalman_param <<- list()
  }
  if (!kalman_key %in% names(kalman_param)) {
    update <- kalman_param
    update[kalman_key] <- list(rep(0, n_par))
    kalman_param <<- update
  }
  # Get the maximum likelihood estimate of the parameter values
  optim_methods <- c("Nelder-Mead", "BFGS", "L-BFGS-B", "SANN", "CG")
  # Initialize model_mle to initiate the while loop
  model_mle <- list("Error")
  i <- 1
  # Loop stops when model_mle does not have error in it or all optim_methods are utilized
  while (grepl("Error", model_mle[1]) & i <= length(optim_methods)) {
    model_mle <- suppressWarnings(
      try(
        expr = {
          dlm::dlmMLE(
            y = as.numeric(ts_object_train), 
            parm = kalman_param[[kalman_key]], 
            build = build_model, 
            method = optim_methods[i]
          )
        },
        silent = TRUE
      )
    )
    i <- i + 1
  }
  # Check for errors
  if (grepl("Error", model_mle[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(paste0(fc_name, " MLE estimation")))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(model_mle[1])))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  # Update kalman_param to be used in next run
  update <- kalman_param
  update[[kalman_key]] <- model_mle$par
  kalman_param <<- update
  # Fit the kalman filter
  fc_model <- build_model(model_mle$par)
  # First do kalman filter based on space state model out previous step
  kalman_filter <- suppressWarnings(
    try(
      expr = {
        dlm::dlmFilter(
          y = as.numeric(ts_object_train), 
          mod = fc_model, 
          simplify = TRUE
        )
      },
      silent = TRUE
    )
  )
  # Check for errors
  if (grepl("Error", kalman_filter[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(paste0(fc_name, " Estimating filter")))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(fc_model)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  # Do forecast based on the filter out previous step
  fc_data <- suppressWarnings(
    try(
      expr = {
        dlm::dlmForecast(
          mod = kalman_filter, 
          nAhead = periods_ahead
        )
      },
      silent = TRUE
    )
  )
  # Check for errors
  if (grepl("Error",fc_data[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(paste0(fc_name, "Estimating forecast")))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(fc_model)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  # Calculate duraction in seconds
  duration <- paste0(format.default(round(as.numeric(difftime(Sys.time(), start_time, units = 'sec')), 1), nsmall = 1), "s")
  # Determine fc_date 
  fc_date <- tstools::period_delta(min(fc_periods), -1)
  
  # Message
  if (verbose) {
    method <- ifelse(
      fc_name == "fc_kalman_poly",
      "Kalman (polynomial)",
      "Kalman (seasonal 12)"
    )
    # Create style
    ING_orange <- make_style("#FF6200")
    # Create log_message
    log_message <- paste0(ING_orange(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message)
    log_message <- paste0(log_message, "(in ", green(duration), "): ", blue(paste0(method)))
    ParallelLogger::logInfo(log_message)
  }
  # Extract forecast model if available
  model <- fc_model
  # Combine data for new forecast model
  fc_model <- list(
    fc_model =
      list(
        model = model,
        fc_data = tibble::tibble(
          fc_date = fc_date,
          period = fc_periods,
          fc_value = as.numeric(fc_data$f)
        )
      )
  )
  # Overwrite name of the list
  names(fc_model) <- fc_name
  # Combine within list of existing forecast models and return
  fc_models %>% 
    append(fc_model) %>% 
    return()
}