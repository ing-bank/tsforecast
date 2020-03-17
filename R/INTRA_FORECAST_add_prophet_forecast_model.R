#' Add prophet forecast model
#'
#' \code{add_prophet_forecast_model} is a function to add a single prophet
#' forecast model to a (named) list of forecast models. The forecast model is
#' created based on a parameter value to determine the flexibility of automatic
#' changepoint selection, which is then used to forecast a specific number of
#' periods ahead.
#'
#' @param fc_models A named list of forecast models, with for each forecast
#'   model a list with the model itself and a table with forecast values.
#' @param ts_object_train A time series object, which contains only the training
#'   data.
#' @param ts_object_valid A time series object, which contains the validation
#'   data. This is used for multivariate frameworks, thus it should have the
#'   forecasted/actual values of the external regressors as well.
#' @param fc_name A character string specifying the name to be used for the new
#'   model that is added to the list of existing forecast models.
#' @param model_type A character string indicating whether a univariate model
#'   (without external regressors) or a multivariate model (with external
#'   regressors) should be estimated.
#' @param changepoint_prior_scale A positive numeric value modulating the
#'   flexibility of the automatic changepoint selection, where large values will
#'   allow many changepoints, while small values will allow few changepoints.
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
#' @importFrom crayon make_style bold italic bgRed red green blue
#' @import Rcpp
#' @import rstan
#' @import prophet
#' @importFrom tstools period_delta period_to_first_day
#'   transform_data_to_ts_object
#'
#' @examples
#' ts_object_train <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    tstools::transform_data_to_ts_object()
#' add_prophet_forecast_model(
#'    fc_models = list(),
#'    ts_object_train = ts_object_train,
#'    fc_name = "fc_prophet_050cps",
#'    changepoint_prior_scale = 0.050,
#'    periods_ahead = 12,
#'    verbose = T
#' )    
add_prophet_forecast_model <- function(fc_models, ts_object_train, ts_object_valid = NULL, fc_name, model_type = c("univariate", "multivariate"), changepoint_prior_scale, periods_ahead = 1, periods_history = Inf, keep_fc_model_objects = FALSE, verbose = FALSE, log_message = "") {
  # Check to make sure fc_models is a list
  if (!is.list(fc_models) | is.data.frame(fc_models)) stop(paste0("Object 'fc_models' is of class ",paste0(class(fc_models), collapse = "/")," ... \n\n Put in a list!"))
  # Check to make sure ts_object_train is a times series object
  if (!is.ts(ts_object_train)) stop(paste0("Object 'ts_object_train' is of class ",paste0(class(ts_object_train), collapse = "/")," ... \n\n Put in a time series object!"))
  # Check to make sure fc_name is a string
  if (!is.character(fc_name)) stop(paste0("Parameter 'fc_name' is of class ",paste0(class(fc_name), collapse = "/")," ... \n\n Put in a character string!"))
  # Check to make sure ts_valid_data has enough observations if multivariate framework
  model_type <- match.arg(model_type)
  if (model_type == "multivariate") {
    if (is.null(ts_object_valid)) {stop("The parameter 'ts_object_valid' is required for a multivariate model!")} else {
      if (nrow(ts_object_valid) < periods_ahead) {
        message <- paste0("Number of observations in 'ts_object_valid' (", nrow(ts_object_valid), ") is smaller than required periods_ahead (", periods_ahead, ")!")
        stop(message)
      }
    }
  }
  # Check to make sure changepoint_prior_scale is a non-negative number
  if (!(is.numeric(changepoint_prior_scale) & changepoint_prior_scale > 0)) {
    message <- paste0("The parameter 'changepoint_prior_scale' should be a positive numeric value, instead of '",changepoint_prior_scale,"' ... ")
    stop(message)
  }
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
  
  # Prepare data for prophet forecasting
  prophet_data <- ts_object_train %>% 
    ts_object_to_tibble() %>% 
    dplyr::mutate(
      ds = tstools::period_to_first_day(period),
      y = col_of_interest
    ) %>% 
    dplyr::select(c("ds", "y", attr(ts_object_train, "xreg_cols")))
  
  # Check number of data points
  n_points <- nrow(ts_object_train)
  n_changepoints <- round(n_points * (2/3), 0)
  n_changepoints <- ifelse(n_changepoints < 25, n_changepoints, 25)
  # Determine seasonality
  yearly_seasonality <- ifelse(12 %in% attr(ts_object_train, "seasonality"), TRUE, FALSE)

  # Create the model
  prophet_model <- prophet::prophet(
    n.changepoints = n_changepoints,
    yearly.seasonality = yearly_seasonality, 
    weekly.seasonality = F,
    daily.seasonality = F,
    changepoint.prior.scale = changepoint_prior_scale
  )
  # Add regressors if required
  if (model_type == "multivariate") {
    for (xreg_col in attr(ts_object_train, "xreg_cols")) {
      prophet_model <- prophet_model %>% 
        prophet::add_regressor(
          name = xreg_col,
          prior.scale = changepoint_prior_scale
        )
    }
  }
  
  # Set seed to enable reproduction of results
  set.seed(42)
  
  # Fit the model (with error catching)
  sink(file = file.path(tempdir(),"prophet_forecast_jibberish.txt"))
  fc_model <- suppressWarnings(
    try(
      expr = {
        prophet::fit.prophet(
          m = prophet_model,
          df = prophet_data
        )
      }, 
      silent = TRUE
    )
  )
  sink()
  # Check for errors
  if (grepl("Error",fc_model[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(paste0("prophet (CPS = ",changepoint_prior_scale,")")))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(fc_model)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }

  # Create fc_data for prophet
  prophet_fc_data <- prophet::make_future_dataframe(
    m = fc_model, 
    periods = periods_ahead, 
    freq = "month",
    include_history = F
  )
  # Add regressors if required
  if (model_type == "multivariate") {
    prophet_fc_data <- ts_object_valid %>% 
      ts_object_to_tibble() %>% 
      dplyr::select(attr(ts_object_train, "xreg_cols")) %>% 
      dplyr::slice(1:periods_ahead) %>% 
      dplyr::bind_cols(
        x = prophet_fc_data,
        y = .
      )
  }
  
  # Create the forecast (with error catching)
  fc_data <- suppressWarnings(
    try(
      expr = {
        predict(
          object = fc_model,
          prophet_fc_data
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
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(paste0("prophet (CPS = ",changepoint_prior_scale,")")))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(fc_data)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  # Determine fc_date 
  fc_date <- tstools::period_delta(min(fc_periods), -1)
  # Calculate duraction in seconds
  duration <- paste0(format.default(round(as.numeric(difftime(Sys.time(), start_time, units = 'sec')), 1), nsmall = 1), "s")
  # Message
  if (verbose) {
    # Create style
    ING_orange <- make_style("#FF6200")
    # Create log_message
    log_message <- paste0(ING_orange(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message)
    log_message <- paste0(log_message, "(in ", green(duration), "): ", blue(paste0("prophet (CPS = ",changepoint_prior_scale,")")))
    ParallelLogger::logInfo(log_message)
  }
  # Decide on keeping fc_model object
  if (keep_fc_model_objects) {
    model <- fc_model
  } else {
    model <- fc_model$params
  }
  # Combine data for new forecast model
  fc_model <- list(
    fc_model =
      list(
        model = model,
        fc_data = tibble::tibble(
          fc_date = fc_date,
          period = fc_periods,
          fc_value = as.numeric(fc_data$yhat)
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