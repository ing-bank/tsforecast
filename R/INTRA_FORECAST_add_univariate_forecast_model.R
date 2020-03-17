#' Add univariate forecast model
#' 
#' \code{add_univariate_forecast_model} is a function to add a single univariate
#' forecast model to a (named) list of forecast models. The forecast model is 
#' created based on a model formula in conjunction with other parameters, which 
#' is then used to forecast a specific number of periods ahead.
#' 
#' @param fc_models A named list of forecast models, with for each forecast 
#'   model a list with the model itself and a table with forecast values.
#' @param ts_object_train A time series object, which contains only the training
#'   data.
#' @param fc_name A character string specifying the name to be used for the new 
#'   model that is added to the list of existing forecast models.
#' @param fc_formula A character string specifying the expression to be 
#'   evaluated to train the time series forecast model.
#' @param model_fc Boolean, which is to be set to TRUE if the forecast 
#'   expression specified for fc_formula returns an object with a forecast 
#'   method, or set to FALSE if the forecast expression returns a forecast 
#'   directly.
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
#' @import forecast
#' @import nnfor
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
#' add_univariate_forecast_model(
#'    fc_models = list(),
#'    ts_object_train = ts_object_train,
#'    fc_name = "fc_drift_l6m",
#'    fc_formula = "rwf(x, h, drift = TRUE)",
#'    model_fc = FALSE,
#'    periods_ahead = 12,
#'    periods_history = 6,
#'    verbose = T
#' )  
add_univariate_forecast_model <- function(fc_models, ts_object_train, fc_name, fc_formula, model_fc = FALSE, periods_ahead = 1, periods_history = Inf, verbose = FALSE, log_message = "") {
  # Check to make sure fc_models is a list
  if (!is.list(fc_models) | is.data.frame(fc_models)) stop(paste0("Object 'fc_models' is of class ",paste0(class(fc_models), collapse = "/")," ... \n\n Put in a list!"))
  # Check to make sure ts_object_train is a times series object
  if (!is.ts(ts_object_train)) stop(paste0("Object 'ts_object_train' is of class ",paste0(class(ts_object_train), collapse = "/")," ... \n\n Put in a time series object!"))
  # Check to make sure fc_name is a string
  if (!is.character(fc_name)) stop(paste0("Parameter 'fc_name' is of class ",paste0(class(fc_name), collapse = "/")," ... \n\n Put in a character string!"))
  # Check to make sure fc_formula is a string
  if (!is.character(fc_formula)) stop(paste0("Parameter 'fc_formula' is of class ",paste0(class(fc_formula), collapse = "/")," ... \n\n Put in a character string!"))
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
  # Determine fc_date 
  fc_date <- tstools::period_delta(min(fc_periods), -1)
  # Generate the forecast formula
  fc_formula <- gsub("\\(x", "\\(ts_object_train", fc_formula)
  if (model_fc) {
    # Sets PI False because ensemble method sets default to True and nnetar is slow bc bootstraping
    fc_formula <- paste0("forecast(object = ",fc_formula,", h = periods_ahead, PI = FALSE)") 
  } else {
    fc_formula <- gsub(", h", ", periods_ahead", fc_formula)
  }
  # Reduce length of the training set
  ts_object_train <- trim_ts_object(
    ts_object = ts_object_train,
    max_length = periods_history,
    from_left = F
  )

  # Set seed to enable reproduction of results
  set.seed(42)
  
  # Create the forecast (with error catching)
  fc_data <- suppressWarnings(
    try(
      expr = eval(parse(text = fc_formula)), 
      silent = TRUE
    )
  )
  # If arima fails, try to adjust the settings
  if (grepl("No suitable ARIMA model found",fc_data[1])) {
    # Adjust forecast formula
    fc_formula <- fc_formula %>% 
      gsub("stepwise = T", "stepwise = F", .) %>% 
      gsub("approximation = T", "approximation = F", .)
    # Try again
    fc_data <- suppressWarnings(
      try(
        expr = eval(parse(text = fc_formula)), 
        silent = TRUE
      )
    )
  }
  # If any of the other models fail, try uniseasonal
  if (grepl("Error",fc_data[1])) {
    # Adjust forecast formula
    fc_formula <- fc_formula %>% 
      gsub("ts_object_train", "force_to_uniseasonal_ts_object(ts_object_train)", .)
    # Try again
    fc_data <- suppressWarnings(
      try(
        expr = eval(parse(text = fc_formula)), 
        silent = TRUE
      )
    )
  }
  # Check for errors
  if (grepl("Error",fc_data[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(fc_formula))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(fc_data)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  # Calculate duraction in seconds
  duration <- paste0(format.default(round(as.numeric(difftime(Sys.time(), start_time, units = 'sec')), 1), nsmall = 1), "s")
  # Message
  if (verbose) {
    # Create style
    ING_orange <- make_style("#FF6200")
    # Create log_message
    if (grepl("_ensemble_", fc_name)) {
      method <- fc_name
    } else {
      method <- fc_data$method
    }
    log_message <- paste0(ING_orange(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message)
    log_message <- paste0(log_message, "(in ", green(duration), "): ", blue(method))
    ParallelLogger::logInfo(log_message)
  }
  # Extract forecast model if available
  model <- fc_data$model
  if (is.null(model)) model <- fc_data$method
  if ("coefficients" %in% names(model)) model <- model$coefficients
  if ("coef" %in% names(model)) model <- model$coef
  if (any(class(model) %in% c("bats", "ets", "nnetarmodels"))) model <- paste0(capture.output(model), collapse = "\n")
  # Combine data for new forecast model
  fc_model <- list(fc_model =
    list(
      model = model,
      fc_data = tibble::tibble(
        fc_date = fc_date,
        period = fc_periods,
        fc_value = as.numeric(fc_data$mean)
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