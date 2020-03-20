#' Add multivariate forecast model
#'
#' \code{add_multivariate_forecast_model} is a function to add a single
#' multivariate forecast model to a (named) list of forecast models. The
#' forecast model is created based on a model formula in conjunction with other
#' parameters, which is then used to forecast a specific number of periods
#' ahead.
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
#' @param fc_formula A character string specifying the expression to be
#'   evaluated to train the time series forecast model.
#' @param periods_ahead A positive integer value indicating the number of
#'   periods to forecast ahead.
#' @param periods_history A positive integer value indicating the number of
#'   historic datapoints to use for training, which is only relevant for
#'   specific forecast methods such as drift and mean.
#' @param keep_fc_model_objects Boolean, which is set to TRUE in order to keep
#'   original fc_model objects in the main_forecasting_table after running the
#'   forecast. This is needed for scenario analysis in multivariate forecasting.
#'   However, it may lead to memory issues, as the main_forecasting_table
#'   increases in size.
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
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    dplyr::slice(1:189) %>%
#'    tstools::transform_data_to_ts_object()
#' ts_object_valid <- tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company"),
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    dplyr::slice(190:191) %>%
#'    tstools::transform_data_to_ts_object()
#' add_multivariate_forecast_model(
#'    fc_models = list(),
#'    ts_object_train = ts_object_train,
#'    ts_object_valid = ts_object_valid,
#'    fc_name = "fc_linear_trend_xreg",
#'    fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
#'    periods_ahead = 2,
#'    verbose = T
#' )    
add_multivariate_forecast_model <- function(fc_models, ts_object_train, ts_object_valid = NULL, fc_name, fc_formula, periods_ahead = 1, periods_history = Inf, keep_fc_model_objects = FALSE, verbose = FALSE, log_message = "") {
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
  # Reduce length of the training set
  ts_object_train <- trim_ts_object(
    ts_object = ts_object_train,
    max_length = periods_history,
    from_left = F
  )
  # Prepare new data for forecast
  xreg_data <- ts_object_valid %>% 
    ts_object_to_tibble() %>% 
    dplyr::select(attr(ts_object_train, "xreg_cols")) %>% 
    dplyr::slice(1:periods_ahead)

  # Set seed to enable reproduction of results
  set.seed(42)
  # Store original fc_formula
  original_fc_formula <- fc_formula
  # Generate the model object
  fc_model <- suppressWarnings(
    try(
      expr = eval(parse(text = fc_formula)), 
      silent = TRUE
    )
  )
  # If arima fails, try to adjust the settings
  if (grepl("No suitable ARIMA model found",fc_model[1])) {
    # Adjust forecast formula
    fc_formula <- fc_formula %>% 
      gsub("stepwise = T", "stepwise = F", .) %>% 
      gsub("approximation = T", "approximation = F", .)
    # Try again
    fc_model <- suppressWarnings(
      try(
        expr = eval(parse(text = fc_formula)), 
        silent = TRUE
      )
    )
  }
  # If any of the other models fail, try uniseasonal
  if (grepl("Error",fc_model[1])) {
    # Adjust forecast formula
    fc_formula <- fc_formula %>% 
      gsub("ts_object_train", "force_to_uniseasonal_ts_object(ts_object_train)", .)
    # Try again
    fc_model <- suppressWarnings(
      try(
        expr = eval(parse(text = fc_formula)), 
        silent = TRUE
      )
    )
  }
  # Check for errors
  if (grepl("Error",fc_model[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(fc_formula))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(fc_model)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  # Generate the forecast formula
  fc_formula <- "forecast(object = fc_model, h = periods_ahead, xreg = xreg_data)"
  if (grepl("tslm\\(", original_fc_formula)) {
    fc_formula <- gsub(", xreg = ", ", newdata = ", fc_formula)
  }
  if (grepl("auto.arima\\(", original_fc_formula) | grepl(", method = 'arima', ", original_fc_formula)) {
    fc_formula <- gsub(", xreg = xreg_data", ", xreg = as.matrix(xreg_data)", fc_formula)
  }
  # Create the forecast (with error catching)
  fc_data <- suppressWarnings(
    try(
      expr = eval(parse(text = fc_formula)), 
      silent = TRUE
    )
  )
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
    log_message <- paste0(ING_orange(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message)
    log_message <- paste0(log_message, "(in ", green(duration), "): ", blue(fc_data$method))
    ParallelLogger::logInfo(log_message)
  }
  # Extract forecast model if available
  model <- fc_model
  if (!keep_fc_model_objects) {
    if ("coefficients" %in% names(model)) model <- model$coefficients
    if ("coef" %in% names(model)) model <- model$coef
    if (any(class(model) == "nnetar")) model <- paste0(capture.output(model), collapse = " ")
    if (any(names(model) == "stl")) model <- paste0(suppressWarnings(capture.output(model$model)), collapse = " ")
  }
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