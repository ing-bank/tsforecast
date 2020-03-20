#' Add randomForest forecast model
#'
#' \code{add_recursive_ml_forecast_model} is a function to add a recursive
#' machine learning model from the caret package. It tunes the parameters with
#' default settings in a cross validated way.
#'
#' @param fc_models A named list of forecast models, with for each forecast
#'   model a list with the model itself and a table with forecast values.
#' @param caret_model_tag A character string specifying the tag of the model to
#'   be used from the caret meta package.
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
#' @param verbose Boolean, indicating whether or not the function should print
#'   messages while running.
#'
#' @return A named list of forecast models, with for each forecast model a list
#'   with the model itself and a table with forecast values.
#'
#' @importFrom magrittr '%>%'
#' @importFrom timetk tk_augment_timeseries_signature
#' @importFrom tidyr drop_na
#' @import tibble
#' @import dplyr
#' @importFrom crayon make_style bold italic bgRed red green blue
#' @import default
#' @import lattice
#' @import caret
#' @importFrom tstools date_to_period period_delta period_to_last_day
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
#' add_recursive_ml_forecast_model(
#'    fc_models = list(),
#'    caret_model_tag = "svmRadialSigma",
#'    ts_object_train = ts_object_train,
#'    ts_object_valid = ts_object_valid,
#'    fc_name = "Example_svm",
#'    model_type = "multivariate",
#'    periods_ahead = 2,
#'    verbose = TRUE
#'  )
add_recursive_ml_forecast_model <- function(fc_models, caret_model_tag, ts_object_train, ts_object_valid = NULL,
                                            fc_name, model_type = c("univariate", "multivariate"), periods_ahead = 1,
                                            periods_history = Inf, keep_fc_model_objects = FALSE, 
                                            verbose = FALSE, log_message = "") {
  # Check to make sure fc_models is a list
  if (!is.list(fc_models)) stop(paste0("Object 'fc_models' is of class ",paste0(class(fc_models), collapse = "/")," ... \n\n Put in a list!"))
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
  # Determine the months for which a forecast needs to be made
  fc_periods <- get_fc_periods(ts_object_train, periods_ahead)
  # Reduce length of the training set
  ts_object_train <- trim_ts_object(
    ts_object = ts_object_train,
    max_length = periods_history,
    from_left = F
  )
  # Sets full ts_object and xreg_cols
  if (model_type == "multivariate") {
    # Combine ts_objects into one
    ts_object_full <- union_ts_objects(
      ts_object_1 = ts_object_train,
      ts_object_2 = ts_object_valid
    )
    xreg_cols <- attr(ts_object_full,"xreg_cols")
  } else {
    ts_object_full <- ts_object_train
    xreg_cols <- NULL
  }
  
  # creates dataframe from ts object
  dates <- ts_object_full %>% 
    ts_object_to_periods()
  data <- ts_object_full %>% 
    timetk::tk_tbl(preserve_index = FALSE) %>% 
    dplyr::mutate(period = dates)
    
  # Removes future values or adds NAs to be filled in
  if (model_type == "multivariate") {
    train_data <- data %>%
      mask_future_values(fc_periods)
  } else {
    train_data <- dplyr::bind_rows(
        data,
        tibble::tibble(
          period = fc_periods,
          col_of_interest = NA
        )
      )
  }

  # Creates transformer of the data
  max_lag <- max(attr(ts_object_full, "seasonality"))
  transform <- transform
  inv_transform <- inv_diff
  default(transform) <- list(
    target_col = "col_of_interest",
    xreg_cols = xreg_cols, 
    transformation = diff,
    lags = seq(1, max_lag, 1),
    drifts = attr(ts_object_full, "seasonality")
  )

  # Creates training data
  ML_train_data <- train_data %>% 
    dplyr::mutate(period = tstools::period_to_last_day(period)) %>% 
    transform()
  
  # Set seed to enable reproduction of results
  set.seed(42)
  
  # Currently cross validated, could be improved with time series validated
  # https://topepo.github.io/caret/data-splitting.html
  fitControl <- caret::trainControl(method = "cv", number = 5)
  
  # Trains using standard grid of the model, could be improved by using special
  # grid per model, http://topepo.github.io/caret/model-training-and-tuning.html#customizing-the-tuning-process
  # Run fine-tuned RPART tree regression
  model <- suppressWarnings(
    try(
      expr = {
        caret::train(
          col_of_interest ~ ., 
          data = ML_train_data, 
          method = caret_model_tag, 
          trControl = fitControl
        )
      },
      silent = TRUE
    )
  ) 
  # Check for errors
  if (grepl("Error",model[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(fc_name), "(model estimation")
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(model)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }  
  
  # Run forecast
  forecasts <- suppressWarnings(
    try(
      expr = {
        train_data %>% 
          dplyr::mutate(
            period = tstools::period_to_last_day(period)
          ) %>% 
          get_forecasts(
            model, 
            transform, 
            inverse_transform_target = inv_transform, 
            steps = periods_ahead
          )
      },
      silent = TRUE
    )
  )
  # Check for errors
  if (grepl("Error",forecasts[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold(fc_name),"(forecasting)")
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(forecasts)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }  
  
  # Create fc_date and periods
  fc_date <- tstools::period_delta(min(fc_periods), -1)
  # Calculate duraction in seconds
  duration <- paste0(format.default(round(as.numeric(difftime(Sys.time(), start_time, units = 'sec')), 1), nsmall = 1), "s")
  # Message
  if (verbose) {
    # Create style
    ING_orange <- make_style("#FF6200")
    # Create log_message
    log_message <- paste0(ING_orange(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message)
    log_message <- paste0(log_message, "(in ", green(duration), "): ", blue(fc_name))
    ParallelLogger::logInfo(log_message)
  }
  # Combine data for new forecast model
  if (keep_fc_model_objects) {
    # Create new forecast model
    fc_model <- list(
      fc_model = list(
        model = model,
        fc_data = tibble::tibble(
          fc_date = fc_date,
          period = fc_periods,
          fc_value = as.numeric(forecasts$col_of_interest)
        ),
        ML_train_data = ML_train_data
      )
    )
  } else {
    # Create new forecast model
    fc_model <- list(
      fc_model = list(
        model = model$modelInfo$label,
        fc_data = tibble::tibble(
          fc_date = fc_date,
          period = fc_periods,
          fc_value = as.numeric(forecasts$col_of_interest)
        )
      )
    )
  }
  names(fc_model) <- fc_name
  # Combine within list of existing forecast models and return
  fc_models %>% 
    append(fc_model) %>% 
    return()
}