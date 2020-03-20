#' Add CTREE forecast model
#'
#' \code{add_ctree_forecast_model} is a function to add a single CTREE tree
#' forecast model to a (named) list of forecast models. The forecast model has
#' hyper-parameters that are automatically tuned. However the amount of
#' fine-tuning can be determined by the user
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
#' @param periods_ahead A positive integer value indicating the number of
#'   periods to forecast ahead.
#' @param periods_history A positive integer value indicating the number of
#'   historic datapoints to use for training, which is only relevant for
#'   specific forecast methods such as drift and mean.
#' @param verbose Boolean, indicating whether or not the function should print
#'   messages while running.
#'
#' @return A named list of forecast models, with for each forecast model a list
#'   with the model itself and a table with forecast values.
#'
#' @importFrom magrittr '%>%'
#' @importFrom timetk tk_augment_timeseries_signature
#' @importFrom purrr pmap_dbl
#' @import tibble
#' @import dplyr
#' @importFrom crayon make_style bold italic bgRed red green blue
#' @import party
#' @importFrom tstools date_to_period period_to_last_day
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
#' add_ctree_forecast_model(
#'    fc_models = list(),
#'    ts_object_train = ts_object_train,
#'    ts_object_valid = ts_object_valid,
#'    fc_name = "Example_CTREE",
#'    model_type = "multivariate",
#'    periods_ahead = 2
#'  )
add_ctree_forecast_model <- function(fc_models, ts_object_train, ts_object_valid = NULL, fc_name, model_type = c("univariate", "multivariate"), periods_ahead = 1, periods_history = Inf, keep_fc_model_objects = FALSE, verbose = FALSE, log_message = "") {
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
  
  # Run for univariate case
  if (model_type == "univariate") {
    # Decompose for ML
    ML_train_data <- decompose_ts_object_for_ML(ts_object_train, filter_date_features = T)
    # Calculate first order difference of col_of_interest
    ML_train_data <- ML_train_data %>%
      dplyr::mutate(col_of_interest = col_of_interest - dplyr::lag(col_of_interest)) %>% 
      dplyr::filter(!is.na(col_of_interest))
    # Create ML fc data
    ML_fc_data <- tibble::tibble(
      period = c(
          ts_object_to_periods(ts_object_train),
          fc_periods 
        ) %>% 
        tstools::period_to_last_day()
      ) %>%
      timetk::tk_augment_timeseries_signature() %>% 
      dplyr::filter(tstools::date_to_period(period) %in% fc_periods)
  }
  # Run for multivariate case
  if (model_type == "multivariate") {
    # Combine ts_objects into one
    ts_object_full <- union_ts_objects(
      ts_object_1 = ts_object_train,
      ts_object_2 = ts_object_valid
    )
    # Decompose for ML
    ML_full_data <- decompose_ts_object_for_ML(ts_object_full, filter_date_features = T)
    # Split of train data
    ML_train_data <- ML_full_data %>% 
      dplyr::slice(1:nrow(ts_object_train))
    # Calculate first order difference for col_of_interest
    ML_train_data <- ML_train_data %>%
      dplyr::mutate(col_of_interest = col_of_interest - dplyr::lag(col_of_interest)) %>% 
      dplyr::filter(!is.na(col_of_interest))
    # Create ML fc data
    ML_fc_data <- ML_full_data %>%   
      dplyr::select(-col_of_interest) %>% 
      dplyr::slice((nrow(ts_object_train) + 1):nrow(ML_full_data)) %>% 
      dplyr::slice(1:periods_ahead)
  }
  # Take out original_col_of_interest in ML datasets
  if ("original_col_of_interest" %in% colnames(ML_train_data)) {
    ML_train_data <- ML_train_data %>% 
      dplyr::select(-original_col_of_interest)
  }
  if ("original_col_of_interest" %in% colnames(ML_fc_data)) {
    ML_fc_data <- ML_fc_data %>% 
      dplyr::select(-original_col_of_interest)
  }
  # Set seed to enable reproduction of results
  set.seed(42)
  
  # Run each parameter combination and get resulting MAPE via the ctree_fit function
  create_runs_from_parameter_set <- function(parameters) {
    suppressWarnings(
      try(
        expr = {
          parameters %>% 
            dplyr::mutate(
              mape = purrr::pmap_dbl(
                .l = list(
                  "minsplit" = minsplit,
                  "mincriterion" = mincriterion,
                  "minbucket" = minbucket,
                  "testtype" = testtype,
                  "teststat" = teststat,
                  "nresample" = nresample
                ),
                .f = ctree_fit,
                ML_data = ML_train_data
              )
            )
        },
        silent = TRUE
      )
    )
  }
  
  # Create grid of "default" CTREE tree parameters
  parameters <- expand.grid(
    minsplit = c(as.integer((nrow(ML_train_data)/2)/3), 2),
    mincriterion = c(0.90, 0.975),
    minbucket = c(4, 7),
    testtype = c("Bonferroni", "Univariate"),
    teststat = c("quad"),
    nresample = 10000,
    stringsAsFactors = FALSE
  )
  # Create initial runs from grid search
  runs <- create_runs_from_parameter_set(parameters)
  # Check for errors
  if (grepl("Error",runs[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold("ctree (initial grid-search)"))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(runs)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  # In case of multiple instances of min(mape), randomly select one
  runs <- runs %>% 
    dplyr::filter(mape == min(mape)) %>% 
    dplyr::sample_n(1)
  
  # Create grid of parameters around first set of parameters in "run"
  parameters <- expand.grid(
    minsplit = as.integer(runif(1, min = as.integer(0.5*runs$minsplit), max = as.integer(1.5*runs$minsplit))),
    mincriterion = runif(1, min = 0.5*runs$mincriterion, max = 1.5*runs$mincriterion),
    minbucket = as.integer(runif(1, min = as.integer(0.5*runs$minbucket), max = as.integer(1.5*runs$minbucket))),
    testtype = runs$testtype,
    teststat = runs$teststat,
    nresample = as.integer(runif(1, min = as.integer(0.5*runs$nresample), max = as.integer(1.5*runs$nresample))),
    stringsAsFactors = FALSE
  )
  # Add parameters from previous run
  parameters <- runs %>% 
    dplyr::select(-mape) %>% 
    dplyr::bind_rows(parameters)
  # Compare between the last two parameter combinations
  runs <- create_runs_from_parameter_set(parameters) 
  # Check for errors
  if (grepl("Error",runs[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold("ctree (zoomed-in grid-search)"))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(runs)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  # In case of multiple instances of min(mape), randomly select one
  runs <- runs %>% 
    dplyr::filter(mape == min(mape)) %>% 
    dplyr::sample_n(1)
  
  # Run fine-tuned CTREE tree regression
  ctree_init <- suppressWarnings(
    try(
      expr = {
        party::ctree(
          formula = col_of_interest ~ .,
          data = ML_train_data,
          controls = party::ctree_control(
            teststat = runs$teststat,
            testtype = runs$testtype,
            mincriterion = runs$mincriterion, 
            minsplit = runs$minsplit,
            minbucket = runs$minbucket,
            nresample = runs$nresample
          )
        )
      },
      silent = TRUE
    )
  ) 
  # Check for errors
  if (!"BinaryTree" %in% class(ctree_init)) {
    if (grepl("Error",ctree_init[1])) {
      # Show error messages
      if (verbose) {
        log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
        message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold("ctree (final selected model)"))
        ParallelLogger::logError(message)
        message <- paste(log_message, italic(red(ctree_init)))
        ParallelLogger::logError(message)
      }
      # Return without adding a model
      return(fc_models)
    }
  }

  # Run forecast
  fc_ctree <- suppressWarnings(
    try(
      expr = {
        predict(ctree_init, ML_fc_data) 
      },
      silent = TRUE
    )
  ) 
  # Check for errors
  if (grepl("Error",fc_ctree[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold("ctree (forecast)"))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(fc_ctree)))
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
    log_message <- paste0(log_message, "(in ", green(duration), "): ", blue("ctree"))
    ParallelLogger::logInfo(log_message)
  }
  # Create forecast values
  fc_values <- as.vector(tail(ts_object_train[,"col_of_interest"], 1)) + as.vector(cumsum(fc_ctree))
  # Create new forecast model
  if (keep_fc_model_objects) {
    fc_model <- list(
      fc_model = list(
        model = ctree_init,
        fc_data = tibble::tibble(
          fc_date = fc_date,
          period = fc_periods,
          fc_value = as.numeric(fc_values)
        ),
        ML_train_data = ML_train_data
      )
    )
  } else {
    fc_model <- list(
      fc_model = list(
        model = paste0(capture.output(ctree_init@tree), collapse = "\n"),
        fc_data = tibble::tibble(
          fc_date = fc_date,
          period = fc_periods,
          fc_value = as.numeric(fc_values)
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