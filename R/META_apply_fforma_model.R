#' Apply the FFORMA model
#'
#' \code{apply_fforma_model} is a function to apply the FFORMA meta model (which
#' has been trained using the \code{train_fforma_model} function) to a set of
#' model forecasts comming from a single row of the main_forecasting_table.
#'
#' @param ts_object_train A time series object, which contains only the training
#'   data.
#' @param fc_errors A tibble with data on the fc errors for all the forecast
#'   models in fc_models.
#'
#' @return A tibble with data on the fc errors for all the forecast models in
#'   fc_models, which has been extended with the FFORMA meta model forecasts and
#'   fc errors.
#'
#' @importFrom magrittr '%>%'
#' @importFrom tibble tibble
#' @importFrom tidyr gather
#' @import dplyr
#' @import caret
#' @import gbm
#' @importFrom crayon make_style bold italic bgRed red green blue
#' @importFrom tstools transform_data_to_ts_object unlist_if_required
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
#' fc_models <- add_all_univariate_forecast_models(
#'    ts_object_train = ts_object_train,
#'    periods_ahead = 12,
#'    verbose = T
#' )
#' apply_fforma_model(
#'    ts_object_train = ts_object_train,
#'    fc_models = fc_models
#' )
apply_fforma_model <- function(fc_models, ts_object_train, verbose = FALSE, log_message = "") {
  # Check fc_models
  if (!is.list(fc_models)) stop(paste0("Object 'fc_models' is of class ",paste0(class(fc_models), collapse = "/")," ... \n\n Put in a list!"))
  # Check ts_object_train
  ts_object_train <- ts_object_train %>%
    tstools::unlist_if_required()
  if (!is.ts(ts_object_train)) stop("The specified ts_object_train must be a time series object!")
  
  # Remove fc_fforma if forecast is already available
  if ("fc_fforma" %in% names(fc_models)) {
    fc_models["fc_fforma"] <- NULL
  }
  # Record start time
  start_time <- Sys.time()
  
  # Create time series features (with error catching)
  model_input <- suppressWarnings(
    try(
      expr = get_ts_features(ts_object_train), 
      silent = TRUE
    )
  )
  # Check for errors
  if (grepl("Error", model_input[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold("FFORMA"))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(model_input)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  
  # Get the weights from the model (with error catching)
  model_weights <- suppressWarnings(
    try(
      expr = {
        predict(
          object = tsforecast::fforma_model, 
          newdata = model_input,
          type = "prob"
        )
      }, 
      silent = TRUE
    )
  )
  # Check for errors
  if (grepl("Error", model_weights[1])) {
    # Show error messages
    if (verbose) {
      log_message <- paste0(red(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message, ":")
      message <- paste(log_message, bgRed("ERROR"), "for calling forecast method:", bold("FFORMA"))
      ParallelLogger::logError(message)
      message <- paste(log_message, italic(red(model_weights)))
      ParallelLogger::logError(message)
    }
    # Return without adding a model
    return(fc_models)
  }
  
  # Return without fc_fforma if not at least one of the models is present
  if (!any(names(model_weights) %in% names(fc_models))) {
    return(fc_models)
  }
  
  # Make sure sum(model_weight) = 1, in case of missing fc_models
  model_weights <- dplyr::left_join(
      x = tibble::tibble(
        fc_model = names(fc_models)
      ),
      y = model_weights %>% 
        tidyr::gather("fc_model", "model_weight"),
      by = "fc_model"
    ) %>% 
    dplyr::mutate(
      model_weight = dplyr::case_when(
        is.na(model_weight) ~ 0,
        TRUE ~ model_weight
      )
    ) %>% 
    dplyr::mutate(
      model_weight = model_weight / sum(model_weight)
    )
  
  # Create empty table to add fc_data to
  fc_data <- tibble::tibble()
  # Add fc_data from each model
  for (fc_model in names(fc_models)) {
    if ('fc_data' %in% names(fc_models[[fc_model]])) {
      fc_data <- fc_models[[fc_model]][['fc_data']] %>% 
        dplyr::mutate(
          fc_value = as.numeric(fc_value),
          fc_model = fc_model
        ) %>% 
        dplyr::bind_rows(fc_data,.)
    }
  }
  
  # Merge with fc_models data to create fc_fforma
  fc_data <- dplyr::left_join(
      x = fc_data,
      y = model_weights,
      by = "fc_model"
    ) %>% 
    # Apply weights to fc_value
    dplyr::group_by(fc_date, period) %>% 
    dplyr::summarise(
      fc_value = weighted.mean(
        x = fc_value,
        w = model_weight
      )
    ) %>% 
    dplyr::ungroup()
  
  # Calculate duraction in seconds
  duration <- paste0(format.default(round(as.numeric(difftime(Sys.time(), start_time, units = 'sec')), 1), nsmall = 1), "s")
  # Message
  if (verbose) {
    # Create style
    ING_orange <- make_style("#FF6200")
    # Create log_message
    n_models <- model_weights %>% 
      dplyr::filter(model_weight > 0) %>% 
      nrow()
    method <- paste0("FFORMA of ",n_models," models")
    log_message <- paste0(ING_orange(bold(format(Sys.time(), "%H:%M:%S"))), " - ", log_message)
    log_message <- paste0(log_message, "(in ", green(duration), "): ", blue(method))
    ParallelLogger::logInfo(log_message)
  }
  
  # Combine data for new forecast model
  fc_model <- list(fc_model =
    list(
      model = model_weights,
      fc_data = fc_data
    )
  )
  # Overwrite name of the list
  names(fc_model) <- "fc_fforma"
  # Combine within list of existing forecast models and return
  fc_models %>% 
    append(fc_model) %>% 
    return()
}