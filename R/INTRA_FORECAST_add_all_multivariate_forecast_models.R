#' Add all multivariate forecast models
#'
#' \code{add_all_multivariate_forecast_models} is a wrapper function to add
#' multiple multivariate forecast models to a (named) list of forecast models.
#' The forecast models are created based on multiple calls of the
#' add-forecasting-model functions in the tsforecast package. The fc_methods
#' parameter can be used to control which forecast models are added.
#'
#' @param ts_object_train A time series object, which contains only the training
#'   data.
#' @param ts_object_valid A time series object, which contains the validation
#'   data. This is used for multivariate frameworks, thus it should have the
#'   forecasted/actual values of the external regressors as well.
#' @param fc_models A named list of forecast models, with for each forecast
#'   model a list with the model itself and a table with forecast values.
#' @param periods_ahead A positive integer value indicating the number of
#'   periods to forecast ahead.
#' @param periods_history A positive integer value indicating the number of
#'   historic datapoints to use for training, which is only relevant for
#'   specific forecast methods such as drift and mean.
#' @param fc_methods A character vector specifying the forecast methods to add. 
#' For more info \code{`?supported_fc_methods`}.
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
#' @importFrom crayon blue
#' @import forecast
#' @import nnfor
#' @import rstan
#' @import prophet
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
#'    dplyr::slice(1:179) %>%
#'    tstools::transform_data_to_ts_object()
#' ts_object_valid <- tstools::initialize_ts_forecast_data(
#'      data = dummy_gasprice,
#'      date_col = "year_month",
#'      col_of_interest = "gasprice",
#'      group_cols = c("state", "oil_company"),
#'      xreg_cols = c("spotprice", "gemprice")
#'    ) %>%
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
#'    dplyr::slice(180:191) %>%
#'    tstools::transform_data_to_ts_object()
#' res <- add_all_multivariate_forecast_models(
#'    ts_object_train = ts_object_train,
#'    ts_object_valid = ts_object_valid,
#'    periods_ahead = 12,
#'    verbose = T
#' )
add_all_multivariate_forecast_models <- function(ts_object_train, ts_object_valid, fc_models = list(), 
                                                 periods_ahead = 1, periods_history = Inf,
                                                 fc_methods = supported_fc_methods_multi_var(), 
                                                 keep_fc_model_objects = FALSE, 
                                                 verbose = FALSE, parallel = FALSE) {
  # Unlist ts_object_train if required
  ts_object_train <- tstools::unlist_if_required(ts_object_train)
  # Check to make sure ts_object_train is a times series object
  if (!is.ts(ts_object_train)) stop(paste0("Object 'ts_object_train' is of class ",paste0(class(ts_object_train), collapse = "/")," ... \n\n Put in a time series object!"))
  # Check that xreg_cols in ts_object_train is not NULL
  if (length(attr(ts_object_train, "xreg_cols")) == 0) stop("Object ts_object_train has no external regressor columns!")
  # Unlist ts_object_valid if required
  ts_object_valid <- tstools::unlist_if_required(ts_object_valid)
  # Check if ts_object_valid is not an empty list
  if (!(is.list(ts_object_valid) & length(ts_object_valid) == 0)) {
    # Check to make sure ts_object_valid is a times series object
    if (!is.ts(ts_object_valid)) stop(paste0("Object 'ts_object_valid' is of class ",paste0(class(ts_object_valid), collapse = "/")," ... \n\n Put in a time series object!"))
    # Check that xreg_cols in ts_object_valid is not NULL
    if (length(attr(ts_object_valid, "xreg_cols")) == 0) stop("Object ts_object_valid has no external regressor columns!")
    # Check that xreg_cols in both ts objects are the same
    if (length(attr(ts_object_valid, "xreg_cols")) != length(attr(ts_object_train, "xreg_cols"))) stop("xreg_cols dimension mismatch between ts_object_train and ts_object_valid. Make sure they are the same!")
    if (!all(attr(ts_object_valid, "xreg_cols") %in% attr(ts_object_train, "xreg_cols"))) stop("xreg_cols mismatch between ts_object_train and ts_object_valid. Make sure they consist of the same variables!")
    # Determine available periods_ahead based on length of xreg_cols in ts_object_valid
    available_periods_ahead <- min(periods_ahead, nrow(ts_object_valid))
  } else {
    # Otherwise set to zero (no forecasting done)
    available_periods_ahead <- 0
  }
  # Check to make sure fc_models is a list
  if (!is.list(fc_models) | is.data.frame(fc_models)) stop(paste0("Object 'fc_models' is of class ",paste0(class(fc_models), collapse = "/")," ... \n\n Put in a list!"))
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
  # Check specified forecast methods
  invalid_fc_methods <- fc_methods[!fc_methods %in% supported_fc_methods_multi_var()]
  if (length(invalid_fc_methods) > 0) {
    message <- paste0("The following specified fc_methods are not valid:\n", paste0("\t", invalid_fc_methods, collapse = "\n"))
    stop(message)
  }
  fc_methods <- match.arg(fc_methods, several.ok = T)
  
  # Initialize log message
  log_message <- ""
  # Expand log_message if verbose
  if (verbose) {
    # Get input for message
    grouping <- ts_object_train %>% 
      attr("grouping") %>% 
      gsub("   ", " ", .)
    split_month <- ts_object_train %>% 
      ts_object_to_periods() %>% 
      max()
    # Add split month
    log_message <- split_month
    # Add grouping
    log_message <- paste0(log_message, " - ", blue(gsub("\\s+"," ",grouping)))
    # Add periods ahead
    log_message <- paste0(log_message, " - ", periods_ahead, "m fc ")
  }
  
  # Extract information on external regressors
  xreg_cols <- attr(ts_object_train, "xreg_cols")
  
  # Remove original column of interest if available
  ts_object_train <- cleanup_multivariate_ts_object(ts_object_train)
  
  # Return results if no forecast is possible (due to missing xreg_cols)
  if (available_periods_ahead == 0) {
    return(fc_models)
  }
  
  #### LINEAR forecast models ####
  if ("linear" %in% fc_methods) {
    # Add linear models with trend (and seasonality)
    fc_models <- fc_models %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_linear_trend_xreg",
        fc_formula = paste0("forecast::tslm(col_of_interest ~ trend + ", paste0(xreg_cols, collapse = " + "), ", data = ts_object_train)"),
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_linear_trend_seasonal_xreg",
        fc_formula = paste0("forecast::tslm(col_of_interest ~ trend + season + ", paste0(xreg_cols, collapse = " + "), ", data = ts_object_train)"),
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### ARIMA forecast models ####
  if ("arima" %in% fc_methods) {
    # Add AutoRegressive Integrated Moving Average (ARIMA(p,d,q)(P,D,Q)m) models
    fc_models <- fc_models %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_arima_xreg",
        fc_formula = paste0("forecast::auto.arima(force_to_univariate_ts_object(ts_object_train), stepwise = T, approximation = T, xreg = ts_object_train[,c(",paste0("'", xreg_cols, "'", collapse = ","),")])"),
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_arima_stl_xreg",
        fc_formula = paste0("forecast::stlm(force_to_univariate_ts_object(ts_object_train), method = 'arima', xreg = ts_object_train[,c(",paste0("'", xreg_cols, "'", collapse = ","),")])"),
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### NEURAL NETWORK forecast models ####
  if ("nn" %in% fc_methods) {
    # Add Neural Network AutoRegression (NNAR(p,P,k)m) models
    fc_models <- fc_models %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_nn_5n_0decay_xreg",
        fc_formula = paste0("forecast::nnetar(force_to_univariate_ts_object(ts_object_train), size = 5, xreg = ts_object_train[,c(",paste0("'", xreg_cols ,"'", collapse = ","),")])"),
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_nn_25n_0decay_xreg",
        fc_formula = paste0("forecast::nnetar(force_to_univariate_ts_object(ts_object_train), size = 25, xreg = ts_object_train[,c(",paste0("'", xreg_cols ,"'", collapse = ","),")])"),
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_nn_5n_50decay_xreg",
        fc_formula = paste0("forecast::nnetar(force_to_univariate_ts_object(ts_object_train), size = 5, decay = 0.50, xreg = ts_object_train[,c(",paste0("'", xreg_cols ,"'", collapse = ","),")])"),
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_nn_25n_50decay_xreg",
        fc_formula = paste0("forecast::nnetar(force_to_univariate_ts_object(ts_object_train), size = 25, decay = 0.50, xreg = ts_object_train[,c(",paste0("'", xreg_cols ,"'", collapse = ","),")])"),
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### PROPHET forecast models ####
  if ("prophet" %in% fc_methods) {
    # Add prophet forecast models developed by Facebook
    fc_models <- fc_models %>% 
      add_prophet_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_prophet_005cps_xreg",
        model_type = "multivariate",
        changepoint_prior_scale = 0.005,
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_prophet_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_prophet_050cps_xreg",
        model_type = "multivariate",
        changepoint_prior_scale = 0.050,
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_prophet_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_prophet_500cps_xreg",
        model_type = "multivariate",
        changepoint_prior_scale = 0.500,
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### TREE forecast models ####
  if ("tree" %in% fc_methods) {
    # Add RPART and CTREE tree regressions
    fc_models <- fc_models %>% 
      add_rpart_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_rpart_xreg",
        model_type = "multivariate",
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_ctree_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_ctree_xreg",
        model_type = "multivariate",
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### RANDOM FOREST forecast models ####
  if ("forest" %in% fc_methods) {
    #Add randomForest
    fc_models <- fc_models %>% 
      add_randomforest_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_randomforest_xreg",
        model_type = "multivariate",
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        keep_fc_model_objects = keep_fc_model_objects,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### RECURSIVE SUPPORT VECTOR MACHINE forecast models ####
  if ("recursive" %in% fc_methods & "svm" %in% fc_methods) {
    fc_models <- fc_models %>% 
      add_recursive_ml_forecast_model(
        caret_model_tag = "svmRadialSigma",
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_rec_svmradsig_xreg",
        model_type = "multivariate",
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### RECURSIVE TREE forecast models ####
  if ("recursive" %in% fc_methods & "tree" %in% fc_methods) {
    fc_models <- fc_models %>% 
      add_recursive_ml_forecast_model(
        caret_model_tag = "rpart",
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_rec_rpart_xreg",
        model_type = "multivariate",
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_recursive_ml_forecast_model(
        caret_model_tag = "ctree",
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_rec_ctree_xreg",
        model_type = "multivariate",
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### RECURSIVE RANDOM FOREST forecast models ####
  if ("recursive" %in% fc_methods & "forest" %in% fc_methods) {
    fc_models <- fc_models %>% 
      add_recursive_ml_forecast_model(
        caret_model_tag = "cforest",
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_rec_cforest_xreg",
        model_type = "multivariate",
        periods_ahead = available_periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  # Return results
  return(fc_models)
}