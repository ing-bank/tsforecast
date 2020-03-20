#' Add all univariate forecast models
#' 
#' \code{add_all_univariate_forecast_models} is a wrapper function to add 
#' multiple univariate forecast models to a (named) list of forecast models. The
#' forecast models are created based on multiple calls of the 
#' \code{add_univariate_forecast_model} function with specific sets of 
#' parameters, which are each used to forecast a specific number of periods 
#' ahead. The fc_methods parameter can be used to control which forecast models 
#' are added.
#' 
#' @param ts_object_train A time series object, which contains only the training
#'   data.
#' @param fc_models A named list of forecast models, with for each forecast 
#'   model a list with the model itself and a table with forecast values.
#' @param periods_ahead A positive integer value indicating the number of
#'   periods to forecast ahead.
#' @param periods_history A positive integer value indicating the number of
#'   historic datapoints to use for training, which is only relevant for
#'   specific forecast methods such as drift and mean.
#' @param fc_methods A character vector specifying the forecast methods to add. 
#' For more info \code{`?supported_fc_methods`}.
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
#' @import forecastHybrid
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
add_all_univariate_forecast_models <- function(ts_object_train, fc_models = list(), 
                                               periods_ahead = 1, periods_history = Inf, 
                                               fc_methods = supported_fc_methods_uni_var(),
                                               verbose = FALSE, parallel = FALSE) {
  # Unlist ts_object_train if required
  ts_object_train <- tstools::unlist_if_required(ts_object_train)
  # Check to make sure ts_object_train is a times series object
  if (!is.ts(ts_object_train)) stop(paste0("Object 'ts_object_train' is of class ",paste0(class(ts_object_train), collapse = "/")," ... \n\n Put in a time series object!"))
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
  invalid_fc_methods <- fc_methods[!fc_methods %in% supported_fc_methods_uni_var()]
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
  
  # Force to be univariate ts object
  ts_object_train <- force_to_univariate_ts_object(ts_object_train)
  
  #### BASIC forecast models ####
  if ("basic" %in% fc_methods) {
    # Set of limits for historic periods to use
    last_X_months <- c(3, 6, 12)
    # Add mean forecast models
    for (lXm in last_X_months) {
      fc_models <- fc_models %>% 
        add_univariate_forecast_model(
          ts_object_train = ts_object_train,
          fc_name = paste0("fc_mean_l",lXm,"m"),
          fc_formula = "forecast::meanf(x, h)",
          periods_ahead = periods_ahead,
          periods_history = lXm,
          verbose = verbose,
          log_message = log_message
        )
    }
    # Add drift forecast models
    for (lXm in last_X_months) {
      fc_models <- fc_models %>% 
        add_univariate_forecast_model(
          ts_object_train = ts_object_train,
          fc_name = paste0("fc_drift_l",lXm,"m"),
          fc_formula = "forecast::rwf(x, h, drift = TRUE)",
          periods_ahead = periods_ahead,
          periods_history = lXm,
          verbose = verbose,
          log_message = log_message
        )
    }
    # Add (seasonal) naive forecast models
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_naive",
        fc_formula = "forecast::naive(x, h)",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_naive_seasonal",
        fc_formula = "forecast::snaive(x, h)",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### LINEAR forecast models ####
  if ("linear" %in% fc_methods) {
    # Add linear models with trend (and seasonality)
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_linear_trend",
        fc_formula = "forecast::tslm(x ~ trend)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_linear_trend_seasonal",
        fc_formula = "forecast::tslm(x ~ trend + season)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }

  #### HOLT-WINTERS forecast models ####
  if ("holt_winters" %in% fc_methods) {
    # Add Holt-Winters filtering models
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_holt_winters_addiv",
        fc_formula = "stats::HoltWinters(x, seasonal = 'additive')",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_holt_winters_multip",
        fc_formula = "stats::HoltWinters(x, seasonal = 'multiplicative')",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### (T)BATS forecast models ####
  if ("bats" %in% fc_methods) {
    # Add Exponential smoothing state space models with Box-Cox transformation, ARMA errors, Trend and Seasonal components
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_bats",
        fc_formula = "forecast::bats(x, stepwise = T, approximation = T)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_tbats",
        fc_formula = "forecast::tbats(x, stepwise = T, approximation = T)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### ETS forecast models ####
  if ("ets" %in% fc_methods) {
    # Add ExponenTial Smoothing (Error,Trend,Seasonal) models
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_ets_addiv",
        fc_formula = "forecast::ets(x, additive.only = T, allow.multiplicative.trend = F)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_ets_multip",
        fc_formula = "forecast::ets(x, additive.only = F, allow.multiplicative.trend = T)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_ets_addiv_damped",
        fc_formula = "forecast::ets(x, additive.only = T, allow.multiplicative.trend = F, damped = T)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_ets_multip_damped",
        fc_formula = "forecast::ets(x, additive.only = F, allow.multiplicative.trend = T, damped = T)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_ets_stl",
        fc_formula = "forecast::stlm(x, method = 'ets')",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
    
  #### ARIMA forecast models ####
  if ("arima" %in% fc_methods) {
    # Add AutoRegressive Integrated Moving Average (ARIMA(p,d,q)(P,D,Q)m) models
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_arima",
        fc_formula = "forecast::auto.arima(x, stepwise = T, approximation = T)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_arima_stl",
        fc_formula = "forecast::stlm(x, method = 'arima')",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }

  #### NEURAL NETWORK forecast models ####
  if ("nn" %in% fc_methods) {
    # Add Neural Network AutoRegression (NNAR(p,P,k)m) models
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_nn_5n_0decay",
        fc_formula = "forecast::nnetar(x, size = 5)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_nn_25n_0decay",
        fc_formula = "forecast::nnetar(x, size = 25)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_nn_5n_50decay",
        fc_formula = "forecast::nnetar(x, size = 5, decay = 0.50)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_nn_25n_50decay",
        fc_formula = "forecast::nnetar(x, size = 25, decay = 0.50)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_nn_5n_mlp",
        fc_formula = "nnfor::mlp(x, hd = 5, reps = 10)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_nn_25n_elm",
        fc_formula = "nnfor::elm(x, hd = 25, reps = 10)",
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
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
        fc_name = "fc_prophet_005cps",
        model_type = "univariate",
        changepoint_prior_scale = 0.005,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_prophet_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_prophet_050cps",
        model_type = "univariate",
        changepoint_prior_scale = 0.050,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_prophet_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_prophet_500cps",
        model_type = "univariate",
        changepoint_prior_scale = 0.500,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
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
        fc_name = "fc_rpart",
        model_type = "univariate",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_ctree_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_ctree",
        model_type = "univariate",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
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
        fc_name = "fc_randomforest",
        model_type = "univariate",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### ENSEMBLE forecast models ####
  if ("ensemble" %in% fc_methods) {
    # Add ensemble of econometric models
    formula = paste0(
      "forecastHybrid::hybridModel(",
        "x, model = 'aefnst', lambda = 'auto', verbose = FALSE,",
        "a.args = list(stepwise = T, approximation = T),",
        "e.args = list(additive.only = T, allow.multiplicative.trend = F),",
        "n.args = list(size = 5),",
        "t.args = list(stepwise = T, approximation = T)",
      ")"
    )
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_ensemble_aefnst",
        fc_formula = formula,
        model_fc = TRUE,
        periods_ahead = periods_ahead,
        periods_history = periods_history,
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
        fc_name = "fc_rec_svmradsig",
        model_type = "univariate",
        periods_ahead = periods_ahead,
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
        fc_name = "fc_rec_rpart",
        model_type = "univariate",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>% 
      add_recursive_ml_forecast_model(
        caret_model_tag = "ctree",
        ts_object_train = ts_object_train,
        fc_name = "fc_rec_ctree",
        model_type = "univariate",
        periods_ahead = periods_ahead,
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
        fc_name = "fc_rec_cforest",
        model_type = "univariate",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  #### KALMAN FILTER forecast models ####
  if ("kalman" %in% fc_methods) {
    # Add Kalman Filter model
    fc_models <- fc_models %>%
      add_kalman_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_kalman_poly",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      ) %>%
      add_kalman_forecast_model(
        ts_object_train = ts_object_train,
        fc_name = "fc_kalman_seas_12",
        periods_ahead = periods_ahead,
        periods_history = periods_history,
        verbose = verbose,
        log_message = log_message
      )
  }

  #### FFORMA meta model ####
  if ("fforma" %in% fc_methods && length(fc_models) > 1) {
    # Add FFORMA meta model
    fc_models <- fc_models %>% 
      apply_fforma_model(
        ts_object_train = ts_object_train,
        verbose = verbose,
        log_message = log_message
      )
  }
  
  # Return results
  return(fc_models)
}