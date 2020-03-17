
context("add_all_univariate_forecast_models")

test_that("check add_all_univariate_forecast_models with different periods_ahead", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object()
  expect_silent(
    fc_models <- add_all_univariate_forecast_models(
      ts_object_train = function_input,
      periods_ahead = 12,
      verbose = F
    )
  )
  expected_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", 
    "fc_drift_l3m", "fc_drift_l6m", "fc_drift_l12m", 
    "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal",
    "fc_holt_winters_addiv", "fc_holt_winters_multip", 
    "fc_bats", "fc_tbats", 
    "fc_ets_addiv", "fc_ets_multip", "fc_ets_addiv_damped", "fc_ets_multip_damped", "fc_ets_stl", 
    "fc_arima", "fc_arima_stl", 
    "fc_nn_5n_0decay", "fc_nn_25n_0decay", "fc_nn_5n_50decay", 
    "fc_nn_25n_50decay", "fc_nn_5n_mlp", "fc_nn_25n_elm",
    "fc_prophet_005cps", "fc_prophet_050cps", "fc_prophet_500cps", 
    "fc_rpart", "fc_ctree", "fc_randomforest",
    "fc_ensemble_aefnst", "fc_rec_svmradsig",
    "fc_rec_rpart", "fc_rec_ctree", "fc_rec_cforest",
    "fc_kalman_poly", "fc_kalman_seas_12",
    "fc_fforma"
  )
  expected_model_classes <- c(
    "meanf", "lagwalk", "naive", "dlm", "HoltWinters",
    "bats", "ets", "ARIMA", "Arima", 
    "list", "character", "numeric",
    "tbl_df", "tbl", "data.frame"
  )
  expect_true(is.list(fc_models))
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_true(all(class(fc_models[[exp_model]]$model) %in% expected_model_classes))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200611)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200612)
    expect_equal(fc_models[[exp_model]]$fc_data$period[12], 200711)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
  }
  capture.output(
    fc_models <- add_all_univariate_forecast_models(
      ts_object_train = function_input,
      fc_methods = c("basic"),
      periods_ahead = 12,
      verbose = T
    ),
    file = 'NUL'
  )
  expected_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", 
    "fc_drift_l3m", "fc_drift_l6m", "fc_drift_l12m", 
    "fc_naive", "fc_naive_seasonal"
  )
  expected_model_classes <- c(
    "meanf", "lagwalk", "naive", "mumeric"
  )
  expect_true(is.list(fc_models))
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_true(all(class(fc_models[[exp_model]]$model) %in% expected_model_classes))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200611)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200612)
    expect_equal(fc_models[[exp_model]]$fc_data$period[12], 200711)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
  }
})

test_that("check add_all_univariate_forecast_models when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object()
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = dummy_gasprice,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = function_input,
      fc_models = data.frame(),
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = function_input,
      fc_models = dummy_gasprice,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = function_input,
      fc_models = function_input,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = function_input,
      periods_ahead = -42
    )
  )
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = function_input,
      periods_ahead = 4.2
    )
  )
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = function_input,
      periods_history = -3
    )
  )
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = function_input,
      periods_ahead = 12,
      fc_methods = c("basic", "linear", "prophet", "it's", "a", "kind", "of", "magic")
    )
  )
  expect_error(
    add_all_univariate_forecast_models(
      ts_object_train = function_input,
      periods_ahead = 12,
      fc_methods = list()
    )
  )
})
