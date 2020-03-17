
context("apply_fforma_model")

test_that("check apply_fforma_model with univariate forecast models", {
  ts_object_train <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object()
  fc_models <- add_all_univariate_forecast_models(
    ts_object_train = ts_object_train,
    periods_ahead = 12,
    fc_methods = c("basic", "linear", "kalman")
  )
  function_output <- apply_fforma_model(
    fc_models = fc_models,
    ts_object_train = ts_object_train
  )
  expect_equal(class(function_output), "list")
  expect_equal(length(function_output), 13)
  expected_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_drift_l3m", 
    "fc_drift_l6m", "fc_drift_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_kalman_poly", 
    "fc_kalman_seas_12", "fc_fforma"
  )
  expect_equal(names(function_output), expected_models)
  for (model in expected_models) {
    expect_equal(class(function_output[[model]]), "list")
    expect_equal(length(function_output[[model]]), 2)
    expect_true(all(class(function_output[[model]]$model) %in% c(
      "meanf", "lagwalk", "numeric", "dlm", "tbl_df", "tbl", "data.frame"
    )))
    expect_equal(class(function_output[[model]]$fc_data), c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(function_output[[model]]$fc_data), 12)
    expect_equal(ncol(function_output[[model]]$fc_data), 3)
    expect_equal(colnames(function_output[[model]]$fc_data), c("fc_date", "period", "fc_value"))
    expect_equal(unique(function_output[[model]]$fc_data$fc_date), 200611)
    expect_equal(unique(function_output[[model]]$fc_data$period), c(200612, 200701:200711))
    expect_equal(sum(is.na(function_output[[model]]$fc_data$fc_value)), 0)
  }
  
  # Try to add again
  function_output <- apply_fforma_model(
    fc_models = fc_models,
    ts_object_train = ts_object_train
  )
  expect_equal(class(function_output), "list")
  expect_equal(length(function_output), 13)
  expected_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_drift_l3m", 
    "fc_drift_l6m", "fc_drift_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_kalman_poly", 
    "fc_kalman_seas_12", "fc_fforma"
  )
  expect_equal(names(function_output), expected_models)
  for (model in expected_models) {
    expect_equal(class(function_output[[model]]), "list")
    expect_equal(length(function_output[[model]]), 2)
    expect_true(all(class(function_output[[model]]$model) %in% c(
      "meanf", "lagwalk", "numeric", "dlm", "tbl_df", "tbl", "data.frame"
    )))
    expect_equal(class(function_output[[model]]$fc_data), c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(function_output[[model]]$fc_data), 12)
    expect_equal(ncol(function_output[[model]]$fc_data), 3)
    expect_equal(colnames(function_output[[model]]$fc_data), c("fc_date", "period", "fc_value"))
    expect_equal(unique(function_output[[model]]$fc_data$fc_date), 200611)
    expect_equal(unique(function_output[[model]]$fc_data$period), c(200612, 200701:200711))
    expect_equal(sum(is.na(function_output[[model]]$fc_data$fc_value)), 0)
  }
  
  # Try to add empty
  function_output <- apply_fforma_model(
    fc_models = list(),
    ts_object_train = ts_object_train
  )
  expect_equal(class(function_output), "list")
  expect_equal(length(function_output), 0)
  
  fc_models <- add_all_univariate_forecast_models(
    ts_object_train = ts_object_train,
    periods_ahead = 12,
    fc_methods = c("ensemble")
  )
  function_output <- apply_fforma_model(
    fc_models = fc_models,
    ts_object_train = ts_object_train
  )
  expect_equal(class(function_output), "list")
  expect_equal(length(function_output), 2)
  expect_equal(names(function_output), c("fc_ensemble_aefnst", "fc_fforma"))
  expect_equal(
    function_output$fc_ensemble_aefnst$fc_data,
    function_output$fc_fforma$fc_data,
  )
})

test_that("check apply_fforma_model with invalid inputs", {
  expect_error(
    apply_fforma_model(
      fc_models = "potato"
    )
  )
  expect_error(
    apply_fforma_model(
      fc_models = 42
    )
  )
  expect_error(
    apply_fforma_model(
      fc_models = list(),
      ts_object_train = "potato"
    )
  )
  expect_error(
    apply_fforma_model(
      fc_models = list(),
      ts_object_train = 42
    )
  )
})
