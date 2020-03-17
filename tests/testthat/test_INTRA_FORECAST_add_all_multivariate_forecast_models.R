
context("add_all_multivariate_forecast_models")

test_that("check add_all_multivariate_forecast_models with differing periods ahead", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  ts_object_train <- function_input %>%
    dplyr::slice(1:179) %>% 
    tstools::transform_data_to_ts_object()
  ts_object_valid <- function_input %>%
    dplyr::slice(180:191) %>% 
    tstools::transform_data_to_ts_object()
  expect_silent(
    fc_models <- add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      periods_ahead = 13,
      verbose = F
    )
  )
  expected_models <- c(
    "fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg", 
    "fc_arima_xreg", "fc_arima_stl_xreg", 
    "fc_nn_5n_0decay_xreg", "fc_nn_25n_0decay_xreg", "fc_nn_5n_50decay_xreg", "fc_nn_25n_50decay_xreg", 
    "fc_prophet_005cps_xreg", "fc_prophet_050cps_xreg", "fc_prophet_500cps_xreg", 
    "fc_rpart_xreg", "fc_ctree_xreg", "fc_randomforest_xreg",
    "fc_rec_svmradsig_xreg", "fc_rec_rpart_xreg", "fc_rec_ctree_xreg", "fc_rec_cforest_xreg"
  )
  expected_model_classes <- c(
    "list", "character", "numeric"
  )
  expect_true(is.list(fc_models))
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_true(all(class(fc_models[[exp_model]]$model) %in% expected_model_classes))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200511)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200512)
    expect_equal(fc_models[[exp_model]]$fc_data$period[12], 200611)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
  }
  capture.output(
    fc_models <- add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_methods = c("linear"),
      periods_ahead = 12,
      verbose = T
    ),
    file = 'NUL'
  )
  expected_models <- c("fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg")
  expected_model_classes <- c("numeric")
  expect_true(is.list(fc_models))
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_true(all(class(fc_models[[exp_model]]$model) %in% expected_model_classes))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200511)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200512)
    expect_equal(fc_models[[exp_model]]$fc_data$period[12], 200611)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
  }
})

test_that("check add_all_multivariate_forecast_models when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  ts_object_train <- function_input %>% 
    dplyr::slice(1:179) %>% 
    tstools::transform_data_to_ts_object()
  ts_object_valid <- function_input %>% 
    dplyr::slice(180:191) %>% 
    tstools::transform_data_to_ts_object()
  function_input_invalid <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  train_invalid <- function_input_invalid %>% 
    dplyr::slice(1:179) %>% 
    tstools::transform_data_to_ts_object()
  valid_invalid <- function_input_invalid %>% 
    dplyr::slice(180:191) %>% 
    tstools::transform_data_to_ts_object()
  function_input_one_xreg <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = "spotprice"
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  train_one_xreg <- function_input_one_xreg %>% 
    dplyr::slice(1:179) %>% 
    tstools::transform_data_to_ts_object()
  valid_one_xreg <- function_input_one_xreg %>% 
    dplyr::slice(180:191) %>% 
    tstools::transform_data_to_ts_object()
  function_input_other_xreg <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = "gemprice"
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  train_other_xreg <- function_input_other_xreg %>% 
    dplyr::slice(1:179) %>% 
    tstools::transform_data_to_ts_object()
  valid_other_xreg <- function_input_other_xreg %>% 
    dplyr::slice(180:191) %>% 
    tstools::transform_data_to_ts_object()
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = dummy_gasprice,
      ts_object_valid = ts_object_valid,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = train_invalid,
      ts_object_valid = ts_object_valid,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = dummy_gasprice,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_models = data.frame(),
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_models = dummy_gasprice,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_models = ts_object_train,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      periods_ahead = -42
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      periods_ahead = 4.2
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      periods_history = -3
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      periods_ahead = 12,
      fc_methods = c("forest", "tree", "it's", "a", "kind", "of", "magic")
    )
  )
  expect_error(
    add_all_mulivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      periods_ahead = 12,
      fc_methods = list()
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = valid_invalid,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = train_invalid,
      ts_object_valid = ts_object_valid,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = train_invalid,
      ts_object_valid = valid_invalid,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = ts_object_train,
      ts_object_valid = valid_one_xreg,
      periods_ahead = 12
    )
  )
  expect_error(
    add_all_multivariate_forecast_models(
      ts_object_train = train_one_xreg,
      ts_object_valid = valid_other_xreg,
      periods_ahead = 12
    )
  )
})
