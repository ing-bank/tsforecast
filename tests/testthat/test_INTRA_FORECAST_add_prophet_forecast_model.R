
context("add_prophet_forecast_model")

test_that("check add_prophet_forecast_model for univariate models with different periods_ahead", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object()
  capture.output(
    fc_models <- list() %>% 
      add_prophet_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_prophet_005cps",
        changepoint_prior_scale = 0.005,
        periods_ahead = 1,
        model_type = "univariate",
        verbose = T
      ) %>% 
      add_prophet_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_prophet_005cps",
        changepoint_prior_scale = 0.005,
        periods_ahead = 1,
        model_type = "univariate",
        verbose = T
      ) %>% 
      add_prophet_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_prophet_050cps",
        changepoint_prior_scale = 0.050,
        periods_ahead = 12,
        model_type = "univariate",
        verbose = T
      ) %>% 
      add_prophet_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_prophet_500cps",
        changepoint_prior_scale = 0.500,
        periods_ahead = 1000,
        model_type = "univariate",
        verbose = T
      ),
    file = 'NUL'
  )
  expect_true(is.list(fc_models))
  expect_equal(names(fc_models), c("fc_prophet_005cps", "fc_prophet_050cps", "fc_prophet_500cps"))
  expect_equal(names(fc_models$fc_prophet_005cps), c("model", "fc_data"))
  expect_equal(names(fc_models$fc_prophet_050cps), c("model", "fc_data"))
  expect_equal(names(fc_models$fc_prophet_500cps), c("model", "fc_data"))
  expect_true(is.data.frame(fc_models$fc_prophet_005cps$fc_data))
  expect_true(is.data.frame(fc_models$fc_prophet_050cps$fc_data))
  expect_true(is.data.frame(fc_models$fc_prophet_500cps$fc_data))
  expect_equal(class(fc_models$fc_prophet_005cps$model), "list")
  expect_equal(class(fc_models$fc_prophet_050cps$model), "list")
  expect_equal(class(fc_models$fc_prophet_500cps$model), "list")
  expect_equal(nrow(fc_models$fc_prophet_005cps$fc_data), 1)
  expect_equal(ncol(fc_models$fc_prophet_005cps$fc_data), 3)
  expect_equal(fc_models$fc_prophet_005cps$fc_data$fc_date, 200611)
  expect_equal(fc_models$fc_prophet_005cps$fc_data$period, 200612)
  expect_equal(nrow(fc_models$fc_prophet_050cps$fc_data), 12)
  expect_equal(ncol(fc_models$fc_prophet_050cps$fc_data), 3)
  expect_equal(unique(fc_models$fc_prophet_050cps$fc_data$fc_date), 200611)
  expect_equal(fc_models$fc_prophet_050cps$fc_data$period[1], 200612)
  expect_equal(length(fc_models$fc_prophet_050cps$fc_data$period), 12)
  expect_equal(nrow(fc_models$fc_prophet_500cps$fc_data), 1000)
  expect_equal(ncol(fc_models$fc_prophet_500cps$fc_data), 3)
  expect_equal(unique(fc_models$fc_prophet_500cps$fc_data$fc_date), 200611)
  expect_equal(fc_models$fc_prophet_500cps$fc_data$period[1], 200612)
  expect_equal(length(fc_models$fc_prophet_500cps$fc_data$period), 1000)
})

test_that("check add_prophet_forecast_model for multivariate models with different periods_ahead", {
  xregs <- list("spotprice", "gemprice", c("spotprice", "gemprice"))
  for (xreg_cols in xregs) {
      function_input <- tstools::initialize_ts_forecast_data(
        data = dummy_gasprice,
        date_col = "year_month",
        col_of_interest = "gasprice",
        group_cols = c("state", "oil_company"),
        xreg_cols = xreg_cols
      ) %>% 
      dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
    ts_object_train <- function_input %>% 
      dplyr::slice(1:189) %>% 
      tstools::transform_data_to_ts_object()
    ts_object_valid <- function_input %>% 
      dplyr::slice(190:191) %>% 
      tstools::transform_data_to_ts_object()
    expect_silent(
      function_output <- list() %>% 
        add_prophet_forecast_model(
          ts_object_train = ts_object_train,
          ts_object_valid = ts_object_valid,
          fc_name = "fc_prophet_005cps",
          changepoint_prior_scale = 0.005,
          periods_ahead = 2,
          model_type = "multivariate",
          verbose = F
        ) %>% 
        add_prophet_forecast_model(
          ts_object_train = ts_object_train,
          ts_object_valid = ts_object_valid,
          fc_name = "fc_prophet_050cps",
          changepoint_prior_scale = 0.050,
          periods_ahead = 2,
          model_type = "multivariate",
          verbose = F
        ) %>% 
        add_prophet_forecast_model(
          ts_object_train = ts_object_train,
          ts_object_valid = ts_object_valid,
          fc_name = "fc_prophet_500cps",
          changepoint_prior_scale = 0.500,
          periods_ahead = 2,
          model_type = "multivariate",
          verbose = F
        )
    )
    expected_models <- c("fc_prophet_005cps", "fc_prophet_050cps", "fc_prophet_500cps")
    expect_true(is.list(function_output))
    expect_equal(names(function_output), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(function_output[[exp_model]]), c("model", "fc_data"))
      expect_equal(class(function_output[[exp_model]]$model), "list")
      expect_equal(nrow(function_output[[exp_model]]$fc_data), 2)
      expect_equal(ncol(function_output[[exp_model]]$fc_data), 3)
      expect_equal(function_output[[exp_model]]$fc_data$period, c(200610, 200611))
      expect_equal(function_output[[exp_model]]$fc_data$fc_date %>% unique(), 200609)
    }
  }
})

test_that("check add_prophet_forecast_model for univariate models with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  ts_object_train <- function_input %>% 
    dplyr::slice(1:189) %>% 
    tstools::transform_data_to_ts_object()
  ts_object_valid <- function_input %>% 
    dplyr::slice(190:191) %>% 
    tstools::transform_data_to_ts_object()
  expect_error(
    add_prophet_forecast_model(
      fc_models = "potato"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = dummy_gasprice,
      periods_ahead = 12,
      fc_name = "fc_prophet_050cps",
      changepoint_prior_scale = 0.050,
      model_type = "univariate"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = dummy_gasprice,
      periods_ahead = 12,
      fc_name = "fc_prophet_050cps",
      changepoint_prior_scale = 0.050,
      model_type = "multivariate"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 2,
      fc_name = 42,
      changepoint_prior_scale = 0.050,
      model_type = "univariate"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 12,
      fc_name = "fc_prophet_050cps",
      changepoint_prior_scale = 0.050,
      model_type = "omnivariate"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 12,
      fc_name = "fc_prophet_050cps",
      changepoint_prior_scale = 0.050,
      model_type = "multivariate"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      periods_ahead = 12,
      fc_name = "fc_prophet_050cps",
      changepoint_prior_scale = 0.050,
      model_type = "multivariate"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 12,
      fc_name = "fc_prophet_050cps",
      changepoint_prior_scale = -0.050,
      model_type = "univariate"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = -3,
      fc_name = "fc_prophet_050cps",
      changepoint_prior_scale = 0.050,
      model_type = "univariate"
    )
  )
  expect_error(
    add_prophet_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 12,
      periods_history = -3,
      fc_name = "fc_prophet_050cps",
      changepoint_prior_scale = 0.050,
      model_type = "univariate"
    )
  )
})
