
context("add_univariate_forecast_model")

test_that("check add_univariate_forecast_model with different periods_ahead", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object()
  expect_silent(
    fc_models <- list() %>% 
      add_univariate_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_average_l3m",
        fc_formula = "forecast::meanf(x, h)",
        periods_ahead = 1,
        periods_history = 3,
        verbose = F
      ) %>% 
      add_univariate_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_drift_l6m",
        fc_formula = "forecast::rwf(x, h, drift = TRUE)",
        periods_ahead = 12,
        periods_history = 6,
        verbose = F
      )
  )
  capture.output(
    fc_models <- fc_models %>% 
      add_univariate_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_snaive",
        fc_formula = "forecast::snaive(x, h)",
        periods_ahead = 1000,
        verbose = T
      ),
    file = 'NUL'
  )
  expect_true(is.list(fc_models))
  expect_equal(names(fc_models), c("fc_average_l3m", "fc_drift_l6m", "fc_snaive"))
  expect_equal(names(fc_models$fc_average_l3m), c("model", "fc_data"))
  expect_equal(names(fc_models$fc_drift_l6m), c("model", "fc_data"))
  expect_equal(names(fc_models$fc_snaive), c("model", "fc_data"))
  expect_true(is.data.frame(fc_models$fc_average_l3m$fc_data))
  expect_true(is.data.frame(fc_models$fc_drift_l6m$fc_data))
  expect_true(is.data.frame(fc_models$fc_snaive$fc_data))
  expect_equal(class(fc_models$fc_average_l3m$model), "meanf")
  expect_equal(class(fc_models$fc_drift_l6m$model), "lagwalk")
  expect_equal(class(fc_models$fc_snaive$model), "lagwalk")
  expect_equal(nrow(fc_models$fc_average_l3m$fc_data), 1)
  expect_equal(ncol(fc_models$fc_average_l3m$fc_data), 3)
  expect_equal(fc_models$fc_average_l3m$fc_data$fc_date, 200611)
  expect_equal(fc_models$fc_average_l3m$fc_data$period, 200612)
  expect_equal(nrow(fc_models$fc_drift_l6m$fc_data), 12)
  expect_equal(ncol(fc_models$fc_drift_l6m$fc_data), 3)
  expect_equal(unique(fc_models$fc_drift_l6m$fc_data$fc_date), 200611)
  expect_equal(fc_models$fc_drift_l6m$fc_data$period[1], 200612)
  expect_equal(length(fc_models$fc_drift_l6m$fc_data$period), 12)
  expect_equal(nrow(fc_models$fc_snaive$fc_data), 1000)
  expect_equal(ncol(fc_models$fc_snaive$fc_data), 3)
  expect_equal(unique(fc_models$fc_snaive$fc_data$fc_date), 200611)
  expect_equal(fc_models$fc_snaive$fc_data$period[1], 200612)
  expect_equal(length(fc_models$fc_snaive$fc_data$period), 1000)
})

test_that("check add_univariate_forecast_model when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object()
  expect_error(
    add_univariate_forecast_model(
      fc_models = data.frame(),
      ts_object_train = function_input,
      fc_name = "fc_average_l3m",
      fc_formula = "forecast::meanf(x, h)",
      periods_ahead = 1,
      periods_history = 3
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = dummy_gasprice,
      ts_object_train = function_input,
      fc_name = "fc_average_l3m",
      fc_formula = "forecast::meanf(x, h)",
      periods_ahead = 1,
      periods_history = 3
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = function_input,
      ts_object_train = function_input,
      fc_name = "fc_average_l3m",
      fc_formula = "forecast::meanf(x, h)",
      periods_ahead = 1,
      periods_history = 3
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = list(),
      ts_object_train = dummy_gasprice,
      fc_name = "fc_average_l3m",
      fc_formula = "forecast::meanf(x, h)",
      periods_ahead = 1,
      periods_history = 3
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = 42,
      fc_formula = "forecast::meanf(x, h)"
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = "fc_average_l3m",
      fc_formula = 42
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = "fc_average_l3m",
      fc_formula = "forecast::meanf(x, h)",
      periods_ahead = -42
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = "fc_average_l3m",
      fc_formula = "forecast::meanf(x, h)",
      periods_ahead = 4.2
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = "fc_average_l3m",
      fc_formula = "forecast::meanf(x, h)",
      periods_history = -42
    )
  )
  expect_error(
    add_univariate_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = "fc_average_l3m",
      fc_formula = "forecast::meanf(x, h)",
      periods_history = 4.2
    )
  )
})
