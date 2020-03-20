
context("add_kalman_forecast_model")

test_that("check add_kalman_forecast_model with different periods_ahead", {
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
      add_kalman_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_kalman_poly",
        periods_ahead = 12,
        periods_history = 12,
        verbose = F
      ) %>% 
      add_kalman_forecast_model(
        ts_object_train = function_input,
        fc_name = "fc_kalman_seas_12",
        periods_ahead = 12,
        periods_history = 12,
        verbose = F
      )
  )
  expect_true(is.list(fc_models))
  expect_equal(names(fc_models), c("fc_kalman_poly", "fc_kalman_seas_12"))
  expect_equal(names(fc_models$fc_kalman_poly), c("model", "fc_data"))
  expect_equal(names(fc_models$fc_kalman_seas_12), c("model", "fc_data"))
  expect_true(is.data.frame(fc_models$fc_kalman_poly$fc_data))
  expect_true(is.data.frame(fc_models$fc_kalman_seas_12$fc_data))
  expect_equal(class(fc_models$fc_kalman_poly$model), "dlm")
  expect_equal(class(fc_models$fc_kalman_seas_12$model), "dlm")
  expect_equal(nrow(fc_models$fc_kalman_poly$fc_data), 12)
  expect_equal(ncol(fc_models$fc_kalman_poly$fc_data), 3)
  expect_equal(unique(fc_models$fc_kalman_poly$fc_data$fc_date), 200611)
  expect_equal(fc_models$fc_kalman_poly$fc_data$period[1], 200612)
  expect_equal(nrow(fc_models$fc_kalman_seas_12$fc_data), 12)
  expect_equal(ncol(fc_models$fc_kalman_seas_12$fc_data), 3)
  expect_equal(unique(fc_models$fc_kalman_seas_12$fc_data$fc_date), 200611)
  expect_equal(fc_models$fc_kalman_seas_12$fc_data$period[1], 200612)
  expect_equal(length(fc_models$fc_kalman_seas_12$fc_data$period), 12)
})

test_that("check add_kalman_forecast_model when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object()
  expect_error(
    add_kalman_forecast_model(
      fc_models = data.frame()
    ),
    "Object 'fc_models' is of class data.frame"
  )
  expect_error(
    add_kalman_forecast_model(
      fc_models = dummy_gasprice
    ),
    "Object 'fc_models' is of class tbl_df/tbl/data.frame"
  )
  expect_error(
    add_kalman_forecast_model(
      fc_models = function_input
    ),
    "Object 'fc_models' is of class msts/ts"
  )
  expect_error(
    add_kalman_forecast_model(
      fc_models = list(),
      ts_object_train = dummy_gasprice
    ),
    "Object 'ts_object_train' is of class tbl_df/tbl/data.frame"
  )
  expect_error(
    add_kalman_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = 42
    ),
    "Parameter 'fc_name' is of class numeric"
  )
  expect_error(
    add_kalman_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = "fc_kalman_poly",
      periods_ahead = -42
    ),
    "The parameter 'periods_ahead' should be a positive integer value, instead of '-42'"
  )
  expect_error(
    add_kalman_forecast_model(
      fc_models = list(),
      ts_object_train = function_input,
      fc_name = "fc_kalman_poly",
      periods_ahead = 4.2
    ),
    "The parameter 'periods_ahead' should be a positive integer value, instead of '4.2'"
  )
})
