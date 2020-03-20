
context("calculate_forecast_errors")

test_that("check calculate_forecast_errors with negative forecasts not allowed", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object() %>% 
    split_ts_object(ts_split_date = 200301)
  function_output <- calculate_forecast_errors(
    fc_models = add_all_univariate_forecast_models(
      ts_object_train = function_input$train,
      periods_ahead = 36,
      fc_methods = c("basic", "linear", "prophet")
    ),
    ts_object_train = function_input$train,
    ts_object_valid = function_input$valid
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 468)
  expect_equal(ncol(function_output), 9)
  expect_equal(colnames(function_output),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$fc_date), "numeric")
  expect_equal(class(function_output$period), "numeric")
  expect_equal(class(function_output$fc_periods_ahead), "numeric")
  expect_equal(class(function_output$fc_value), "numeric")
  expect_equal(class(function_output$actual), "numeric")
  expect_equal(class(function_output$fc_error), "numeric")
  expect_equal(class(function_output$MASE), "numeric")
  expect_equal(unique(function_output$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(function_output$fc_model), c(
    "fc_drift_l12m", "fc_drift_l3m", "fc_drift_l6m", 
    "fc_linear_trend", "fc_linear_trend_seasonal", 
    "fc_mean_l12m", "fc_mean_l3m", "fc_mean_l6m", 
    "fc_naive", "fc_naive_seasonal", 
    "fc_prophet_005cps", "fc_prophet_050cps", "fc_prophet_500cps"
  ))
  expect_equal(unique(function_output$fc_date), 200301)
  expect_equal(length(unique(function_output$period)), 36)
  expect_equal(min(function_output$period), 200302)
  expect_equal(max(function_output$period), 200601)
  expect_equal(unique(function_output$fc_periods_ahead), 1:36)
  expect_equal(round(min(function_output$fc_value),2), 0.00)
  expect_equal(round(max(function_output$fc_value),2), 3.44)
  expect_equal(round(min(function_output$actual),2), 1.33)
  expect_equal(round(max(function_output$actual),2), 2.90)
  expect_equal(round(min(function_output$fc_error),2), -2.90)
  expect_equal(round(max(function_output$fc_error),2), 1.49)
  expect_equal(round(min(function_output$MASE),2), 0.00)
  expect_equal(round(max(function_output$MASE),2), 13.69)
})

test_that("check calculate_forecast_errors with negative forecasts allowed", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object() %>% 
    split_ts_object(ts_split_date = 200301)
  function_output <- calculate_forecast_errors(
    fc_models = add_all_univariate_forecast_models(
      ts_object_train = function_input$train,
      periods_ahead = 36,
      fc_methods = c("basic", "linear", "prophet")
    ),
    ts_object_train = function_input$train,
    ts_object_valid = function_input$valid,
    allow_negative_fc = T
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 468)
  expect_equal(ncol(function_output), 9)
  expect_equal(colnames(function_output),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$fc_date), "numeric")
  expect_equal(class(function_output$period), "numeric")
  expect_equal(class(function_output$fc_periods_ahead), "numeric")
  expect_equal(class(function_output$fc_value), "numeric")
  expect_equal(class(function_output$actual), "numeric")
  expect_equal(class(function_output$fc_error), "numeric")
  expect_equal(class(function_output$MASE), "numeric")
  expect_equal(unique(function_output$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(function_output$fc_model), c(
    "fc_drift_l12m", "fc_drift_l3m", "fc_drift_l6m", 
    "fc_linear_trend", "fc_linear_trend_seasonal", 
    "fc_mean_l12m", "fc_mean_l3m", "fc_mean_l6m", 
    "fc_naive", "fc_naive_seasonal", 
    "fc_prophet_005cps", "fc_prophet_050cps", "fc_prophet_500cps"
  ))
  expect_equal(unique(function_output$fc_date), 200301)
  expect_equal(length(unique(function_output$period)), 36)
  expect_equal(min(function_output$period), 200302)
  expect_equal(max(function_output$period), 200601)
  expect_equal(unique(function_output$fc_periods_ahead), 1:36)
  expect_equal(round(min(function_output$fc_value),2), -2.66)
  expect_equal(round(max(function_output$fc_value),2), 3.44)
  expect_equal(round(min(function_output$actual),2), 1.33)
  expect_equal(round(max(function_output$actual),2), 2.90)
  expect_equal(round(min(function_output$fc_error),2), -5.21)
  expect_equal(round(max(function_output$fc_error),2), 1.49)
  expect_equal(round(min(function_output$MASE),2), 0.00)
  expect_equal(round(max(function_output$MASE),2), 24.62)
})

test_that("check calculate_forecast_errors for edge case without validation data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object() %>% 
    split_ts_object(ts_split_date = 200611)
  function_output <- calculate_forecast_errors(
    fc_models = add_all_univariate_forecast_models(
      ts_object_train = function_input$train,
      periods_ahead = 36,
      fc_methods = c("basic", "linear", "prophet")
    ),
    ts_object_train = function_input$train,
    ts_object_valid = function_input$valid
  )
  expect_true(is.data.frame(function_output))
  expect_equal(nrow(function_output), 468)
  expect_equal(ncol(function_output), 9)
  expect_equal(colnames(function_output),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$fc_date), "numeric")
  expect_equal(class(function_output$period), "numeric")
  expect_equal(class(function_output$fc_periods_ahead), "numeric")
  expect_equal(class(function_output$fc_value), "numeric")
  expect_equal(class(function_output$actual), "numeric")
  expect_equal(class(function_output$fc_error), "numeric")
  expect_equal(unique(function_output$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(function_output$fc_model), c(
    "fc_drift_l12m", "fc_drift_l3m", "fc_drift_l6m", 
    "fc_linear_trend", "fc_linear_trend_seasonal", 
    "fc_mean_l12m", "fc_mean_l3m", "fc_mean_l6m", 
    "fc_naive", "fc_naive_seasonal", 
    "fc_prophet_005cps", "fc_prophet_050cps", "fc_prophet_500cps"
  ))
  expect_equal(unique(function_output$fc_date), 200611)
  expect_equal(length(unique(function_output$period)), 36)
  expect_equal(min(function_output$period), 200612)
  expect_equal(max(function_output$period), 200911)
  expect_equal(unique(function_output$fc_periods_ahead), 1:36)
  expect_equal(round(min(function_output$fc_value),2), 1.44)
  expect_equal(round(max(function_output$fc_value),2), 6.45)
  expect_true(is.na(min(function_output$actual)))
  expect_true(is.na(max(function_output$actual)))
  expect_true(is.na(min(function_output$fc_error)))
  expect_true(is.na(max(function_output$fc_error)))
  expect_true(is.na(min(function_output$MASE)))
  expect_true(is.na(max(function_output$MASE)))
})

test_that("check calculate_forecast_errors when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object() %>% 
    split_ts_object(ts_split_date = 200301)
  fc_models = add_all_univariate_forecast_models(
    ts_object_train = function_input$train,
    periods_ahead = 12,
    fc_methods = c("basic", "linear", "prophet")
  )
  expect_error(
    calculate_forecast_errors(
      fc_models = dummy_gasprice
    )
  )
  expect_error(
    calculate_forecast_errors(
      fc_models = data.frame()
    )
  )
  expect_error(
    calculate_forecast_errors(
      fc_models = function_input$valid
    )
  )
  expect_error(
    calculate_forecast_errors(
      fc_models = function_input,
      ts_object_train = function_input$train,
      ts_object_valid = function_input$valid
    )
  )
  expect_error(
    calculate_forecast_errors(
      fc_models = fc_models,
      ts_object_train = list(),
      ts_object_valid = function_input$valid
    )
  )
  expect_error(
    calculate_forecast_errors(
      fc_models = fc_models,
      ts_object_train = dummy_gasprice,
      ts_object_valid = function_input$valid
    )
  )
  expect_error(
    calculate_forecast_errors(
      fc_models = fc_models,
      ts_object_train = function_input$train,
      ts_object_valid = dummy_gasprice
    )
  )
})
