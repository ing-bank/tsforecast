
context("compare_forecasts_with_actuals")

test_that("check compare_forecasts_with_actuals with different inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 120,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(train_length %% 21 == 0 | valid_length == max(valid_length) | valid_length == 0) %>% # To limit the runtime of the tests
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("basic", "linear")
    )
  function_output <- compare_forecasts_with_actuals(
    main_forecasting_table = head(function_input, 1),
    fc_models = c("fc_mean_l3m","fc_drift_l12m","fc_naive_seasonal","fc_linear_trend_seasonal"),
    demo_mode = F
  )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~period", "~value", "~fc_model", "~fc_model"))
  expect_equal(function_output$labels$x, "period")
  expect_equal(function_output$labels$y, "value")
  expect_equal(function_output$labels$colour, "fc_model")
  expect_equal(function_output$labels$text, "paste0(\"Type: \", fc_model, \"<br>Period: \", format.Date(period, ...")
  expect_equal(function_output$labels$xintercept, "xintercept")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 300)
  expect_equal(ncol(function_output$data), 5)
  expect_equal(colnames(function_output$data), c("grouping", "period", "fc_model", "value", "fc_periods_ahead"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$period, "Date")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$value, "numeric")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_equal(unique(function_output$data$grouping), c("state = New York   &   oil_company = CompanyA"))
  expect_equal(min(function_output$data$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$data$period), as.Date("2003-12-31"))
  expect_equal(function_output$data$fc_model, c(
    rep("actuals", 156), 
    rep("fc_drift_l12m", 36), 
    rep("fc_linear_trend_seasonal", 36),
    rep("fc_mean_l3m", 36), 
    rep("fc_naive_seasonal", 36)
  ))
  expect_equal(round(min(function_output$data$value),2), 0.81)
  expect_equal(round(max(function_output$data$value),2), 1.87)
  function_output <- compare_forecasts_with_actuals(
    main_forecasting_table = tail(function_input, 1),
    fc_models = c("fc_naive"),
    demo_mode = T
  )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~period", "~value", "~fc_model", "~fc_model"))
  expect_equal(function_output$labels$x, "period")
  expect_equal(function_output$labels$y, "value")
  expect_equal(function_output$labels$colour, "fc_model")
  expect_equal(function_output$labels$text, "paste0(\"Type: \", fc_model, \"<br>Period: \", format.Date(period, ...")
  expect_equal(function_output$labels$xintercept, "xintercept")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 227)
  expect_equal(ncol(function_output$data), 5)
  expect_equal(colnames(function_output$data), c("grouping", "period", "fc_model", "value", "fc_periods_ahead"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$period, "Date")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$value, "numeric")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_equal(unique(function_output$data$grouping), c("state = Indiana   &   oil_company = CompanyB"))
  expect_equal(min(function_output$data$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$data$period), as.Date("2009-11-30"))
  expect_equal(function_output$data$fc_model, c(
    rep("actuals", 191), 
    rep("fc_naive", 36)
  ))
  expect_equal(round(min(function_output$data$value),2), 0.83)
  expect_equal(round(max(function_output$data$value),2), 3.35)
})

test_that("check compare_forecasts_with_actuals when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 190,
      max_train_periods = Inf
    ) %>% 
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("basic", "linear")
    )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = function_input %>% 
        dplyr::select(-fc_errors)
    )
  )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = function_input
    )
  )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = head(function_input, 1),
      fc_models = ""
    )
  )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = head(function_input, 1),
      fc_models = NULL
    )
  )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = head(function_input, 1),
      fc_models = c("fc_linear_trend", "42")
    )
  )
  expect_error(
    compare_forecasts_with_actuals(
      main_forecasting_table = head(function_input, 1),
      fc_models = 42
    )
  )
})
