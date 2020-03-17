
context("compare_forecast_per_group")

test_that("check compare_forecast_per_group with different inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 120,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(ts_split_date == 200503) %>%
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("prophet")
    )
  function_output <- compare_forecast_per_group(
    main_forecasting_table = function_input,
    fc_model = "fc_prophet_050cps_xreg",
    demo_mode = F
  )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~period", "~value", "~grouping"))
  expect_equal(function_output$labels$x, "period")
  expect_equal(function_output$labels$y, "value")
  expect_equal(function_output$labels$colour, "grouping")
  expect_equal(function_output$labels$text, "paste0(\"Group: \", grouping, \"<br>Period: \", format.Date(period, ...")
  expect_equal(function_output$labels$xintercept, "xintercept")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 764)
  expect_equal(ncol(function_output$data), 5)
  expect_equal(colnames(function_output$data), c("grouping", "period", "fc_model", "value", "fc_periods_ahead"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$period, "Date")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$value, "numeric")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_equal(unique(function_output$data$grouping), c(
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(min(function_output$data$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$data$period), as.Date("2006-11-30"))
  expect_equal(function_output$data$fc_model, c(
    rep("actuals", 684), 
    rep("fc_prophet_050cps_xreg", 80)
  ))
  expect_equal(round(min(function_output$data$value),2), 0.78)
  expect_equal(round(max(function_output$data$value),2), 2.75)
  function_output <- compare_forecast_per_group(
    main_forecasting_table = function_input,
    fc_model = "fc_prophet_050cps_xreg",
    demo_mode = T
  )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~period", "~value", "~grouping"))
  expect_equal(function_output$labels$x, "period")
  expect_equal(function_output$labels$y, "value")
  expect_equal(function_output$labels$colour, "grouping")
  expect_equal(function_output$labels$text, "paste0(\"Group: \", grouping, \"<br>Period: \", format.Date(period, ...")
  expect_equal(function_output$labels$xintercept, "xintercept")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 764)
  expect_equal(ncol(function_output$data), 5)
  expect_equal(colnames(function_output$data), c("grouping", "period", "fc_model", "value", "fc_periods_ahead"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$period, "Date")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$value, "numeric")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_equal(unique(function_output$data$grouping), c(
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(min(function_output$data$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$data$period), as.Date("2006-11-30"))
  expect_equal(function_output$data$fc_model, c(
    rep("actuals", 684), 
    rep("fc_prophet_050cps_xreg", 80)
  ))
  expect_equal(round(min(function_output$data$value),2), 0.78)
  expect_equal(round(max(function_output$data$value),2), 2.75)
})

test_that("check compare_forecast_per_group when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 120,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(ts_split_date %in% c(200503, 200504)) %>%
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("linear")
    )
  expect_error(
    compare_forecast_per_group(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    compare_forecast_per_group(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    compare_forecast_per_group(
      main_forecasting_table = function_input %>% 
        dplyr::select(-fc_errors)
    )
  )
  expect_error(
    compare_forecast_per_group(
      main_forecasting_table = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    compare_forecast_per_group(
      main_forecasting_table = function_input
    )
  )
  expect_error(
    compare_forecast_per_group(
      main_forecasting_table = head(function_input, 1),
      fc_model = ""
    )
  )
  expect_error(
    compare_forecast_per_group(
      main_forecasting_table = head(function_input, 1),
      fc_model = c("fc_drift_l12m", "42")
    )
  )
  expect_error(
    compare_forecast_per_group(
      main_forecasting_table = head(function_input, 1),
      fc_model = 42
    )
  )
})
