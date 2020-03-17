
context("compare_forecast_performance_per_group")

test_that("check compare_forecast_performance_per_group with different inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 180,
      max_train_periods = Inf
    ) %>% 
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("linear")
    )
  
  function_output <- function_input %>% 
    get_forecast_accuracy_overview(
      metric = "MAE"
    ) %>% 
    compare_forecast_performance_per_group(
      fc_model = "fc_linear_trend",
      demo_mode = F
    )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~fc_periods_ahead", "~metric", "~grouping", "~grouping"))
  expect_equal(function_output$labels$x, "Forecast horizon")
  expect_equal(function_output$labels$y, "metric")
  expect_equal(function_output$labels$colour, "grouping")
  expect_equal(function_output$labels$fill, "grouping")
  expect_equal(function_output$labels$ymin, "lower_bound")
  expect_equal(function_output$labels$ymax, "upper_bound")
  expect_equal(function_output$labels$text, "paste0(\"Group: \", grouping, \"<br>Forecast horizon: \", fc_periods_ahead, ...")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 44)
  expect_equal(ncol(function_output$data), 7)
  expect_equal(colnames(function_output$data), c("grouping", "fc_model", "fc_periods_ahead", "n_data_point", "lower_bound", "metric", "upper_bound"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_is(function_output$data$n_data_point, "integer")
  expect_is(function_output$data$lower_bound, "numeric")
  expect_is(function_output$data$metric, "numeric")
  expect_is(function_output$data$upper_bound, "numeric")
  expect_equal(unique(function_output$data$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(sort(function_output$data$fc_model), c(rep("fc_linear_trend", 44)))
  expect_equal(unique(function_output$data$fc_periods_ahead), 1:11)
  expect_equal(unique(function_output$data$n_data_point), 11:1)
  expect_equal(round(min(function_output$data$lower_bound),2), 0.00)
  expect_equal(round(max(function_output$data$lower_bound),2), 0.85)
  expect_equal(round(min(function_output$data$metric),2), 0.09)
  expect_equal(round(max(function_output$data$metric),2), 1.00)
  expect_equal(round(min(function_output$data$upper_bound),2), 0.09)
  expect_equal(round(max(function_output$data$upper_bound),2), 2.34)
  
  function_output <- function_input %>% 
    get_forecast_accuracy_overview(
      metric = "MAPE"
    ) %>% 
    compare_forecast_performance_per_group(
      fc_model = "fc_linear_trend_seasonal",
      demo_mode = T
    )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~fc_periods_ahead", "~metric", "~grouping", "~grouping"))
  expect_equal(function_output$labels$x, "Forecast horizon")
  expect_equal(function_output$labels$y, "metric")
  expect_equal(function_output$labels$colour, "grouping")
  expect_equal(function_output$labels$fill, "grouping")
  expect_equal(function_output$labels$ymin, "lower_bound")
  expect_equal(function_output$labels$ymax, "upper_bound")
  expect_equal(function_output$labels$text, "paste0(\"Group: \", grouping, \"<br>Forecast horizon: \", fc_periods_ahead, ...")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 44)
  expect_equal(ncol(function_output$data), 7)
  expect_equal(colnames(function_output$data), c("grouping", "fc_model", "fc_periods_ahead", "n_data_point", "lower_bound", "metric", "upper_bound"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_is(function_output$data$n_data_point, "integer")
  expect_is(function_output$data$lower_bound, "numeric")
  expect_is(function_output$data$metric, "numeric")
  expect_is(function_output$data$upper_bound, "numeric")
  expect_equal(unique(function_output$data$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(sort(function_output$data$fc_model), c(rep("fc_linear_trend_seasonal", 44)))
  expect_equal(unique(function_output$data$fc_periods_ahead), 1:11)
  expect_equal(unique(function_output$data$n_data_point), 11:1)
  expect_equal(round(min(function_output$data$lower_bound),2), 0.00)
  expect_equal(round(max(function_output$data$lower_bound),2), 0.33)
  expect_equal(round(min(function_output$data$metric),2), 0.05)
  expect_equal(round(max(function_output$data$metric),2), 0.33)
  expect_equal(round(min(function_output$data$upper_bound),2), 0.05)
  expect_equal(round(max(function_output$data$upper_bound),2), 0.67)
  
  function_output <- function_input %>% 
    get_forecast_accuracy_overview(
      metric = "MASE"
    ) %>% 
    compare_forecast_performance_per_group(
      fc_model = "fc_linear_trend",
      demo_mode = T
    )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~fc_periods_ahead", "~metric", "~grouping", "~grouping"))
  expect_equal(function_output$labels$x, "Forecast horizon")
  expect_equal(function_output$labels$y, "metric")
  expect_equal(function_output$labels$colour, "grouping")
  expect_equal(function_output$labels$fill, "grouping")
  expect_equal(function_output$labels$ymin, "lower_bound")
  expect_equal(function_output$labels$ymax, "upper_bound")
  expect_equal(function_output$labels$text, "paste0(\"Group: \", grouping, \"<br>Forecast horizon: \", fc_periods_ahead, ...")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 44)
  expect_equal(ncol(function_output$data), 7)
  expect_equal(colnames(function_output$data), c("grouping", "fc_model", "fc_periods_ahead", "n_data_point", "lower_bound", "metric", "upper_bound"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_is(function_output$data$n_data_point, "integer")
  expect_is(function_output$data$lower_bound, "numeric")
  expect_is(function_output$data$metric, "numeric")
  expect_is(function_output$data$upper_bound, "numeric")
  expect_equal(unique(function_output$data$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(sort(function_output$data$fc_model), c(rep("fc_linear_trend", 44)))
  expect_equal(unique(function_output$data$fc_periods_ahead), 1:11)
  expect_equal(unique(function_output$data$n_data_point), 11:1)
  expect_equal(round(min(function_output$data$lower_bound),2), 0.00)
  expect_equal(round(max(function_output$data$lower_bound),2), 3.67)
  expect_equal(round(min(function_output$data$metric),2), 0.36)
  expect_equal(round(max(function_output$data$metric),2), 4.32)
  expect_equal(round(min(function_output$data$upper_bound),2), 0.36)
  expect_equal(round(max(function_output$data$upper_bound),2), 9.93)
})

test_that("check compare_forecast_performance_per_group when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 190,
      max_train_periods = Inf
    ) %>% 
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("linear")
    ) %>% 
    get_forecast_accuracy_overview()
  expect_error(
    compare_forecast_performance_per_group(
      accuracy_overview = "potato"
    )
  )
  expect_error(
    compare_forecast_performance_per_group(
      accuracy_overview = dummy_gasprice
    )
  )
  expect_error(
    compare_forecast_performance_per_group(
      accuracy_overview = function_input %>% 
        dplyr::select(-mean)
    )
  )
  expect_error(
    compare_forecast_performance_per_group(
      accuracy_overview = function_input %>% 
        dplyr::filter(FALSE),
      fc_model = "fc_linear_trend_seasonal"
    )
  )
  expect_error(
    compare_forecast_performance_per_group(
      accuracy_overview = function_input,
      fc_model = ""
    )
  )
  expect_error(
    compare_forecast_performance_per_group(
      accuracy_overview = function_input,
      fc_model = c("fc_linear_trend_seasonal", "42")
    )
  )
  expect_error(
    compare_forecast_performance_per_group(
      accuracy_overview = function_input,
      fc_model = 42
    )
  )
})
