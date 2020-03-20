
context("compare_forecasts_performance")

test_that("check compare_forecasts_performance with different inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 160,
      max_train_periods = Inf
    ) %>% 
    head(30) %>% 
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("basic")
    )
  
  function_output <- function_input %>% 
    get_forecast_accuracy_overview(
      metric = "MAE"
    ) %>% 
    compare_forecasts_performance(
      fc_models = c("fc_naive_seasonal", "fc_drift_l12m"),
      demo_mode = T
    )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~fc_periods_ahead", "~metric", "~fc_model", "~fc_model"))
  expect_equal(function_output$labels$x, "Forecast horizon")
  expect_equal(function_output$labels$y, "metric")
  expect_equal(function_output$labels$colour, "fc_model")
  expect_equal(function_output$labels$fill, "fc_model")
  expect_equal(function_output$labels$ymin, "lower_bound")
  expect_equal(function_output$labels$ymax, "upper_bound")
  expect_equal(function_output$labels$text, "paste0(\"Model: \", fc_model, \"<br>Forecast horizon: \", fc_periods_ahead, ...")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 62)
  expect_equal(ncol(function_output$data), 7)
  expect_equal(colnames(function_output$data), c("grouping", "fc_model", "fc_periods_ahead", "n_data_point", "lower_bound", "metric", "upper_bound"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_is(function_output$data$n_data_point, "integer")
  expect_is(function_output$data$lower_bound, "numeric")
  expect_is(function_output$data$metric, "numeric")
  expect_is(function_output$data$upper_bound, "numeric")
  expect_equal(unique(function_output$data$grouping), c("state = New York   &   oil_company = CompanyA"))
  expect_equal(sort(function_output$data$fc_model), c(
    rep("fc_drift_l12m", 31), 
    rep("fc_naive_seasonal", 31)
  ))
  expect_equal(unique(function_output$data$fc_periods_ahead), 1:31)
  expect_equal(unique(function_output$data$n_data_point), 30:1)
  expect_equal(round(min(function_output$data$lower_bound),2), 0.00)
  expect_equal(round(max(function_output$data$lower_bound),2), 0.75)
  expect_equal(round(min(function_output$data$metric),2), 0.37)
  expect_equal(round(max(function_output$data$metric),2), 1.07)
  expect_equal(round(min(function_output$data$upper_bound),2), 0.73)
  expect_equal(round(max(function_output$data$upper_bound),2), 2.61)
  
  function_output <- function_input %>% 
    get_forecast_accuracy_overview(
      metric = "MAE"
    ) %>% 
    compare_forecasts_performance(
      fc_models = "fc_mean_l6m",
      demo_mode = F
    )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~fc_periods_ahead", "~metric", "~fc_model", "~fc_model"))
  expect_equal(function_output$labels$x, "Forecast horizon")
  expect_equal(function_output$labels$y, "metric")
  expect_equal(function_output$labels$colour, "fc_model")
  expect_equal(function_output$labels$fill, "fc_model")
  expect_equal(function_output$labels$ymin, "lower_bound")
  expect_equal(function_output$labels$ymax, "upper_bound")
  expect_equal(function_output$labels$text, "paste0(\"Model: \", fc_model, \"<br>Forecast horizon: \", fc_periods_ahead, ...")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 31)
  expect_equal(ncol(function_output$data), 7)
  expect_equal(colnames(function_output$data), c("grouping", "fc_model", "fc_periods_ahead", "n_data_point", "lower_bound", "metric", "upper_bound"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_is(function_output$data$n_data_point, "integer")
  expect_is(function_output$data$lower_bound, "numeric")
  expect_is(function_output$data$metric, "numeric")
  expect_is(function_output$data$upper_bound, "numeric")
  expect_equal(unique(function_output$data$grouping), c("state = New York   &   oil_company = CompanyA"))
  expect_equal(function_output$data$fc_model, rep("fc_mean_l6m", 31))
  expect_equal(unique(function_output$data$fc_periods_ahead), 1:31)
  expect_equal(unique(function_output$data$n_data_point), 30:1)
  expect_equal(round(min(function_output$data$lower_bound),2), 0.00)
  expect_equal(round(max(function_output$data$lower_bound),2), 0.67)
  expect_equal(round(min(function_output$data$metric),2), 0.34)
  expect_equal(round(max(function_output$data$metric),2), 0.88)
  expect_equal(round(min(function_output$data$upper_bound),2), 0.67)
  expect_equal(round(max(function_output$data$upper_bound),2), 2.45)
  
  function_output <- function_input %>% 
    get_forecast_accuracy_overview(
      metric = "MASE"
    ) %>% 
    compare_forecasts_performance(
      fc_models = c(
        "fc_drift_l12m", "fc_drift_l3m", "fc_drift_l6m", "fc_mean_l12m", 
        "fc_mean_l3m", "fc_mean_l6m", "fc_naive", "fc_naive_seasonal"
      ),
      demo_mode = F
    )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~fc_periods_ahead", "~metric", "~fc_model", "~fc_model"))
  expect_equal(function_output$labels$x, "Forecast horizon")
  expect_equal(function_output$labels$y, "metric")
  expect_equal(function_output$labels$colour, "fc_model")
  expect_equal(function_output$labels$fill, "fc_model")
  expect_equal(function_output$labels$ymin, "lower_bound")
  expect_equal(function_output$labels$ymax, "upper_bound")
  expect_equal(function_output$labels$text, "paste0(\"Model: \", fc_model, \"<br>Forecast horizon: \", fc_periods_ahead, ...")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 248)
  expect_equal(ncol(function_output$data), 7)
  expect_equal(colnames(function_output$data), c("grouping", "fc_model", "fc_periods_ahead", "n_data_point", "lower_bound", "metric", "upper_bound"))
  expect_is(function_output$data$grouping, "character")
  expect_is(function_output$data$fc_model, "character")
  expect_is(function_output$data$fc_periods_ahead, "numeric")
  expect_is(function_output$data$n_data_point, "integer")
  expect_is(function_output$data$lower_bound, "numeric")
  expect_is(function_output$data$metric, "numeric")
  expect_is(function_output$data$upper_bound, "numeric")
  expect_equal(unique(function_output$data$grouping), c("state = New York   &   oil_company = CompanyA"))
  expect_equal(sort(function_output$data$fc_model), c(
    rep("fc_drift_l12m", 31), 
    rep("fc_drift_l3m", 31),
    rep("fc_drift_l6m", 31),
    rep("fc_mean_l12m", 31), 
    rep("fc_mean_l3m", 31),
    rep("fc_mean_l6m", 31),
    rep("fc_naive", 31),
    rep("fc_naive_seasonal", 31)
  ))
  expect_equal(unique(function_output$data$fc_periods_ahead), 1:31)
  expect_equal(unique(function_output$data$n_data_point), 30:1)
  expect_equal(round(min(function_output$data$lower_bound),2), 0.00)
  expect_equal(round(max(function_output$data$lower_bound),2), 3.54)
  expect_equal(round(min(function_output$data$metric),2), 1.32)
  expect_equal(round(max(function_output$data$metric),2), 9.79)
  expect_equal(round(min(function_output$data$upper_bound),2), 1.93)
  expect_equal(round(max(function_output$data$upper_bound),2), 32.14)
})

test_that("check compare_forecasts_performance when invalid inputs are used", {
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
      fc_methods = c("basic")
    ) %>% 
    get_forecast_accuracy_overview()
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = "potato"
    )
  )
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = dummy_gasprice
    )
  )
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = function_input %>% 
        dplyr::select(-mean)
    )
  )
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = function_input %>% 
        dplyr::filter(FALSE),
      fc_model = "fc_mean_l3m"
    )
  )
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = function_input,
      fc_model = "fc_mean_l3m"
    )
  )
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = function_input %>% 
        dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA"),
      fc_model = ""
    )
  )
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = function_input %>% 
        dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA"),
      fc_model = NULL
    )
  )
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = function_input %>% 
        dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA"),
      fc_models = c("fc_mean_l3m", "42")
    )
  )
  expect_error(
    compare_forecasts_performance(
      accuracy_overview = function_input %>% 
        dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA"),
      fc_models = 42
    )
  )
})
