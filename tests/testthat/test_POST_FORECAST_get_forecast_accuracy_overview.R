
context("get_forecast_accuracy_overview")

test_that("check get_forecast_accuracy_overview with different metrics", {
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
      fc_methods = c("basic", "linear"),
      add_fc_errors = F
    ) %>% 
    add_fc_errors_to_main_forecasting_table()
  
  function_output <- get_forecast_accuracy_overview(
    main_forecasting_table = function_input,
    metric = "MAE"
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 1440)
  expect_equal(ncol(function_output), 14)
  expect_equal(colnames(function_output), c(
    "grouping", "fc_periods_ahead", "fc_model", "n_data_point", 
    "MAE", "MAPE", "MASE", 
    "min", "q1", "metric", "q3", "max", "sd", "order"
  ))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_periods_ahead), "numeric")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$n_data_point), "integer")
  expect_equal(class(function_output$MAE), "numeric")
  expect_equal(class(function_output$MAPE), "numeric")
  expect_equal(class(function_output$MASE), "numeric")
  expect_equal(class(function_output$min), "numeric")
  expect_equal(class(function_output$q1), "numeric")
  expect_equal(class(function_output$metric), "numeric")
  expect_equal(class(function_output$q3), "numeric")
  expect_equal(class(function_output$max), "numeric")
  expect_equal(class(function_output$sd), "numeric")
  expect_equal(class(function_output$order), "numeric")
  expect_equal(unique(function_output$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(unique(function_output$fc_periods_ahead), 1:36)
  expect_equal(unique(function_output$fc_model), c(
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_mean_l12m", 
    "fc_mean_l6m", "fc_drift_l3m", "fc_naive", "fc_drift_l12m", 
    "fc_naive_seasonal", "fc_mean_l3m", "fc_drift_l6m"
  ))
  expect_equal(unique(function_output$n_data_point), 5:3)
  expect_equal(round(min(function_output$MAE),2), 0.06)
  expect_equal(round(max(function_output$MAE),2), 2.85)
  expect_equal(round(min(function_output$MAPE),2), 0.04)
  expect_equal(round(max(function_output$MAPE),2), 1.89)
  expect_equal(round(min(function_output$MASE),2), 0.28)
  expect_equal(round(max(function_output$MASE),2), 13.05)
  expect_equal(round(min(function_output$min),2), 0.00)
  expect_equal(round(max(function_output$min),2), 1.20)
  expect_equal(round(min(function_output$q1),2), 0.01)
  expect_equal(round(max(function_output$q1),2), 1.97)
  expect_equal(round(min(function_output$metric),2), 0.06)
  expect_equal(round(max(function_output$metric),2), 2.85)
  expect_equal(round(min(function_output$q3),2), 0.08)
  expect_equal(round(max(function_output$q3),2), 4.03)
  expect_equal(round(min(function_output$max),2), 0.12)
  expect_equal(round(max(function_output$max),2), 5.12)
  expect_equal(round(min(function_output$sd),2), 0.02)
  expect_equal(round(max(function_output$sd),2), 2.40)
  expect_equal(unique(function_output$order), 1:10)
  
  function_output <- get_forecast_accuracy_overview(
    main_forecasting_table = function_input,
    metric = "MAPE"
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 1440)
  expect_equal(ncol(function_output), 14)
  expect_equal(colnames(function_output), c(
    "grouping", "fc_periods_ahead", "fc_model", "n_data_point", 
    "MAE", "MAPE", "MASE", 
    "min", "q1", "metric", "q3", "max", "sd", "order"
  ))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_periods_ahead), "numeric")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$n_data_point), "integer")
  expect_equal(class(function_output$MAE), "numeric")
  expect_equal(class(function_output$MAPE), "numeric")
  expect_equal(class(function_output$MASE), "numeric")
  expect_equal(class(function_output$min), "numeric")
  expect_equal(class(function_output$q1), "numeric")
  expect_equal(class(function_output$metric), "numeric")
  expect_equal(class(function_output$q3), "numeric")
  expect_equal(class(function_output$max), "numeric")
  expect_equal(class(function_output$sd), "numeric")
  expect_equal(class(function_output$order), "numeric")
  expect_equal(unique(function_output$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(unique(function_output$fc_periods_ahead), 1:36)
  expect_equal(unique(function_output$fc_model), c(
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_mean_l12m", 
    "fc_naive_seasonal", "fc_mean_l6m", "fc_drift_l3m", "fc_naive", 
    "fc_drift_l12m", "fc_drift_l6m", "fc_mean_l3m"
  ))
  expect_equal(unique(function_output$n_data_point), 5:3)
  expect_equal(round(min(function_output$MAE),2), 0.06)
  expect_equal(round(max(function_output$MAE),2), 2.85)
  expect_equal(round(min(function_output$MAPE),2), 0.04)
  expect_equal(round(max(function_output$MAPE),2), 1.89)
  expect_equal(round(min(function_output$MASE),2), 0.28)
  expect_equal(round(max(function_output$MASE),2), 13.05)
  expect_equal(round(min(function_output$min),2), 0.00)
  expect_equal(round(max(function_output$min),2), 0.84)
  expect_equal(round(min(function_output$q1),2), 0.01)
  expect_equal(round(max(function_output$q1),2), 1.05)
  expect_equal(round(min(function_output$metric),2), 0.04)
  expect_equal(round(max(function_output$metric),2), 1.89)
  expect_equal(round(min(function_output$q3),2), 0.05)
  expect_equal(round(max(function_output$q3),2), 2.71)
  expect_equal(round(min(function_output$max),2), 0.08)
  expect_equal(round(max(function_output$max),2), 4.16)
  expect_equal(round(min(function_output$sd),2), 0.01)
  expect_equal(round(max(function_output$sd),2), 2.03)
  expect_equal(unique(function_output$order), 1:10)
  
  function_output <- get_forecast_accuracy_overview(
    main_forecasting_table = function_input,
    metric = "MASE"
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 1440)
  expect_equal(ncol(function_output), 14)
  expect_equal(colnames(function_output), c(
    "grouping", "fc_periods_ahead", "fc_model", "n_data_point", 
    "MAE", "MAPE", "MASE", 
    "min", "q1", "metric", "q3", "max", "sd", "order"
  ))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_periods_ahead), "numeric")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$n_data_point), "integer")
  expect_equal(class(function_output$MAE), "numeric")
  expect_equal(class(function_output$MAPE), "numeric")
  expect_equal(class(function_output$MASE), "numeric")
  expect_equal(class(function_output$min), "numeric")
  expect_equal(class(function_output$q1), "numeric")
  expect_equal(class(function_output$metric), "numeric")
  expect_equal(class(function_output$q3), "numeric")
  expect_equal(class(function_output$max), "numeric")
  expect_equal(class(function_output$sd), "numeric")
  expect_equal(class(function_output$order), "numeric")
  expect_equal(unique(function_output$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(unique(function_output$fc_periods_ahead), 1:36)
  expect_equal(unique(function_output$fc_model), c(
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_mean_l12m", 
    "fc_mean_l6m", "fc_drift_l3m", "fc_naive", "fc_drift_l12m", "fc_naive_seasonal", 
    "fc_mean_l3m", "fc_drift_l6m"
  ))
  expect_equal(unique(function_output$n_data_point), 5:3)
  expect_equal(round(min(function_output$MAE),2), 0.06)
  expect_equal(round(max(function_output$MAE),2), 2.85)
  expect_equal(round(min(function_output$MAPE),2), 0.04)
  expect_equal(round(max(function_output$MAPE),2), 1.89)
  expect_equal(round(min(function_output$MASE),2), 0.28)
  expect_equal(round(max(function_output$MASE),2), 13.05)
  expect_equal(round(min(function_output$min),2), 0.00)
  expect_equal(round(max(function_output$min),2), 5.71)
  expect_equal(round(min(function_output$q1),2), 0.04)
  expect_equal(round(max(function_output$q1),2), 8.96)
  expect_equal(round(min(function_output$metric),2), 0.28)
  expect_equal(round(max(function_output$metric),2), 13.05)
  expect_equal(round(min(function_output$q3),2), 0.40)
  expect_equal(round(max(function_output$q3),2), 18.45)
  expect_equal(round(min(function_output$max),2), 0.60)
  expect_equal(round(max(function_output$max),2), 23.61)
  expect_equal(round(min(function_output$sd),2), 0.08)
  expect_equal(round(max(function_output$sd),2), 11.05)
  expect_equal(unique(function_output$order), 1:10)
})

test_that("check get_forecast_accuracy_overview when invalid inputs are used", {
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
    get_forecast_accuracy_overview(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    get_forecast_accuracy_overview(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    get_forecast_accuracy_overview(
      main_forecasting_table = function_input %>% 
        dplyr::select(-fc_errors)
    )
  )
  expect_error(
    get_forecast_accuracy_overview(
      main_forecasting_table = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    get_forecast_accuracy_overview(
      main_forecasting_table = function_input,
      metric = "magic"
    )
  )
  expect_error(
    get_forecast_accuracy_overview(
      main_forecasting_table = function_input,
      metric = c("MAE", "MAPE")
    )
  )
})
