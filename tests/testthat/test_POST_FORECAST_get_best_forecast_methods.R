
context("get_best_forecast_methods")

test_that("check get_best_forecast_methods with different n and min/max periods ahead", {
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
      fc_methods = c("basic")
    ) %>% 
    get_forecast_accuracy_overview()
  function_output <- get_best_forecast_methods(
    accuracy_overview = function_input,
    n = 1,
    min_periods_ahead = 1,
    max_periods_ahead = Inf
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 4)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("grouping", "fc_model", "ranking"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$ranking), "numeric")
  expect_equal(unique(function_output$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(unique(function_output$fc_model), c("fc_mean_l3m", "fc_naive"))
  expect_equal(unique(function_output$ranking), 1)
  function_output <- get_best_forecast_methods(
    accuracy_overview = function_input,
    n = 5,
    min_periods_ahead = 1,
    max_periods_ahead = Inf
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 20)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("grouping", "fc_model", "ranking"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$ranking), "numeric")
  expect_equal(unique(function_output$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(unique(function_output$fc_model), c("fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_naive", "fc_naive_seasonal"))
  expect_equal(unique(function_output$ranking), 1:5)
  function_output <- get_best_forecast_methods(
    accuracy_overview = function_input,
    n = 5,
    min_periods_ahead = 30,
    max_periods_ahead = 36
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 20)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("grouping", "fc_model", "ranking"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$ranking), "numeric")
  expect_equal(unique(function_output$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(unique(function_output$fc_model), c("fc_mean_l3m", "fc_mean_l12m", "fc_mean_l6m", "fc_naive", "fc_naive_seasonal"))
  expect_equal(unique(function_output$ranking), 1:5)
  function_output <- get_best_forecast_methods(
    accuracy_overview = function_input,
    n = 100,
    min_periods_ahead = -42,
    max_periods_ahead = NA
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 32)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("grouping", "fc_model", "ranking"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$ranking), "numeric")
  expect_equal(unique(function_output$grouping), c(
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB", 
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB"
  ))
  expect_equal(unique(function_output$fc_model), c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_drift_l12m", "fc_drift_l6m", "fc_drift_l3m"
  ))
  expect_equal(unique(function_output$ranking), 1:8)
  function_output <- get_best_forecast_methods(
    accuracy_overview = function_input,
    n = 3,
    min_periods_ahead = 1000,
    max_periods_ahead = 1000
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 0)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("grouping", "fc_model", "ranking"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$ranking), "numeric")
})

test_that("check get_best_forecast_methods when invalid inputs are used", {
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
      fc_methods = c("basic"),
      add_fc_errors = F
    ) %>% 
    add_fc_errors_to_main_forecasting_table() %>% 
    get_forecast_accuracy_overview()
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = "potato"
    )
  )
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = dummy_gasprice
    )
  )
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = function_input,
      n = -10
    )
  )
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = function_input,
      n = 0
    )
  )
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = function_input,
      n = -Inf
    )
  )
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = function_input,
      n = 2.5
    )
  )
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = function_input,
      n = "3"
    )
  )
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = function_input,
      n = as.Date("2018-05-09")
    )
  )
  expect_error(
    get_best_forecast_methods(
      accuracy_overview = function_input,
      min_periods_ahead = 42,
      max_periods_ahead = 3
    )
  )
})
