
context("add_fc_errors_to_main_forecasting_table")

test_that("check add_fc_errors_to_main_forecasting_table for univariate with valid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) %>%  # To limit the runtime of the tests
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("basic", "linear", "arima"),
      add_fc_errors = F
    )
  function_output <- add_fc_errors_to_main_forecasting_table(
    main_forecasting_table = function_input
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 6)
  expect_equal(ncol(function_output), 10)
  
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$fc_models), "list")
  expect_equal(class(function_output$fc_errors), "list")
  expected_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", 
    "fc_drift_l3m", "fc_drift_l6m", "fc_drift_l12m", 
    "fc_naive", "fc_naive_seasonal",
    "fc_linear_trend", "fc_linear_trend_seasonal",
    "fc_arima", "fc_arima_stl"
  )
  fc_errors <- function_output$fc_errors[[1]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 432)
  expect_equal(ncol(fc_errors), 9)
  expect_equal(colnames(fc_errors),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(fc_errors$grouping), "character")
  expect_equal(class(fc_errors$fc_model), "character")
  expect_equal(class(fc_errors$fc_date), "numeric")
  expect_equal(class(fc_errors$period), "numeric")
  expect_equal(class(fc_errors$fc_periods_ahead), "numeric")
  expect_equal(class(fc_errors$fc_value), "numeric")
  expect_equal(class(fc_errors$actual), "numeric")
  expect_equal(class(fc_errors$fc_error), "numeric")
  expect_equal(class(fc_errors$MASE), "numeric")
  expect_equal(unique(fc_errors$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(fc_errors$fc_model), sort(expected_models))
  expect_equal(unique(fc_errors$fc_date), 199301)
  expect_equal(length(unique(fc_errors$period)), 36)
  expect_equal(min(fc_errors$period), 199302)
  expect_equal(max(fc_errors$period), 199601)
  expect_equal(unique(fc_errors$fc_periods_ahead), 1:36)
  expect_equal(round(min(fc_errors$fc_value),2), 0.00)
  expect_equal(round(max(fc_errors$fc_value),2), 3.12)
  expect_equal(round(min(fc_errors$actual),2), 0.81)
  expect_equal(round(max(fc_errors$actual),2), 1.34)
  expect_equal(round(min(fc_errors$fc_error),2), -1.30)
  expect_equal(round(max(fc_errors$fc_error),2), 2.12)
  expect_equal(round(min(fc_errors$MASE),2), 0.01)
  expect_equal(round(max(fc_errors$MASE),2), 14.15)
  fc_errors <- function_output$fc_errors[[3]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 432)
  expect_equal(ncol(fc_errors), 9)
  expect_equal(colnames(fc_errors),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(fc_errors$grouping), "character")
  expect_equal(class(fc_errors$fc_model), "character")
  expect_equal(class(fc_errors$fc_date), "numeric")
  expect_equal(class(fc_errors$period), "numeric")
  expect_equal(class(fc_errors$fc_periods_ahead), "numeric")
  expect_equal(class(fc_errors$fc_value), "numeric")
  expect_equal(class(fc_errors$actual), "numeric")
  expect_equal(class(fc_errors$fc_error), "numeric")
  expect_equal(class(fc_errors$MASE), "numeric")
  expect_equal(unique(fc_errors$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(fc_errors$fc_model), sort(expected_models))
  expect_equal(unique(fc_errors$fc_date), 199712)
  expect_equal(length(unique(fc_errors$period)), 36)
  expect_equal(min(fc_errors$period), 199801)
  expect_equal(max(fc_errors$period), 200012)
  expect_equal(unique(fc_errors$fc_periods_ahead), 1:36)
  expect_equal(round(min(fc_errors$fc_value),2), 1.00)
  expect_equal(round(max(fc_errors$fc_value),2), 2.94)
  expect_equal(round(min(fc_errors$actual),2), 0.81)
  expect_equal(round(max(fc_errors$actual),2), 1.83)
  expect_equal(round(min(fc_errors$fc_error),2), -0.84)
  expect_equal(round(max(fc_errors$fc_error),2), 1.76)
  expect_equal(round(min(fc_errors$MASE),2), 0.00)
  expect_equal(round(max(fc_errors$MASE),2), 10.43)
  fc_errors <- function_output$fc_errors[[6]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 432)
  expect_equal(ncol(fc_errors), 9)
  expect_equal(colnames(fc_errors),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(fc_errors$grouping), "character")
  expect_equal(class(fc_errors$fc_model), "character")
  expect_equal(class(fc_errors$fc_date), "numeric")
  expect_equal(class(fc_errors$period), "numeric")
  expect_equal(class(fc_errors$fc_periods_ahead), "numeric")
  expect_equal(class(fc_errors$fc_value), "numeric")
  expect_equal(class(fc_errors$actual), "numeric")
  expect_equal(class(fc_errors$fc_error), "numeric")
  expect_equal(class(fc_errors$MASE), "numeric")
  expect_equal(unique(fc_errors$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(fc_errors$fc_model), sort(expected_models))
  expect_equal(unique(fc_errors$fc_date), 200611)
  expect_equal(length(unique(fc_errors$period)), 36)
  expect_equal(min(fc_errors$period), 200612)
  expect_equal(max(fc_errors$period), 200911)
  expect_equal(unique(fc_errors$fc_periods_ahead), 1:36)
  expect_equal(round(min(fc_errors$fc_value),2), 1.44)
  expect_equal(round(max(fc_errors$fc_value),2), 6.45)
  expect_true(is.na(min(fc_errors$actual)))
  expect_true(is.na(max(fc_errors$actual)))
  expect_true(all(is.na(fc_errors$actual)))
  expect_true(is.na(min(fc_errors$fc_error)))
  expect_true(is.na(max(fc_errors$fc_error)))
  expect_true(all(is.na(fc_errors$fc_error)))
  expect_true(is.na(min(fc_errors$MASE)))
  expect_true(is.na(max(fc_errors$MASE)))
  expect_true(all(is.na(fc_errors$MASE)))
})

test_that("check add_fc_errors_to_main_forecasting_table for multivariate with valid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) %>%  # To limit the runtime of the tests
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("nn", "prophet"),
      add_fc_errors = F
    )
  function_output <- add_fc_errors_to_main_forecasting_table(
    main_forecasting_table = function_input
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 6)
  expect_equal(ncol(function_output), 10)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$fc_models), "list")
  expect_equal(class(function_output$fc_errors), "list")
  expected_models <- c(
    "fc_nn_5n_0decay_xreg", "fc_nn_5n_50decay_xreg", "fc_nn_25n_0decay_xreg", "fc_nn_25n_50decay_xreg",
    "fc_prophet_005cps_xreg", "fc_prophet_050cps_xreg", "fc_prophet_500cps_xreg"
  )
  fc_errors <- function_output$fc_errors[[1]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 252)
  expect_equal(ncol(fc_errors), 9)
  expect_equal(colnames(fc_errors),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(fc_errors$grouping), "character")
  expect_equal(class(fc_errors$fc_model), "character")
  expect_equal(class(fc_errors$fc_date), "numeric")
  expect_equal(class(fc_errors$period), "numeric")
  expect_equal(class(fc_errors$fc_periods_ahead), "numeric")
  expect_equal(class(fc_errors$fc_value), "numeric")
  expect_equal(class(fc_errors$actual), "numeric")
  expect_equal(class(fc_errors$fc_error), "numeric")
  expect_equal(class(fc_errors$MASE), "numeric")
  expect_equal(unique(fc_errors$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(fc_errors$fc_model), sort(expected_models))
  expect_equal(unique(fc_errors$fc_date), 199301)
  expect_equal(length(unique(fc_errors$period)), 36)
  expect_equal(min(fc_errors$period), 199302)
  expect_equal(max(fc_errors$period), 199601)
  expect_equal(unique(fc_errors$fc_periods_ahead), 1:36)
  expect_equal(round(min(fc_errors$fc_value),2), 0.74)
  expect_true(round(max(fc_errors$fc_value),2) %in% c(3.30, 1.87)) # windows, linux
  expect_equal(round(min(fc_errors$actual),2), 0.81)
  expect_equal(round(max(fc_errors$actual),2), 1.34)
  expect_equal(round(min(fc_errors$fc_error),2), -0.54)
  expect_true(round(max(fc_errors$fc_error),2) %in% c(2.17, 0.98)) # windows, linux
  expect_equal(round(min(fc_errors$MASE),2), 0.00)
  expect_true(round(max(fc_errors$MASE),2) %in% c(0.17, 0.10)) # windows, linux
  fc_errors <- function_output$fc_errors[[3]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 252)
  expect_equal(ncol(fc_errors), 9)
  expect_equal(colnames(fc_errors),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(fc_errors$grouping), "character")
  expect_equal(class(fc_errors$fc_model), "character")
  expect_equal(class(fc_errors$fc_date), "numeric")
  expect_equal(class(fc_errors$period), "numeric")
  expect_equal(class(fc_errors$fc_periods_ahead), "numeric")
  expect_equal(class(fc_errors$fc_value), "numeric")
  expect_equal(class(fc_errors$actual), "numeric")
  expect_equal(class(fc_errors$fc_error), "numeric")
  expect_equal(class(fc_errors$MASE), "numeric")
  expect_equal(unique(fc_errors$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(fc_errors$fc_model), sort(expected_models))
  expect_equal(unique(fc_errors$fc_date), 199712)
  expect_equal(length(unique(fc_errors$period)), 36)
  expect_equal(min(fc_errors$period), 199801)
  expect_equal(max(fc_errors$period), 200012)
  expect_equal(unique(fc_errors$fc_periods_ahead), 1:36)
  expect_equal(round(min(fc_errors$fc_value),2), 0.73)
  expect_equal(round(max(fc_errors$fc_value),2), 1.54)
  expect_equal(round(min(fc_errors$actual),2), 0.81)
  expect_equal(round(max(fc_errors$actual),2), 1.83)
  expect_equal(round(min(fc_errors$fc_error),2), -0.69)
  expect_true(round(max(fc_errors$fc_error),2) %in% c(0.46, 0.42)) # windows, linux
  expect_equal(round(min(fc_errors$MASE),2), 0.00)
  expect_true(round(max(fc_errors$MASE),2) %in% c(0.10, 0.10)) # windows, linux
  fc_errors <- function_output$fc_errors[[6]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 0)
  expect_equal(ncol(fc_errors), 0)
})

test_that("check add_fc_errors_to_main_forecasting_table for multivariate with valid inputs", {
  data <- dummy_gasprice %>% 
    dplyr::mutate(gasprice = ifelse(
        test = year_month >= as.Date("2006-01-31"), 
        yes = NA,
        no = gasprice
      )
    )
  function_input <- tstools::initialize_ts_forecast_data(
      data = data,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) %>%  # To limit the runtime of the tests
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("tree"),
      add_fc_errors = F
    )
  function_output <- add_fc_errors_to_main_forecasting_table(
    main_forecasting_table = function_input
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 6)
  expect_equal(ncol(function_output), 10)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$fc_models), "list")
  expect_equal(class(function_output$fc_errors), "list")
  expected_models <- c("fc_rpart_xreg", "fc_ctree_xreg")
  fc_errors <- function_output$fc_errors[[1]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 72)
  expect_equal(ncol(fc_errors), 9)
  expect_equal(colnames(fc_errors),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(fc_errors$grouping), "character")
  expect_equal(class(fc_errors$fc_model), "character")
  expect_equal(class(fc_errors$fc_date), "numeric")
  expect_equal(class(fc_errors$period), "numeric")
  expect_equal(class(fc_errors$fc_periods_ahead), "numeric")
  expect_equal(class(fc_errors$fc_value), "numeric")
  expect_equal(class(fc_errors$actual), "numeric")
  expect_equal(class(fc_errors$fc_error), "numeric")
  expect_equal(class(fc_errors$MASE), "numeric")
  expect_equal(unique(fc_errors$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(fc_errors$fc_model), sort(expected_models))
  expect_equal(unique(fc_errors$fc_date), 199301)
  expect_equal(length(unique(fc_errors$period)), 36)
  expect_equal(min(fc_errors$period), 199302)
  expect_equal(max(fc_errors$period), 199601)
  expect_equal(unique(fc_errors$fc_periods_ahead), 1:36)
  expect_equal(round(min(fc_errors$fc_value),2), 0.55)
  expect_equal(round(max(fc_errors$fc_value),2), 4.14)
  expect_equal(round(min(fc_errors$actual),2), 0.81)
  expect_equal(round(max(fc_errors$actual),2), 1.34)
  expect_equal(round(min(fc_errors$fc_error),2), -0.67)
  expect_equal(round(max(fc_errors$fc_error),2), 3.15)
  expect_equal(round(min(fc_errors$MASE),2), 0.00)
  expect_equal(round(max(fc_errors$MASE),2), 0.24)
  fc_errors <- function_output$fc_errors[[3]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 72)
  expect_equal(ncol(fc_errors), 9)
  expect_equal(colnames(fc_errors),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(fc_errors$grouping), "character")
  expect_equal(class(fc_errors$fc_model), "character")
  expect_equal(class(fc_errors$fc_date), "numeric")
  expect_equal(class(fc_errors$period), "numeric")
  expect_equal(class(fc_errors$fc_periods_ahead), "numeric")
  expect_equal(class(fc_errors$fc_value), "numeric")
  expect_equal(class(fc_errors$actual), "numeric")
  expect_equal(class(fc_errors$fc_error), "numeric")
  expect_equal(class(fc_errors$MASE), "numeric")
  expect_equal(unique(fc_errors$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(fc_errors$fc_model), sort(expected_models))
  expect_equal(unique(fc_errors$fc_date), 199712)
  expect_equal(length(unique(fc_errors$period)), 36)
  expect_equal(min(fc_errors$period), 199801)
  expect_equal(max(fc_errors$period), 200012)
  expect_equal(unique(fc_errors$fc_periods_ahead), 1:36)
  expect_equal(round(min(fc_errors$fc_value),2), 0)
  expect_true(round(max(fc_errors$fc_value),2) %in% c(2.22, 1.54)) # windows, linux
  expect_equal(round(min(fc_errors$actual),2), 0.81)
  expect_equal(round(max(fc_errors$actual),2), 1.83)
  expect_true(round(min(fc_errors$fc_error),2) %in% c(-1.78, -1.83)) # windows, linux
  expect_true(round(max(fc_errors$fc_error),2) %in% c(1.11, 0.56)) # windows, linux
  expect_true(round(min(fc_errors$MASE),2) %in% c(0.00, 0.00)) # windows, linux
  expect_true(round(max(fc_errors$MASE),2) %in% c(0.25, 0.25)) # windows, linux
  fc_errors <- function_output$fc_errors[[6]]
  expect_true(is.data.frame(fc_errors))
  expect_equal(nrow(fc_errors), 22)
  expect_equal(ncol(fc_errors), 9)
  expect_equal(colnames(fc_errors),c("grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", "fc_value", "actual", "fc_error", "MASE"))
  expect_equal(class(fc_errors$grouping), "character")
  expect_equal(class(fc_errors$fc_model), "character")
  expect_equal(class(fc_errors$fc_date), "numeric")
  expect_equal(class(fc_errors$period), "numeric")
  expect_equal(class(fc_errors$fc_periods_ahead), "numeric")
  expect_equal(class(fc_errors$fc_value), "numeric")
  expect_equal(class(fc_errors$actual), "numeric")
  expect_equal(class(fc_errors$fc_error), "numeric")
  expect_equal(class(fc_errors$MASE), "numeric")
  expect_equal(unique(fc_errors$grouping), "state = New York   &   oil_company = CompanyA")
  expect_equal(unique(fc_errors$fc_model), sort(expected_models))
  expect_equal(unique(fc_errors$fc_date), 200512)
  expect_equal(length(unique(fc_errors$period)), 11)
  expect_equal(min(fc_errors$period), 200601)
  expect_equal(max(fc_errors$period), 200611)
  expect_equal(unique(fc_errors$fc_periods_ahead), 1:11)
  expect_equal(round(min(fc_errors$fc_value),2), 0.00)
  expect_true(round(max(fc_errors$fc_value),2) %in% c(3.28, 2.69)) # windows, linux
  expect_true(all(is.na(fc_errors$actual)))
  expect_true(all(is.na(fc_errors$fc_error)))
  expect_true(all(is.na(fc_errors$MASE)))
})

test_that("check add_fc_errors_to_main_forecasting_table when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 180,
      max_train_periods = Inf
    ) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = "basic",
      add_fc_errors = F
    )
  expect_error(
    add_fc_errors_to_main_forecasting_table(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    add_fc_errors_to_main_forecasting_table(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    add_fc_errors_to_main_forecasting_table(
      main_forecasting_table = function_input %>% 
        dplyr::select(-ts_object_train)
    )
  )
  expect_error(
    add_fc_errors_to_main_forecasting_table(
      main_forecasting_table = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
})
