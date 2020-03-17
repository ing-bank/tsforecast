
context("update_main_forecasting_table")

dir_for_testing <- tempdir()
file_path_for_testing_univariate <- file.path(dir_for_testing, "test_for_univariate.RData") 
file_path_for_testing_multivariate <- file.path(dir_for_testing, "test_for_multivariate.RData")
file_path_for_testing_invalid <- file.path(dir_for_testing, "test_for_invalid.RData")

test_that("check update_main_forecasting_table for univariate without initial available file", {
  if (file.exists(file_path_for_testing_univariate)) {
    expect_true(file.remove(file_path_for_testing_univariate))
  }
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company")
  )
  for (run in c("once", "twice")) {
    expect_silent(
      function_output <- update_main_forecasting_table(
        file_path = file_path_for_testing_univariate,
        data = function_input,
        min_train_periods = 15 * 12,
        fc_methods = c("basic"),
        verbose = F,
        parallel = T
      )
    )
    expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(function_output), 48)
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
    expect_equal(class(function_output$fc_errors), "list")
    expect_equal(class(function_output$fc_models), "list")
    expected_models <- c(
      "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", 
      "fc_drift_l3m", "fc_drift_l6m", "fc_drift_l12m", 
      "fc_naive", "fc_naive_seasonal"
    )
    fc_models <- function_output$fc_models[[1]]
    expect_equal(class(fc_models), "list")
    expect_equal(names(fc_models), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
      expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
      expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
      expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200512)
      expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200601)
      expect_equal(fc_models[[exp_model]]$fc_data$period[12], 200612)
      expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
    }
    fc_models <- function_output$fc_models[[48]]
    expect_equal(class(fc_models), "list")
    expect_equal(names(fc_models), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
      expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
      expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
      expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200611)
      expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200612)
      expect_equal(fc_models[[exp_model]]$fc_data$period[12], 200711)
      expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
    }
  }
})

test_that("check update_main_forecasting_table for univariate when extending on available file", {
  expect_true(file.exists(file_path_for_testing_univariate))
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company")
  )
  for (run in c("once", "twice")) {
    capture.output(
      function_output <- update_main_forecasting_table(
        file_path = file_path_for_testing_univariate,
        data = function_input,
        min_train_periods = 14 * 12,
        fc_methods = c("basic", "linear"),
        verbose = F,
        parallel = F
      ),
      file = 'NUL'
    )
    expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(function_output), 96)
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
    expect_equal(class(function_output$fc_errors), "list")
    expect_equal(class(function_output$fc_models), "list")
    expected_models <- c(
      "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", 
      "fc_drift_l3m", "fc_drift_l6m", "fc_drift_l12m", 
      "fc_naive", "fc_naive_seasonal",
      "fc_linear_trend", "fc_linear_trend_seasonal"
    )
    fc_models <- function_output$fc_models[[1]]
    expect_equal(class(fc_models), "list")
    expect_equal(names(fc_models), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
      expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
      expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
      expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200412)
      expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200501)
      expect_equal(fc_models[[exp_model]]$fc_data$period[12], 200512)
      expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
    }
    fc_models <- function_output$fc_models[[96]]
    expect_equal(class(fc_models), "list")
    expect_equal(names(fc_models), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
      expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
      expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
      expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200611)
      expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200612)
      expect_equal(fc_models[[exp_model]]$fc_data$period[12], 200711)
      expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
    }
  }
})

test_that("check update_main_forecasting_table for multivariate without initial available file", {
  if (file.exists(file_path_for_testing_multivariate)) {
    expect_true(file.remove(file_path_for_testing_multivariate))
  }
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  for (run in c("once", "twice")) {
    capture.output(
      function_output <- update_main_forecasting_table(
        file_path = file_path_for_testing_multivariate,
        data = function_input,
        min_train_periods = 15 * 12,
        fc_methods = c("linear"),
        verbose = F
      ),
      file = 'NUL'
    )
    expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(function_output), 48)
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
    expect_equal(class(function_output$fc_errors), "list")
    expect_equal(class(function_output$fc_models), "list")
    expected_models <- c("fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg")
    fc_models <- function_output$fc_models[[1]]
    expect_equal(class(fc_models), "list")
    expect_equal(names(fc_models), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
      expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
      expect_equal(nrow(fc_models[[exp_model]]$fc_data), 11)
      expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200512)
      expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200601)
      expect_equal(fc_models[[exp_model]]$fc_data$period[11], 200611)
      expect_equal(length(fc_models[[exp_model]]$fc_data$period), 11)
    }
    fc_models <- function_output$fc_models[[47]]
    expect_equal(class(fc_models), "list")
    expect_equal(names(fc_models), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
      expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
      expect_equal(nrow(fc_models[[exp_model]]$fc_data), 1)
      expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200610)
      expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200611)
      expect_equal(length(fc_models[[exp_model]]$fc_data$period), 1)
    }
    fc_models <- function_output$fc_models[[48]]
    expect_equal(fc_models, list())
  }
})

test_that("check update_main_forecasting_table for multivariate when extending on available file", {
  expect_true(file.exists(file_path_for_testing_multivariate))
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  for (run in c("once", "twice")) {
    expect_silent(
      function_output <- update_main_forecasting_table(
        file_path = file_path_for_testing_multivariate,
        data = function_input,
        min_train_periods = 15 * 12 + 8,
        fc_methods = c("linear", "prophet"),
        verbose = F,
        parallel = T
      )
    )
    expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(function_output), 16)
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
    expect_equal(class(function_output$fc_errors), "list")
    expect_equal(class(function_output$fc_models), "list")
    expected_models <- c(
      "fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg",
      "fc_prophet_005cps_xreg", "fc_prophet_050cps_xreg", "fc_prophet_500cps_xreg"
    )
    fc_models <- function_output$fc_models[[1]]
    expect_equal(class(fc_models), "list")
    expect_equal(names(fc_models), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
      expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
      expect_equal(nrow(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200608)
      expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200609)
      expect_equal(fc_models[[exp_model]]$fc_data$period[3], 200611)
      expect_equal(length(fc_models[[exp_model]]$fc_data$period), 3)
    }
    fc_models <- function_output$fc_models[[15]]
    expect_equal(class(fc_models), "list")
    expect_equal(names(fc_models), expected_models)
    for (exp_model in expected_models) {
      expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
      expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
      expect_equal(nrow(fc_models[[exp_model]]$fc_data), 1)
      expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
      expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200610)
      expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200611)
      expect_equal(length(fc_models[[exp_model]]$fc_data$period), 1)
    }
    fc_models <- function_output$fc_models[[16]]
    expect_equal(fc_models, list())
  }
})

test_that("check update_main_forecasting_table when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  saveRDS(
    object = dummy_gasprice,
    file = file_path_for_testing_invalid
  )
  expect_error(
    update_main_forecasting_table(
      file_path = file_path_for_testing_invalid,
      data = function_input,
      min_train_periods = 15 * 12
    )
  )
  saveRDS(
    object = "potato",
    file = file_path_for_testing_invalid
  )
  expect_error(
    update_main_forecasting_table(
      file_path = file_path_for_testing_invalid,
      data = function_input,
      min_train_periods = 15 * 12
    )
  )
  expect_error(
    update_main_forecasting_table(
      file_path = file_path_for_testing_univariate,
      data = function_input,
      min_train_periods = 15 * 12,
      fc_methods = c("basic")
    )
  )
  expect_error(
    update_main_forecasting_table(
      file_path = file_path_for_testing_multivariate,
      data = function_input,
      min_train_periods = 15 * 12,
      seasonal_periods = 3
    )
  )
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company")
  )
  expect_error(
    update_main_forecasting_table(
      file_path = file_path_for_testing_multivariate,
      data = function_input,
      min_train_periods = 15 * 12,
      fc_methods = c("linear")
    )
  )
  expect_error(
    update_main_forecasting_table(
      file_path = file_path_for_testing_univariate,
      data = function_input,
      min_train_periods = 15 * 12,
      seasonal_periods = 12
    )
  )
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice %>% 
      dplyr::filter(oil_company == "CompanyA"),
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = "state",
    xreg_cols = c("spotprice", "gemprice")
  )
  expect_error(
    update_main_forecasting_table(
      file_path = file_path_for_testing_multivariate,
      data = function_input,
      min_train_periods = 15 * 12
    )
  )
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice %>% 
      dplyr::filter(
        state == "New York",
        oil_company == "CompanyA"
      ),
    date_col = "year_month",
    col_of_interest = "gasprice"
  )
  expect_error(
    update_main_forecasting_table(
      file_path = file_path_for_testing_univariate,
      data = function_input,
      min_train_periods = 15 * 12
    )
  )
})
