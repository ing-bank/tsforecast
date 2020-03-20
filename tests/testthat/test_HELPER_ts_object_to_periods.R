
context("ts_object_to_periods")

test_that("check ts_object_to_periods with multivariate, multiseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  function_output <- ts_object_to_periods(ts_object = function_input)
  expect_equal(length(function_output), 191)
  expect_true(is.numeric(function_output))
})

test_that("check ts_object_to_periods with multivariate, uniseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  function_output <- ts_object_to_periods(ts_object = function_input)
  expect_equal(length(function_output), 191)
  expect_true(is.numeric(function_output))
})

test_that("check ts_object_to_periods with univariate, multiseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12, 3))
  function_output <- ts_object_to_periods(ts_object = function_input)
  expect_equal(length(function_output), 191)
  expect_true(is.numeric(function_output))
})

test_that("check ts_object_to_periods with univariate, uniseasonal ts data and valid split date", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 1)
  function_output <- ts_object_to_periods(ts_object = function_input)
  expect_equal(length(function_output), 191)
  expect_true(is.numeric(function_output))
})

test_that("check ts_object_to_periods with ts data full of missing", {
  function_input <- ts(data = c(NA, NA, NA))
  function_output <- ts_object_to_periods(ts_object = function_input)
  expect_equal(length(function_output), 3)
  expect_true(is.numeric(function_output))
})

test_that("check ts_object_to_periods when input data is not ts", {
  expect_error(
    ts_object_to_periods(
      ts_object = dummy_gasprice
    )
  )
  expect_error(
    ts_object_to_periods(
      ts_object = list()
    )
  )
  expect_error(
    ts_object_to_periods(
      ts_object = "string"
    )
  )
  expect_error(
    ts_object_to_periods(
      ts_object = 42
    )
  )
  expect_error(
    ts_object_to_periods(
      ts_object = as.Date("2018-05-09")
    )
  )
})
