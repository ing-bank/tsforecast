
context("ts_object_to_tibble")

test_that("check ts_object_to_tibble with multivariate, multiseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  function_output <- ts_object_to_tibble(ts_object = function_input)
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 4)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "spotprice", "gemprice"))
  expect_true(class(function_output$period) == "numeric")
  expect_true(class(function_output$col_of_interest) == "numeric")
  expect_true(class(function_output$spotprice) == "numeric")
  expect_true(class(function_output$gemprice) == "numeric")
})

test_that("check ts_object_to_tibble with multivariate, uniseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  function_output <- ts_object_to_tibble(ts_object = function_input)
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 4)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "spotprice", "gemprice"))
  expect_true(class(function_output$period) == "numeric")
  expect_true(class(function_output$col_of_interest) == "numeric")
  expect_true(class(function_output$spotprice) == "numeric")
  expect_true(class(function_output$gemprice) == "numeric")
})

test_that("check ts_object_to_tibble with univariate, multiseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12, 3))
  function_output <- ts_object_to_tibble(ts_object = function_input)
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 2)
  expect_equal(colnames(function_output), c("period", "col_of_interest"))
  expect_true(class(function_output$period) == "numeric")
  expect_true(class(function_output$col_of_interest) == "numeric")
})

test_that("check ts_object_to_tibble with univariate, uniseasonal ts data and valid split date", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 1)
  function_output <- ts_object_to_tibble(ts_object = function_input)
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 2)
  expect_equal(colnames(function_output), c("period", "col_of_interest"))
  expect_true(class(function_output$period) == "numeric")
  expect_true(class(function_output$col_of_interest) == "numeric")
})

test_that("check ts_object_to_tibble when input data is not ts", {
  expect_error(
    ts_object_to_tibble(
      ts_object = dummy_gasprice
    )
  )
  expect_error(
    ts_object_to_tibble(
      ts_object = list()
    )
  )
  expect_error(
    ts_object_to_tibble(
      ts_object = "string"
    )
  )
  expect_error(
    ts_object_to_tibble(
      ts_object = 42
    )
  )
  expect_error(
    ts_object_to_tibble(
      ts_object = as.Date("2018-05-09")
    )
  )
})
