
context("union_ts_objects")

test_that("check union_ts_objects for training data with multivariate, multiseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = nrow(function_input) - 12,
      from_left = T
    )
  ts_part_two <- function_input %>% 
    trim_ts_object(
      max_length = 12,
      from_left = F
    )
  function_output <- union_ts_objects(
    ts_object_1 = ts_part_one,
    ts_object_2 = ts_part_two
  )
  expect_equivalent(function_input,function_output)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), 2006 + 10/12)
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
  function_output <- union_ts_objects(
    ts_object_1 = ts_part_two,
    ts_object_2 = ts_part_one
  )
  expect_equivalent(function_input,function_output)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), 2006 + 10/12)
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})
  
test_that("check union_ts_objects for validation data with multivariate, uniseasonal ts datae", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = nrow(function_input) - 1,
      from_left = T
    )
  ts_part_two <- function_input %>% 
    trim_ts_object(
      max_length = 1,
      from_left = F
    )
  function_output <- union_ts_objects(
    ts_object_1 = ts_part_one,
    ts_object_2 = ts_part_two
  )
  expect_equivalent(function_input,function_output)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), 2006 + 10/12)
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check union_ts_objects for both with univariate, multiseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12, 3))
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = 1,
      from_left = T
    )
  ts_part_two <- function_input %>% 
    trim_ts_object(
      max_length = nrow(function_input) - 1,
      from_left = F
    )
  function_output <- union_ts_objects(
    ts_object_1 = ts_part_one,
    ts_object_2 = ts_part_two
  )
  expect_equivalent(function_input,function_output)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 1)
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), 2006 + 10/12)
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check union_ts_objects for training data with univariate, uniseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 1)
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = floor(nrow(function_input)/2),
      from_left = T
    )
  ts_part_two <- function_input %>% 
    trim_ts_object(
      max_length = ceiling(nrow(function_input)/2),
      from_left = F
    )
  function_output <- union_ts_objects(
    ts_object_1 = ts_part_one,
    ts_object_2 = ts_part_two
  )
  expect_equivalent(function_input,function_output)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 1)
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), 2006 + 10/12)
  expect_equal(attr(function_output, "seasonality"), 1)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check union_ts_objects with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = 1,
      from_left = T
    )
  ts_part_two <- function_input %>% 
    trim_ts_object(
      max_length = 1,
      from_left = F
    )
  expect_error(
    union_ts_objects(
      ts_object_1 = ts_part_one,
      ts_object_2 = ts_part_two
    )
  )
  expect_error(
    union_ts_objects(
      ts_object_1 = function_input,
      ts_object_2 = function_input
    )
  )
  expect_error(
    union_ts_objects(
      ts_object_1 = function_input,
      ts_object_2 = dummy_gasprice
    )
  )
  expect_error(
    union_ts_objects(
      ts_object_1 = list(),
      ts_object_2 = function_input
    )
  )
  alt_function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12))
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = nrow(function_input) - 12,
      from_left = T
    )
  ts_part_two <- alt_function_input %>% 
    trim_ts_object(
      max_length = 12,
      from_left = F
    )
  expect_error(
    union_ts_objects(
      ts_object_1 = ts_part_one,
      ts_object_2 = ts_part_two
    )
  )
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(3))
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = nrow(function_input) - 12,
      from_left = T
    )
  ts_part_two <- alt_function_input %>% 
    trim_ts_object(
      max_length = 12,
      from_left = F
    )
  expect_error(
    union_ts_objects(
      ts_object_1 = ts_part_one,
      ts_object_2 = ts_part_two
    )
  )
  alt_function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyB") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(3))
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = nrow(function_input) - 12,
      from_left = T
    )
  ts_part_two <- alt_function_input %>% 
    trim_ts_object(
      max_length = 12,
      from_left = F
    )
  expect_error(
    union_ts_objects(
      ts_object_1 = ts_part_one,
      ts_object_2 = ts_part_two
    )
  )
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyB") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(3))
  ts_part_one <- function_input %>% 
    trim_ts_object(
      max_length = nrow(function_input) - 12,
      from_left = T
    )
  ts_part_two <- alt_function_input %>% 
    trim_ts_object(
      max_length = 12,
      from_left = F
    )
  expect_error(
    union_ts_objects(
      ts_object_1 = ts_part_one,
      ts_object_2 = ts_part_two
    )
  )
})
