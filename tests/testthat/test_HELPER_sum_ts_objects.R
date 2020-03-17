
context("sum_ts_objects")

test_that("check sum_ts_objects for training data with multivariate, multiseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"), 
    xreg_cols = c("spotprice", "gemprice")
  )
  ts_part_one <- function_input %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    dplyr::select(-spotprice) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_part_two <- function_input %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyA") %>% 
    dplyr::select(-gemprice) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_part_three <- function_input %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_part_four <- function_input %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyB") %>%
    dplyr::select(-spotprice, -gemprice) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_objects <- list(ts_part_one, ts_part_two, ts_part_three, ts_part_four)
  for (i in 1:4) {
    function_output <- sum_ts_objects(
      ts_objects = ts_objects[1:i],
      new_grouping = "testing"
    )
    expect_true(is.ts(function_output))
    expect_equal(nrow(function_output), 191)
    expect_true(ncol(function_output) %in% c(2, 3))
    expect_true(all(colnames(function_output) %in% c("col_of_interest", "spotprice", "gemprice")))
    expect_equal(min(time(function_output)), 1991)
    expect_equal(max(time(function_output)), 2006 + 10/12)
    expect_equal(attr(function_output, "seasonality"), c(12,3))
    expect_equal(attr(function_output, "grouping"), "testing")
    expect_true(all(attr(function_output, "xreg_cols") %in% c("spotprice", "gemprice")))
  }
})
  
test_that("check sum_ts_objects for validation data with multivariate, uniseasonal ts datae", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"), 
    xreg_cols = c("spotprice", "gemprice")
  )
  ts_part_one <- function_input %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  ts_part_two <- function_input %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyA") %>% 
    dplyr::filter(period <= "2006-01-01") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  ts_part_three <- function_input %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  ts_part_four <- function_input %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyB") %>% 
    dplyr::filter(period >= "1992-01-01") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  ts_objects <- list(ts_part_one, ts_part_two, ts_part_three, ts_part_four)
  for (i in 1:4) {
    function_output <- sum_ts_objects(
      ts_objects = ts_objects[1:i],
      new_grouping = "testing"
    )
    expect_true(is.ts(function_output))
    expect_equal(nrow(function_output), 191)
    expect_equal(ncol(function_output), 3)
    expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
    expect_equal(min(time(function_output)), 1991)
    expect_equal(max(time(function_output)), 2006 + 10/12)
    expect_equal(attr(function_output, "seasonality"), 3)
    expect_equal(attr(function_output, "grouping"), "testing")
    expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
  }
})

test_that("check sum_ts_objects for both with univariate, multiseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company")
  )
  ts_part_one <- function_input %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    dplyr::filter(period <= "2006-01-01") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_part_two <- function_input %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_part_three <- function_input %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>% 
    dplyr::filter(period >= "1992-01-01") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_part_four <- function_input %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyB") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  ts_objects <- list(ts_part_one, ts_part_two, ts_part_three, ts_part_four)
  for (i in 1:4) {
    function_output <- sum_ts_objects(
      ts_objects = ts_objects[1:i],
      new_grouping = "testing"
    )
    expect_true(is.ts(function_output))
    expect_true(nrow(function_output) %in% c(180, 191))
    expect_equal(ncol(function_output), 1)
    expect_equal(colnames(function_output), "col_of_interest")
    expect_equal(min(time(function_output)), 1991)
    expect_true(max(time(function_output)) %in% c(2005 + 11/12 ,2006 + 10/12))
    expect_equal(attr(function_output, "seasonality"), c(12,3))
    expect_equal(attr(function_output, "grouping"), "testing")
    expect_equal(attr(function_output, "xreg_cols"), character())
  }
})

test_that("check sum_ts_objects for training data with univariate, uniseasonal ts data", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company")
  )
  ts_part_one <- function_input %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12)
  ts_part_two <- function_input %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12)
  ts_part_three <- function_input %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12)
  ts_part_four <- function_input %>% 
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyB") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12)
  ts_objects <- list(ts_part_one, ts_part_two, ts_part_three, ts_part_four)
  for (i in 1:4) {
    function_output <- sum_ts_objects(
      ts_objects = ts_objects[1:i],
      new_grouping = "testing"
    )
    expect_true(is.ts(function_output))
    expect_equal(nrow(function_output), 191)
    expect_equal(ncol(function_output), 1)
    expect_equal(colnames(function_output), "col_of_interest")
    expect_equal(min(time(function_output)), 1991)
    expect_equal(max(time(function_output)), 2006 + 10/12)
    expect_equal(attr(function_output, "seasonality"), 12)
    expect_equal(attr(function_output, "grouping"), "testing")
    expect_equal(attr(function_output, "xreg_cols"), character())
  }
})

test_that("check sum_ts_objects with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  expect_error(
    sum_ts_objects(
      ts_objects = "potato"
    )
  )
  expect_error(
    sum_ts_objects(
      ts_objects = list()
    )
  )
  expect_error(
    sum_ts_objects(
      ts_objects = list(
        function_input,
        dummy_gasprice
      ),
      new_grouping = "testing"
    )
  )
  expect_error(
    sum_ts_objects(
      ts_objects = list(
        function_input,
        list()
      ),
      new_grouping = "testing"
    )
  )
  expect_error(
    sum_ts_objects(
      ts_objects = list(
        function_input,
        function_input
      ),
      new_grouping = 42
    )
  )
})
