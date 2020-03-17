
context("add_missing_attributes_to_ts_object")

test_that("check add_missing_attributes_to_ts_object with multiseasonal, multivariate ts object", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  new_ts_object <- window(
    x = ts_object,
    end = c(2001, 1)
  )
  function_output <- add_missing_attributes_to_ts_object(
    new_ts_object = new_ts_object,
    prev_ts_object = ts_object
  )
  expect_equal(class(function_output), c("msts", "ts"))
  expect_equal(nrow(function_output), 121)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest","spotprice","gemprice"))
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice","gemprice"))
})

test_that("check add_missing_attributes_to_ts_object with uniseasonal, multivariate ts object", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  new_ts_object <- window(
    x = ts_object,
    end = c(2001, 1)
  )
  function_output <- add_missing_attributes_to_ts_object(
    new_ts_object = new_ts_object,
    prev_ts_object = ts_object
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 121)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest","spotprice","gemprice"))
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice","gemprice"))
})

test_that("check add_missing_attributes_to_ts_object with uniseasonal, univariate ts object", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12)
  new_ts_object <- window(
    x = ts_object,
    end = c(2001, 1)
  )
  function_output <- add_missing_attributes_to_ts_object(
    new_ts_object = new_ts_object,
    prev_ts_object = ts_object
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 121)
  expect_equal(ncol(function_output), 1)
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(attr(function_output, "seasonality"), 12)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check add_missing_attributes_to_ts_object with multiseasonal, univariate ts object", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  new_ts_object <- window(
    x = ts_object,
    end = c(2001, 1)
  )
  function_output <- add_missing_attributes_to_ts_object(
    new_ts_object = new_ts_object,
    prev_ts_object = ts_object
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 121)
  expect_equal(ncol(function_output), 1)
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check add_missing_attributes_to_ts_object with non-ts objects", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12)
  expect_error(
    add_missing_attributes_to_ts_object(
      new_ts_object = dummy_gasprice,
      prev_ts_object = ts_object
    )
  )
  expect_error(
    add_missing_attributes_to_ts_object(
      new_ts_object = ts_object,
      prev_ts_object = dummy_gasprice
    )
  )
})
