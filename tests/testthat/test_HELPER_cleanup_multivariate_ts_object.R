
context("cleanup_multivariate_ts_object")

test_that("check cleanup_multivariate_ts_object with multiseasonal, multivariate ts object", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  function_output <- cleanup_multivariate_ts_object(ts_object = ts_object)
  expect_equal(class(function_output), c("msts", "ts"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check cleanup_multivariate_ts_object with uniseasonal, multivariate ts object", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = "gemprice"
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  function_output <- cleanup_multivariate_ts_object(ts_object = ts_object)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 2)
  expect_equal(colnames(function_output), c("col_of_interest", "gemprice"))
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), "gemprice")
})

test_that("check cleanup_multivariate_ts_object with uniseasonal, univariate ts object", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    dplyr::mutate(original_col_of_interest = col_of_interest) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12)
  function_output <- cleanup_multivariate_ts_object(ts_object = ts_object)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 1)
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(attr(function_output, "seasonality"), 12)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check cleanup_multivariate_ts_object with multiseasonal, univariate ts object", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  function_output <- cleanup_multivariate_ts_object(ts_object = ts_object)
  expect_equal(ts_object, function_output)
  expect_equal(class(function_output), c("msts", "ts"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 1)
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check cleanup_multivariate_ts_object with non-ts objects", {
  expect_error(
    cleanup_multivariate_ts_object(ts_object = dummy_gasprice)
  )
  non_ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") 
  expect_error(
    cleanup_multivariate_ts_object(ts_object = non_ts_object)
  ) 
  non_ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3)) %>% 
    as.vector()
  expect_error(
    cleanup_multivariate_ts_object(ts_object = non_ts_object)
  )
})
