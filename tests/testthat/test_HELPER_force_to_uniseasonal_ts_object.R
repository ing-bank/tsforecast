
context("force_to_uniseasonal_ts_object")

test_that("check force_to_uniseasonal_ts_object with an msts object of seasonality 3 and 12", {
  ts_object <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"), 
    xreg_cols = c("spotprice", "gemprice")
  ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(3,12))
  function_output <- force_to_uniseasonal_ts_object(ts_object = ts_object)
  expect_true(is.ts(function_output))
  expect_true(!("msts" %in% class(function_output)))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check force_to_uniseasonal_ts_object with an msts object of seasonality 12 and 3", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  function_output <- force_to_uniseasonal_ts_object(ts_object = ts_object)
  expect_true(is.ts(function_output))
  expect_true(!("msts" %in% class(function_output)))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(attr(function_output, "seasonality"), 12)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check force_to_uniseasonal_ts_object with an ts object of seasonality 3", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  function_output <- force_to_uniseasonal_ts_object(ts_object = ts_object)
  expect_true(is.ts(function_output))
  expect_equal(function_output,ts_object)
  expect_true(!("msts" %in% class(function_output)))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check force_to_uniseasonal_ts_object with an ts object of seasonality 12", {
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12)
  function_output <- force_to_uniseasonal_ts_object(ts_object = ts_object)
  expect_true(is.ts(function_output))
  expect_equal(function_output,ts_object)
  expect_true(!("msts" %in% class(function_output)))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(attr(function_output, "seasonality"), 12)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check force_to_uniseasonal_ts_object with non-ts objects", {
  expect_error(
    force_to_uniseasonal_ts_object(ts_object = dummy_gasprice)
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
    force_to_uniseasonal_ts_object(ts_object = non_ts_object)
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
    force_to_uniseasonal_ts_object(ts_object = non_ts_object)
  )
})
