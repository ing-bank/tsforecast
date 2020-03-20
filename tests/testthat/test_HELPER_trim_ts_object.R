
context("trim_ts_object")

apply_trim_ts_object_tests <- function(function_input, n_cols = 1) {
  function_output <- trim_ts_object(
    ts_object = function_input, 
    max_length = 42,
    from_left = T
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 42)
  expect_equal(ncol(function_output), n_cols)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice")[1:n_cols])
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), (1994 + 5/12))
  function_output <- trim_ts_object(
    ts_object = function_input, 
    max_length = 42,
    from_left = F
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 42)
  expect_equal(ncol(function_output), n_cols)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice")[1:n_cols])
  expect_equal(min(time(function_output)), (2003 + 5/12))
  expect_equal(max(time(function_output)), (2006 + 10/12))
  function_output <- trim_ts_object(
    ts_object = function_input, 
    max_length = 1000,
    from_left = T
  )
  function_output_alt <- trim_ts_object(
    ts_object = function_input, 
    from_left = F
  )
  expect_equivalent(function_output, function_output_alt)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), n_cols)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice")[1:n_cols])
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), (2006 + 10/12))
  return(function_output) # For extra tests
}

test_that("check trim_ts_object with a multivariate, uniseasonal time series object and differing max_length", {
  function_output <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3) %>% 
    apply_trim_ts_object_tests(n_cols = 3)
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check trim_ts_object with a multivariate, multiseasonal time series object and differing max_length", {
  function_output <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company"), 
    xreg_cols = c("spotprice", "gemprice")
  ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3)) %>% 
    apply_trim_ts_object_tests(n_cols = 3)
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check trim_ts_object with a univariate, multiseasonal time series object and differing max_length", {
  function_output <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company")
  ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3)) %>% 
    apply_trim_ts_object_tests()
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check trim_ts_object with a univariate, uniseasonal time series object and differing max_length", {
  function_output <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice", 
    group_cols = c("state", "oil_company")
  ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12) %>% 
    apply_trim_ts_object_tests()
  expect_equal(attr(function_output, "seasonality"), 12)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check trim_ts_object with invalid max_length", {
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
    trim_ts_object(
      ts_object = function_input, 
      max_length = -10
    )
  )
  expect_error(
    trim_ts_object(
      ts_object = function_input, 
      max_length = 0
    )
  )
  expect_error(
    trim_ts_object(
      ts_object = function_input, 
      max_length = -Inf
    )
  )
  expect_error(
    trim_ts_object(
      ts_object = function_input, 
      max_length = 2.5
    )
  )
  expect_error(
    trim_ts_object(
      ts_object = function_input, 
      max_length = "3"
    )
  )
  expect_error(
    trim_ts_object(
      ts_object = function_input, 
      max_length = as.Date("2018-05-09")
    )
  )
})

test_that("check trim_ts_object when input data is not ts", {
  expect_error(
    trim_ts_object(ts_object = dummy_gasprice)
  )
  expect_error(
    trim_ts_object(ts_object = list())
  )
  expect_error(
    trim_ts_object(ts_object = "string")
  )
  expect_error(
    trim_ts_object(ts_object = 42)
  )
  expect_error(
    trim_ts_object(ts_object = as.Date("2018-05-09"))
  )
})
