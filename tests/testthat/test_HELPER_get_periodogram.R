
context("get_periodogram")

test_that("check get_periodogram with valid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>%
    dplyr::select(period, col_of_interest, grouping) %>%
    tstools::transform_data_to_ts_object(seasonal_periods = 1)
  function_output <- get_periodogram(function_input)
  expect_is(function_output, "spec")
  expect_equal(length(function_output), 16)
  expect_equal(names(function_output), c(
    "freq", "spec", "coh", "phase", "kernel", "df", "bandwidth", 
    "n.used", "orig.n", "series", "snames", "method", "taper", "pad", 
    "detrend", "demean"
  ))
  expect_equal(round(min(function_output$freq), 5), 0.00521)
  expect_equal(round(max(function_output$freq), 5), 0.50000)
  expect_equal(round(mean(function_output$freq), 5), 0.25260)
  expect_equal(round(sd(function_output$freq), 5), 0.14509)
  expect_equal(round(min(function_output$spec), 5), 0.00026)
  expect_equal(round(max(function_output$spec), 5), 0.44992)
  expect_equal(round(mean(function_output$spec), 5), 0.00881)
  expect_equal(round(sd(function_output$spec), 5), 0.04987)
})

test_that("check get_periodogram with invalid input data that is not a univariate time series", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") 
  expect_error(
    get_periodogram(list())
  ) 
  expect_error(
    get_periodogram(dummy_gasprice)
  ) 
  expect_error(
    get_periodogram(function_input)
  )
  expect_error(
    get_periodogram(
      function_input %>% 
        dplyr::select(period, col_of_interest, grouping, gemprice) %>%
        tstools::transform_data_to_ts_object(seasonal_periods = 1)
    )
  )
  expect_error(
    get_periodogram(
      function_input %>% 
        dplyr::select(period, col_of_interest, grouping) %>%
        tstools::transform_data_to_ts_object(seasonal_periods = c(12,3)),
      type = "ponies"
    )
  )
})
