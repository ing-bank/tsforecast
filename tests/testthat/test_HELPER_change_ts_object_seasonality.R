
context("change_ts_object_seasonality")

test_that("check change_ts_object_seasonality with valid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>% 
    dplyr::select(period, col_of_interest, grouping) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 1)
  function_output <- change_ts_object_seasonality(
    ts_object = function_input, 
    seasonality = c(12, 6, 3)
  )
  expect_equal(class(function_output), c("msts", "ts"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 1)
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(attr(function_output, "seasonality"), c(12, 6, 3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyB")
  expect_equal(attr(function_output, "xreg_cols"), character())

  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    dplyr::filter(grouping == "state = Indiana   &   oil_company = CompanyA") %>% 
    dplyr::select(period, col_of_interest, grouping, spotprice, gemprice) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12, 3))
  function_output <- change_ts_object_seasonality(
    ts_object = function_input, 
    seasonality = c(1, 1)
  )
  expect_equal(class(function_output), c("mts", "ts", "matrix"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(attr(function_output, "seasonality"), 1)
  expect_equal(attr(function_output, "grouping"), "state = Indiana   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
  
  function_output <- change_ts_object_seasonality(
    ts_object = list()
  )
  expect_equal(function_output, list())
})

test_that("check change_ts_object_seasonality with invalid data", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>% 
    dplyr::select(period, col_of_interest, grouping)
  expect_error(
    change_ts_object_seasonality(
      ts_object = dummy_gasprice
    )
  )
  expect_error(
    change_ts_object_seasonality(
      ts_object = function_input
    )
  )
  expect_error(
    change_ts_object_seasonality(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      seasonality = "a"
    ),
    "The parameter 'seasonality' should be a vector of positive integer values"
  )
  expect_error(
    change_ts_object_seasonality(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      seasonality = -1
    ),
    "The parameter 'seasonality' should be a vector of positive integer values"
  )
  expect_error(
    change_ts_object_seasonality(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      seasonality = 1.1
    ),
    "The parameter 'seasonality' should be a vector of positive integer values"
  )
  expect_error(
    change_ts_object_seasonality(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      seasonality = -1.1
    ),
    "The parameter 'seasonality' should be a vector of positive integer values"
  )
  expect_error(
    change_ts_object_seasonality(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      seasonality = 1.001
    ),
    "The parameter 'seasonality' should be a vector of positive integer values"
  )
})
