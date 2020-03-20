
context("get_seasonality_additive")

test_that("check get_seasonality_additive with valid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>% 
    dplyr::select(period, col_of_interest, grouping) %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 1)
  function_output <- get_seasonality_additive(
    ts_object = function_input, 
    top_num = 1
  )
  expect_equal(function_output, 12)
  function_output <- get_seasonality_additive(
    ts_object = function_input, 
    top_num = 3
  )
  expect_equal(function_output, c(12, 4, 6))
  expect_warning(
    function_output <- get_seasonality_additive(
       ts_object = function_input, 
       top_num = 42
    )
  )
  expect_equal(function_output, c(12, 4, 6, 2, 3, 24, 48, 16, 8, 64, 32))
   
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
  function_output <- get_seasonality_additive(
    ts_object = function_input, 
    top_num = 1
  )
  expect_equal(function_output, 12)
  function_output <- get_seasonality_additive(
    ts_object = function_input, 
    top_num = 3
  )
  expect_equal(function_output, c(12, 2, 6))
  expect_warning(
    function_output <- get_seasonality_additive(
      ts_object = function_input, 
      top_num = 42
    ),
    "top_num is larger than the total number of seasonality patterns detected, so seasonal_periods contains all patterns"
  )
  expect_equal(function_output, c(12, 2, 6, 3, 4, 24, 8, 48, 16, 64, 32))
})

test_that("check get_seasonality_additive with invalid data", {
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
    get_seasonality_additive(
      ts_object = list()
    )
  )
  expect_error(
    get_seasonality_additive(
      ts_object = dummy_gasprice
    )
  )
  expect_error(
    get_seasonality_additive(
      ts_object = function_input
    )
  )
  expect_error(
    get_seasonality_additive(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      top_num = "a"
    ),
    'Given top_num is not a number, please assign an integer number to it!'
  )
  expect_error(
    get_seasonality_additive(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      top_num = -1
    ),
    'Given top_num is a negative number, please assign a positive integer number to it!'
  )
  expect_error(
    get_seasonality_additive(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      top_num = 1.1
    ),
    'Given top_num is not an integer number, please assign an integer number to it!'
  )
  expect_error(
    get_seasonality_additive(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      top_num = -1.1
    ),
    'Given top_num is a negative number, please assign a positive integer number to it!'
  )
  expect_error(
    get_seasonality_additive(
      ts_object = function_input %>% 
        tstools::transform_data_to_ts_object(seasonal_periods = 1),
      top_num = 1.001
    ),
    'Given top_num is not an integer number, please assign an integer number to it!'
  )
})
