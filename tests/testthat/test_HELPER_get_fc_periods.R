
context("get_fc_periods")

apply_get_fc_periods_tests <- function(function_input) {
  function_output <- get_fc_periods(ts_object_train = function_input, periods_ahead = 1)
  expect_equal(length(function_output),1)
  expect_true(is.numeric(function_output))
  expect_equal(
    object = function_output, 
    expected = c(200612)
  )
  function_output <- get_fc_periods(ts_object_train = function_input, periods_ahead = 10)
  expect_equal(length(function_output),10)
  expect_true(is.numeric(function_output))
  expect_equal(
    object = function_output, 
    expected = c(
      200612, 200701, 200702, 200703, 200704, 200705, 200706, 200707, 200708, 200709
    )
  )
  function_output <- get_fc_periods(ts_object_train = function_input, periods_ahead = 20)
  expect_equal(length(function_output),20)
  expect_true(is.numeric(function_output))
  expect_equal(
    object = function_output, 
    expected = c(
      200612, 200701, 200702, 200703, 200704, 200705, 200706, 200707, 200708, 200709, 
      200710, 200711, 200712, 200801, 200802, 200803, 200804, 200805, 200806, 200807
    )
  )
  expect_error(
    get_fc_periods(periods_ahead = 10)
  )
  expect_error(
    get_fc_periods(ts_object_train = function_input)
  )
  expect_error(
    get_fc_periods(ts_object_train = function_input, periods_ahead = 0)
  )
  expect_error(
    get_fc_periods(ts_object_train = function_input, periods_ahead = 1.5)
  )
  expect_error(
    get_fc_periods(ts_object_train = function_input, periods_ahead = -1.5)
  )
  expect_error(
    get_fc_periods(ts_object_train = function_input, periods_ahead = -10)
  )
  expect_error(
    get_fc_periods(ts_object_train = function_input, periods_ahead = "string")
  )
  expect_error(
    get_fc_periods(ts_object_train = function_input, periods_ahead = as.Date("2018-05-09"))
  )
}

test_that("check get_fc_periods with a multivariate, uniseasonal time series object and differing periods_ahead", {
  tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3) %>% 
    apply_get_fc_periods_tests()
})

test_that("check get_fc_periods with a multivariate, multiseasonal time series object and differing periods_ahead", {
  tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3)) %>% 
    apply_get_fc_periods_tests()
})

test_that("check get_fc_periods with a univariate, multiseasonal time series object and differing periods_ahead", {
  tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3)) %>% 
    apply_get_fc_periods_tests()
})

test_that("check get_fc_periods with a univariate, uniseasonal time series object and differing periods_ahead", {
  tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 12) %>% 
    apply_get_fc_periods_tests()
})

test_that("check get_fc_periods when input data is not ts", {
  expect_error(
    get_fc_periods(
      ts_object_train = ts(c(1,2,3,4), frequency = 4),
      periods_ahead = 10
    )
  )
  expect_error(
    get_fc_periods(ts_object_train = dummy_gasprice, periods_ahead = 10)
  )
  expect_error(
    get_fc_periods(ts_object_train = list(), periods_ahead = 10)
  )
  expect_error(
    get_fc_periods(ts_object_train = "string", periods_ahead = 10)
  )
  expect_error(
    get_fc_periods(ts_object_train = 42, periods_ahead = 10)
  )
  expect_error(
    get_fc_periods(ts_object_train = as.Date("2018-05-09"), periods_ahead = 10)
  )
})
