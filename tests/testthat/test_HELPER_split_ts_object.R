
context("split_ts_object")

test_that("check split_ts_object for training data with multivariate, multiseasonal ts data and valid split date", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12,3))
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 200101, 
    output = "train", 
    max_length = Inf
  )
  function_output_alt <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = as.Date("2001-01-13"), 
    output = "train", 
    max_length = Inf
  )
  expect_equivalent(function_output,function_output_alt)
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 121)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), 2001)
  expect_equal(attr(function_output, "seasonality"), c(12,3))
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})
  
test_that("check split_ts_object for validation data with multivariate, uniseasonal ts data and valid split date", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"), 
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 200610, 
    output = "valid", 
    max_length = Inf
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 1)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), (2007 - 2/12))
  expect_equal(max(time(function_output)), (2007 - 2/12))
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check split_ts_object for both with univariate, multiseasonal ts data and valid split date", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = c(12, 3))
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 200012, 
    output = "both", 
    max_length = Inf
  )
  function_output_alt <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = as.Date("2000-12-13"), 
    output = "both", 
    max_length = Inf
  )
  expect_equivalent(function_output,function_output_alt)
  expect_true(is.list(function_output))
  expect_equal(length(function_output), 2)
  expect_equal(names(function_output), c("train", "valid"))
  expect_true(is.ts(function_output$train))
  expect_equal(nrow(function_output$train), 120)
  expect_equal(ncol(function_output$train), 1)
  expect_equal(colnames(function_output$train), "col_of_interest")
  expect_equal(max(time(function_output$train)), (2001 - 1/12))
  expect_equal(attr(function_output$train, "seasonality"), c(12,3))
  expect_equal(attr(function_output$train, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$train, "xreg_cols"), character())
  expect_true(is.ts(function_output$valid))
  expect_equal(nrow(function_output$valid), 71)
  expect_equal(ncol(function_output$valid), 1)
  expect_equal(colnames(function_output$valid), "col_of_interest")
  expect_equal(attr(function_output$valid, "seasonality"), c(12,3))
  expect_equal(attr(function_output$valid, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$valid, "xreg_cols"), character())
})

test_that("check split_ts_object for training data with univariate, uniseasonal ts data and valid split date", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 1)
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 199101, 
    output = "train", 
    max_length = Inf
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 1)
  expect_equal(ncol(function_output), 1)
  expect_equal(colnames(function_output), "col_of_interest")
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), 1991)
  expect_equal(attr(function_output, "seasonality"), 1)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), character())
})

test_that("check split_ts_object with different max_length arguments", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice, 
      date_col = "year_month", 
      col_of_interest = "gasprice", 
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object(seasonal_periods = 3)
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 200101, 
    output = "train", 
    max_length = 42
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 42)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), (1997 + 7/12))
  expect_equal(max(time(function_output)), 2001)
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 200101, 
    output = "valid", 
    max_length = 42
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 42)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), (2001 + 1/12))
  expect_equal(max(time(function_output)), 2004.5)
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 199101, 
    output = "train",
    max_length = NA
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 1)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), 1991)
  expect_equal(max(time(function_output)), 1991)
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 199101, 
    output = "valid",
    max_length = 1000
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 190)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), (1991 + 1/12))
  expect_equal(max(time(function_output)), (2006 + 10/12))
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 200610, 
    output = "valid", 
    max_length = 42
  )
  expect_true(is.ts(function_output))
  expect_equal(nrow(function_output), 1)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output)), (2006 + 10/12))
  expect_equal(max(time(function_output)), (2006 + 10/12))
  expect_equal(attr(function_output, "seasonality"), 3)
  expect_equal(attr(function_output, "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output, "xreg_cols"), c("spotprice", "gemprice"))
  function_output <- split_ts_object(
    ts_object = function_input, 
    ts_split_date = 200611, 
    output = "valid"
  )
  expect_true(is.list(function_output))
  expect_equal(length(function_output), 0)
})

test_that("check split_ts_object with invalid split dates", {
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
    split_ts_object(
      ts_object = function_input, 
      ts_split_date = "200101", 
      output = "both", 
      max_length = Inf
    )
  )
  expect_error(
    split_ts_object(
      ts_object = function_input, 
      ts_split_date = "2001-01-01", 
      output = "both", 
      max_length = Inf
    )
  )
  expect_error(
    split_ts_object(
      ts_object = function_input, 
      ts_split_date = 2001, 
      output = "both", 
      max_length = Inf
    )
  )
  expect_error(
    split_ts_object(
      ts_object = function_input, 
      ts_split_date = 20010101, 
      output = "both", 
      max_length = Inf
    )
  )
  expect_error(
    split_ts_object(
      ts_object = function_input, 
      ts_split_date = 199012, 
      output = "both", 
      max_length = Inf
    )
  )
  expect_error(
    split_ts_object(
      ts_object = function_input, 
      ts_split_date = as.Date("2006-12-01"), 
      output = "both", 
      max_length = Inf
    )
  )
})

test_that("check split_ts_object when input data is not ts", {
  expect_error(
    split_ts_object(
      ts_object = dummy_gasprice, 
      ts_split_date = 200101, 
      output = "train", 
      max_length = Inf
    )
  )
  expect_error(
    split_ts_object(
      ts_object = list(), 
      ts_split_date = 199101, 
      output = "valid", 
      max_length = Inf
    )
  )
  expect_error(
    split_ts_object(
      ts_object = "string", 
      ts_split_date = as.Date("2001-01-13"), 
      output = "both", 
      max_length = Inf
    )
  )
  expect_error(
    split_ts_object(
      ts_object = 42, 
      ts_split_date = 200101, 
      output = "valid", 
      max_length = 50
    )
  )
  expect_error(
    split_ts_object(
      ts_object = as.Date("2018-05-09"), 
      ts_split_date = as.Date("2018-05-09"), 
      output = "train", 
      max_length = 50
    )
  )
})
