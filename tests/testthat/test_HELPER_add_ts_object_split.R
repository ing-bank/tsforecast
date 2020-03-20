
context("add_ts_object_split")

test_that("check add_ts_object_split when valid inputs are used", {
  function_input <- tibble::tibble(
    ts_start = 199101,
    ts_split_date = 199308,
    ts_end = 200611,
    train_length = 32,
    valid_length = 159
  )
  ts_object <- tstools::initialize_ts_forecast_data(
       data = dummy_gasprice,
       date_col = "year_month",
       col_of_interest = "gasprice",
       group_cols = c("state", "oil_company")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>%
    dplyr::select(period, col_of_interest, grouping) %>%
    tstools::transform_data_to_ts_object(seasonal_periods = c(12, 3))
  function_output <- function_input %>%
    add_ts_object_split(
       type = "train",
       ts_object = ts_object
    ) %>% 
    add_ts_object_split(
      type = "valid",
      ts_object = ts_object
    )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 1)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c("ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$ts_object_train[[1]]), c("msts", "ts"))
  expect_equal(class(function_output$ts_object_valid[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[1]]), 32)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 1)
  expect_equal(nrow(function_output$ts_object_valid[[1]]), 159)
  expect_equal(ncol(function_output$ts_object_valid[[1]]), 1)
  
  function_input <- tibble::tibble(
    ts_start = 199101,
    ts_split_date = 200611,
    ts_end = 200611,
    train_length = 191,
    valid_length = 0
  )
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>%
    dplyr::select(period, col_of_interest, grouping, spotprice, gemprice) %>%
    tstools::transform_data_to_ts_object(seasonal_periods = 1)
  function_output <- function_input %>%
    add_ts_object_split(
      type = "train",
      ts_object = ts_object
    ) %>% 
    add_ts_object_split(
      type = "valid",
      ts_object = ts_object
    )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 1)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c("ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$ts_object_train[[1]]), c("mts", "ts", "matrix"))
  expect_equal(class(function_output$ts_object_valid[[1]]), "list")
  expect_equal(nrow(function_output$ts_object_train[[1]]), 191)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 3)
  expect_equal(length(function_output$ts_object_valid[[1]]), 0)
})

test_that("check add_ts_object_split when invalid inputs are used", {
  function_input <- tibble::tibble(
    ts_start = 199101,
    ts_split_date = 199308,
    ts_end = 200611,
    train_length = 32,
    valid_length = 159
  )
  ts_object <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB") %>%
    dplyr::select(period, col_of_interest, grouping) %>%
    tstools::transform_data_to_ts_object(seasonal_periods = c(12, 3))
  expect_error(
    add_ts_object_split(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    add_ts_object_split(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    add_ts_object_split(
      main_forecasting_table = function_input %>% 
        dplyr::select(-ts_split_date)
    )
  )
  expect_error(
    add_ts_object_split(
      main_forecasting_table = function_input %>% 
        dplyr::select(-train_length)
    )
  )
  expect_error(
    add_ts_object_split(
      main_forecasting_table = function_input %>% 
        dplyr::select(-valid_length)
    )
  )
  expect_error(
    add_ts_object_split(
      main_forecasting_table = function_input,
      type = "potato"
    )
  )
  expect_error(
    add_ts_object_split(
      main_forecasting_table = function_input,
      type = "train",
      ts_object = list()
    )
  )
  expect_error(
    add_ts_object_split(
      main_forecasting_table = function_input,
      type = "train",
      ts_object = dummy_gasprice
    )
  )
})
