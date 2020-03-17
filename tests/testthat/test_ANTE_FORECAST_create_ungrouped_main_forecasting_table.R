
context("create_ungrouped_main_forecasting_table")

test_that("check create_ungrouped_main_forecasting_table with a two xreg_cols and default train_periods", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  function_output <- create_ungrouped_main_forecasting_table(
    data = function_input,
    seasonal_periods = c(12,3),
    min_train_periods = 25,
    max_train_periods = Inf
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 167)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c("ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(min(function_output$train_length), 25)
  expect_equal(max(function_output$train_length), 191)
  expect_equal(min(function_output$valid_length), 0)
  expect_equal(max(function_output$valid_length), 166)
  expect_equal(class(function_output$ts_object_train[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[1]]), 25)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[1]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[1]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[1]])), 1993)
  expect_equal(attr(function_output$ts_object_train[[1]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[1]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[1]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_valid[[1]]), 166)
  expect_equal(ncol(function_output$ts_object_valid[[1]]), 3)
  expect_equal(colnames(function_output$ts_object_valid[[1]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_valid[[1]])), (1993 + 1/12))
  expect_equal(max(time(function_output$ts_object_valid[[1]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[1]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_valid[[1]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_valid[[1]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_train[[167]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[167]]), 191)
  expect_equal(ncol(function_output$ts_object_train[[167]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[167]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[167]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[167]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_train[[167]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[167]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[167]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[167]]), "list")
  expect_equal(length(function_output$ts_object_valid[[167]]), 0)
})

test_that("check create_ungrouped_main_forecasting_table with one xreg_col and limited train_periods", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = "gemprice"
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    dplyr::mutate(
      col_of_interest = ifelse(period >= as.Date('2006-06-30'), NA, col_of_interest)
    )
  function_output <- create_ungrouped_main_forecasting_table(
    data = function_input,
    seasonal_periods = c(1,3,12),
    min_train_periods = 42,
    max_train_periods = 50
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 144)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c("ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(min(function_output$train_length), 42)
  expect_equal(max(function_output$train_length), 50)
  expect_equal(min(function_output$valid_length), 0)
  expect_equal(max(function_output$valid_length), 143)
  expect_equal(class(function_output$ts_object_train[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[1]]), 42)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 2)
  expect_equal(colnames(function_output$ts_object_train[[1]]), c("col_of_interest", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[1]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[1]])), (1994 + 5/12))
  expect_equal(attr(function_output$ts_object_train[[1]], "seasonality"), c(1,3,12))
  expect_equal(attr(function_output$ts_object_train[[1]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[1]], "xreg_cols"), "gemprice")
  expect_equal(class(function_output$ts_object_valid[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_valid[[1]]), 149)
  expect_equal(ncol(function_output$ts_object_valid[[1]]), 2)
  expect_equal(colnames(function_output$ts_object_valid[[1]]), c("col_of_interest", "gemprice"))
  expect_equal(min(time(function_output$ts_object_valid[[1]])), (1994 + 6/12))
  expect_equal(max(time(function_output$ts_object_valid[[1]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[1]], "seasonality"), c(1,3,12))
  expect_equal(attr(function_output$ts_object_valid[[1]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_valid[[1]], "xreg_cols"), "gemprice")
  expect_equal(class(function_output$ts_object_train[[144]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[144]]), 50)
  expect_equal(ncol(function_output$ts_object_train[[144]]), 2)
  expect_equal(colnames(function_output$ts_object_train[[144]]), c("col_of_interest", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[144]])), (2002 + 3/12))
  expect_equal(max(time(function_output$ts_object_train[[144]])), (2006 + 4/12))
  expect_equal(attr(function_output$ts_object_train[[144]], "seasonality"), c(1,3,12))
  expect_equal(attr(function_output$ts_object_train[[144]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[144]], "xreg_cols"), c("gemprice"))
  expect_equal(class(function_output$ts_object_valid[[144]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_valid[[144]]), 6)
  expect_equal(ncol(function_output$ts_object_valid[[144]]), 2)
})

test_that("check create_ungrouped_main_forecasting_table with two xreg_cols and seasonality detection", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  function_output <- create_ungrouped_main_forecasting_table(
    data = function_input,
    seasonal_periods = 1,
    min_train_periods = 10,
    max_train_periods = 10
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 182)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c("ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(min(function_output$train_length), 10)
  expect_equal(max(function_output$train_length), 10)
  expect_equal(min(function_output$valid_length), 0)
  expect_equal(max(function_output$valid_length), 181)
  expect_equal(class(function_output$ts_object_train[[1]]), "ts")
  expect_equal(nrow(function_output$ts_object_train[[1]]), 10)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 1)
  expect_equal(colnames(function_output$ts_object_train[[1]]), "col_of_interest")
  expect_equal(min(time(function_output$ts_object_train[[1]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[1]])), (1991 + 9/12))
  expect_equal(attr(function_output$ts_object_train[[1]], "seasonality"), 1)
  expect_equal(attr(function_output$ts_object_train[[1]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[1]], "xreg_cols"), character())
  expect_equal(class(function_output$ts_object_valid[[1]]), "ts")
  expect_equal(nrow(function_output$ts_object_valid[[1]]), 181)
  expect_equal(ncol(function_output$ts_object_valid[[1]]), 1)
  expect_equal(colnames(function_output$ts_object_valid[[1]]), "col_of_interest")
  expect_equal(min(time(function_output$ts_object_valid[[1]])), (1991 + 10/12))
  expect_equal(max(time(function_output$ts_object_valid[[1]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[1]], "seasonality"), 1)
  expect_equal(attr(function_output$ts_object_valid[[1]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_valid[[1]], "xreg_cols"), character())
  expect_equal(class(function_output$ts_object_train[[182]]), "ts")
  expect_equal(nrow(function_output$ts_object_train[[182]]), 10)
  expect_equal(ncol(function_output$ts_object_train[[182]]), 1)
  expect_equal(colnames(function_output$ts_object_train[[182]]), "col_of_interest")
  expect_equal(min(time(function_output$ts_object_train[[182]])), (2006 + 1/12))
  expect_equal(max(time(function_output$ts_object_train[[182]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_train[[182]], "seasonality"), 1)
  expect_equal(attr(function_output$ts_object_train[[182]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[182]], "xreg_cols"), character())
  expect_equal(class(function_output$ts_object_valid[[182]]), "list")
  expect_equal(length(function_output$ts_object_valid[[182]]), 0)
})

test_that("check create_ungrouped_main_forecasting_table with two xreg_cols and limited and equal train_periods", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  function_output <- create_ungrouped_main_forecasting_table(
    data = function_input,
    seasonal_periods = NULL,
    min_train_periods = 90,
    max_train_periods = 96
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 102)
  expect_equal(ncol(function_output), 7)
  expect_equal(colnames(function_output), c("ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(min(function_output$train_length), 90)
  expect_equal(max(function_output$train_length), 96)
  expect_equal(min(function_output$valid_length), 0)
  expect_equal(max(function_output$valid_length), 101)
  expect_equal(class(function_output$ts_object_train[[1]]), c("mts", "ts", "matrix"))
  expect_equal(nrow(function_output$ts_object_train[[1]]), 90)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[1]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[1]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[1]])), (1998 + 5/12))
  expect_equal(attr(function_output$ts_object_train[[1]], "seasonality"), 3)
  expect_equal(attr(function_output$ts_object_train[[1]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[1]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[1]]), c("mts", "ts", "matrix"))
  expect_equal(nrow(function_output$ts_object_valid[[1]]), 101)
  expect_equal(ncol(function_output$ts_object_valid[[1]]), 3)
  expect_equal(colnames(function_output$ts_object_valid[[1]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_valid[[1]])), (1998 + 6/12))
  expect_equal(max(time(function_output$ts_object_valid[[1]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[1]], "seasonality"), 3)
  expect_equal(attr(function_output$ts_object_valid[[1]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_valid[[1]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_train[[102]]), c("mts", "ts", "matrix"))
  expect_equal(nrow(function_output$ts_object_train[[102]]), 96)
  expect_equal(ncol(function_output$ts_object_train[[102]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[102]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[102]])), (1998 + 11/12))
  expect_equal(max(time(function_output$ts_object_train[[102]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_train[[102]], "seasonality"), 12)
  expect_equal(attr(function_output$ts_object_train[[102]], "grouping"), "state = New York   &   oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[102]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[102]]), "list")
  expect_equal(length(function_output$ts_object_valid[[102]]), 0)
  for (i in 1:(nrow(function_output) - 1)) {
    expect_equal(
      attr(function_output$ts_object_train[[i]], "seasonality"),
      attr(function_output$ts_object_valid[[i]], "seasonality")
    )
  }
})

test_that("check create_ungrouped_main_forecasting_table when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = "potato"
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input %>% 
        dplyr::select(-period)
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input %>% 
        dplyr::select(-col_of_interest)
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input,
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = 48.5
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input,
      seasonal_periods = c(12,3),
      min_train_periods = 50,
      max_train_periods = 42
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input,
      seasonal_periods = c(12,3),
      min_train_periods = 1,
      max_train_periods = 100
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input,
      seasonal_periods = "string"
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input,
      seasonal_periods = 123
    )
  )
  expect_error(
    create_ungrouped_main_forecasting_table(
      data = function_input,
      min_train_periods = 1000
    )
  )
})
