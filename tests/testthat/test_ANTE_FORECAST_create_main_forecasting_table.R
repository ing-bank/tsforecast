
context("create_main_forecasting_table")

test_that("check create_main_forecasting_table with valid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  function_output <- create_main_forecasting_table(
    data = function_input,
    seasonal_periods = c(12,3), 
    min_train_periods = 25, 
    max_train_periods = Inf
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 4*167)
  expect_equal(ncol(function_output), 8)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(unique(function_output$grouping), c(
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB", 
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
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
  expect_equal(class(function_output$ts_object_train[[668]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[668]]), 191)
  expect_equal(ncol(function_output$ts_object_train[[668]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[668]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[668]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[668]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_train[[668]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[668]], "grouping"), "state = Indiana   &   oil_company = CompanyB")
  expect_equal(attr(function_output$ts_object_train[[668]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[668]]), "list")
  expect_equal(length(function_output$ts_object_valid[[668]]), 0)
})

test_that("check create_main_forecasting_table with xreg forecasts", {
  data <- dummy_gasprice %>% 
    dplyr::mutate(gasprice = ifelse(
        test = year_month > as.Date("2005-11-30"), 
        yes = NA,
        no = gasprice
      )
    ) %>% 
    dplyr::filter(state == "New York")
  function_input <- tstools::initialize_ts_forecast_data(
    data = data, 
    date_col = "year_month", 
    col_of_interest = "gasprice",
    group_cols = "oil_company",
    xreg_cols = c("spotprice", "gemprice")
  )
  function_output <- create_main_forecasting_table(
    data = function_input,
    seasonal_periods = c(12,3), 
    min_train_periods = 25, 
    max_train_periods = Inf
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 2*155)
  expect_equal(ncol(function_output), 8)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(unique(function_output$grouping), c(
    "oil_company = CompanyA", 
    "oil_company = CompanyB"
  ))
  expect_equal(min(function_output$train_length), 25)
  expect_equal(max(function_output$train_length), 179)
  expect_equal(min(function_output$valid_length), 0)
  expect_equal(max(function_output$valid_length), 154)
  expect_equal(class(function_output$ts_object_train[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[1]]), 25)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[1]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[1]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[1]])), 1993)
  expect_equal(attr(function_output$ts_object_train[[1]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[1]], "grouping"), "oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_train[[1]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_valid[[1]]), 166)
  expect_equal(ncol(function_output$ts_object_valid[[1]]), 3)
  expect_equal(colnames(function_output$ts_object_valid[[1]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_valid[[1]])), (1993 + 1/12))
  expect_equal(max(time(function_output$ts_object_valid[[1]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[1]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_valid[[1]], "grouping"), "oil_company = CompanyA")
  expect_equal(attr(function_output$ts_object_valid[[1]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_train[[310]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[310]]), 179)
  expect_equal(ncol(function_output$ts_object_train[[310]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[310]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[310]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[310]])), (2005 + 10/12))
  expect_equal(attr(function_output$ts_object_train[[310]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[310]], "grouping"), "oil_company = CompanyB")
  expect_equal(attr(function_output$ts_object_train[[310]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[310]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_valid[[310]]), 12)
  expect_equal(ncol(function_output$ts_object_valid[[310]]), 3)
  expect_equal(colnames(function_output$ts_object_valid[[310]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_valid[[310]])), 2005 + 11/12)
  expect_equal(max(time(function_output$ts_object_valid[[310]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[310]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_valid[[310]], "grouping"), "oil_company = CompanyB")
  expect_equal(attr(function_output$ts_object_valid[[310]], "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check create_main_forecasting_table with xreg hierarchical forecasts", {
  data <- dummy_hierarchical_gasprice %>% 
    dplyr::mutate(gasprice = ifelse(
        test = year_month > as.Date("2005-11-30"), 
        yes = NA,
        no = gasprice
      )
    )
  function_input <- tstools::initialize_ts_forecast_data(
    data = data, 
    date_col = "year_month", 
    col_of_interest = "gasprice",
    group_cols = "currency",
    xreg_cols = c("spotprice", "gemprice"),
    hierarchical_cols = c("oil_company", "location")
  )
  function_output <- create_main_forecasting_table(
    data = function_input,
    seasonal_periods = c(12,3), 
    min_train_periods = 144, 
    max_train_periods = Inf
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 1944)
  expect_equal(ncol(function_output), 9)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid", "hierarchy"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$hierarchy), "list")
  expect_equal(length(unique(function_output$grouping)), 54)
  expect_equal(min(function_output$train_length), 144)
  expect_equal(max(function_output$train_length), 179)
  expect_equal(min(function_output$valid_length), 0)
  expect_equal(max(function_output$valid_length), 35)
  expect_equal(class(function_output$ts_object_train[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[1]]), 144)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[1]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[1]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[1]])), (2002 + 11/12))
  expect_equal(attr(function_output$ts_object_train[[1]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[1]], "grouping"), "oil_company = CompanyC   &   location = USA   &   currency = EUR")
  expect_equal(attr(function_output$ts_object_train[[1]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_valid[[1]]), 47)
  expect_equal(ncol(function_output$ts_object_valid[[1]]), 3)
  expect_equal(colnames(function_output$ts_object_valid[[1]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_valid[[1]])), 2003)
  expect_equal(max(time(function_output$ts_object_valid[[1]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[1]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_valid[[1]], "grouping"), "oil_company = CompanyC   &   location = USA   &   currency = EUR")
  expect_equal(attr(function_output$ts_object_valid[[1]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_train[[312]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[312]]), 167)
  expect_equal(ncol(function_output$ts_object_train[[312]]), 3)
  expect_equal(colnames(function_output$ts_object_train[[312]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_train[[312]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[312]])), (2004 + 10/12))
  expect_equal(attr(function_output$ts_object_train[[312]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[312]], "grouping"), "oil_company = CompanyA   &   location = New York   &   currency = EUR")
  expect_equal(attr(function_output$ts_object_train[[312]], "xreg_cols"), c("spotprice", "gemprice"))
  expect_equal(class(function_output$ts_object_valid[[312]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_valid[[312]]), 24)
  expect_equal(ncol(function_output$ts_object_valid[[312]]), 3)
  expect_equal(colnames(function_output$ts_object_valid[[312]]), c("col_of_interest", "spotprice", "gemprice"))
  expect_equal(min(time(function_output$ts_object_valid[[312]])), 2004 + 11/12)
  expect_equal(max(time(function_output$ts_object_valid[[312]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[312]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_valid[[312]], "grouping"), "oil_company = CompanyA   &   location = New York   &   currency = EUR")
  expect_equal(attr(function_output$ts_object_valid[[312]], "xreg_cols"), c("spotprice", "gemprice"))
})

test_that("check create_main_forecasting_table without grouping column and colvars", {
  data <- dummy_gasprice %>% 
    dplyr::filter(
      state == "New York",
      oil_company == "CompanyA"
    )
  function_input <- tstools::initialize_ts_forecast_data(
    data = data, 
    date_col = "year_month", 
    col_of_interest = "gasprice"
  )
  function_output <- create_main_forecasting_table(
    data = function_input,
    seasonal_periods = c(12,3), 
    min_train_periods = 25, 
    max_train_periods = Inf
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 167)
  expect_equal(ncol(function_output), 8)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(unique(function_output$grouping), "group = all data")
  expect_equal(min(function_output$train_length), 25)
  expect_equal(max(function_output$train_length), 191)
  expect_equal(min(function_output$valid_length), 0)
  expect_equal(max(function_output$valid_length), 166)
  expect_equal(class(function_output$ts_object_train[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[1]]), 25)
  expect_equal(ncol(function_output$ts_object_train[[1]]), 1)
  expect_equal(colnames(function_output$ts_object_train[[1]]), "col_of_interest")
  expect_equal(min(time(function_output$ts_object_train[[1]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[1]])), 1993)
  expect_equal(attr(function_output$ts_object_train[[1]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[1]], "grouping"), "group = all data")
  expect_equal(class(function_output$ts_object_valid[[1]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_valid[[1]]), 166)
  expect_equal(ncol(function_output$ts_object_valid[[1]]), 1)
  expect_equal(colnames(function_output$ts_object_valid[[1]]), "col_of_interest")
  expect_equal(min(time(function_output$ts_object_valid[[1]])), (1993 + 1/12))
  expect_equal(max(time(function_output$ts_object_valid[[1]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_valid[[1]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_valid[[1]], "grouping"), "group = all data")
  expect_equal(class(function_output$ts_object_train[[167]]), c("msts", "ts"))
  expect_equal(nrow(function_output$ts_object_train[[167]]), 191)
  expect_equal(ncol(function_output$ts_object_train[[167]]), 1)
  expect_equal(colnames(function_output$ts_object_train[[167]]), "col_of_interest")
  expect_equal(min(time(function_output$ts_object_train[[167]])), 1991)
  expect_equal(max(time(function_output$ts_object_train[[167]])), (2006 + 10/12))
  expect_equal(attr(function_output$ts_object_train[[167]], "seasonality"), c(12,3))
  expect_equal(attr(function_output$ts_object_train[[167]], "grouping"), "group = all data")
  expect_equal(class(function_output$ts_object_valid[[167]]), "list")
  expect_equal(length(function_output$ts_object_valid[[167]]), 0)
})

test_that("check create_main_forecasting_table with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice, 
    date_col = "year_month", 
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  )
  expect_error(
    create_main_forecasting_table(
      data = "potato"
    )
  )
  expect_error(
    create_main_forecasting_table(
      data = function_input %>% 
        dplyr::select(-period)
    )
  )
  expect_error(
    create_main_forecasting_table(
      data = function_input %>% 
        dplyr::select(-col_of_interest)
    )
  )
  expect_error(
    create_main_forecasting_table(
      data = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    create_main_forecasting_table(
      data = function_input,
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = 48.5
    )
  )
  expect_error(
    create_main_forecasting_table(
      data = function_input,
      seasonal_periods = c(12,3),
      min_train_periods = 50,
      max_train_periods = 42
    )
  )
  expect_error(
    create_main_forecasting_table(
      data = function_input,
      seasonal_periods = c(12,3),
      min_train_periods = 1,
      max_train_periods = 100
    )
  )
  expect_error(
    create_main_forecasting_table(
      data = function_input,
      seasonal_periods = "string"
    )
  )
  expect_error(
    create_main_forecasting_table(
      data = function_input,
      seasonal_periods = 123
    )
  )
})
