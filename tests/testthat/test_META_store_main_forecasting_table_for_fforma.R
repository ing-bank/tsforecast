
context("store_main_forecasting_table_for_fforma")

dir_for_testing <- tempdir()

get_unique_rows_per_grouping <- function(function_output) {
  function_output %>% 
    dplyr::group_by(grouping) %>% 
    dplyr::summarise(n_rows = dplyr::n()) %>% 
    dplyr::pull(n_rows) %>% 
    unique() %>% 
    return()
}

function_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company"),
    xreg_cols = c("spotprice", "gemprice")
  ) %>%
  create_main_forecasting_table(
    seasonal_periods = NULL,
    min_train_periods = 188
  ) %>%
  add_fc_models_to_main_forecasting_table(
    fc_methods = c("basic", "linear")
  ) %>% 
  dplyr::filter(valid_length != 0)

test_that("check if store_main_forecasting_table_for_fforma correctly stores and overwrites on disk", {
  store_main_forecasting_table_for_fforma(
    main_forecasting_table = function_input,
    name = "testing",
    dir_fforma = dir_for_testing,
    overwrite = TRUE
  )
  test_file <- list.files(dir_for_testing, full.names = T)
  test_file <- test_file[grepl("TESTING", test_file)]
  expect_equal(length(test_file), 1)
  function_output <- readRDS(test_file)
  expect_is(function_output, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 12)
  expect_equal(ncol(function_output), 4)
  expect_equal(get_unique_rows_per_grouping(function_output), 3)
  expect_equal(unique(function_output$ts_split_date), c(200608, 200609, 200610))
  expect_equal(colnames(function_output), c(
    "grouping", "ts_object_train", "ts_split_date", "best_fc_model"
  ))
  
  last_time_saved <- file.info(test_file)$mtime
  expect_error(
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = function_input,
      name = "testing",
      dir_fforma = dir_for_testing,
      overwrite = FALSE
    )
  )
  expect_equal(file.info(test_file)$mtime, last_time_saved)

  store_main_forecasting_table_for_fforma(
    main_forecasting_table = function_input,
    name = "testing",
    dir_fforma = dir_for_testing,
    overwrite = TRUE
  )
  expect_true(file.info(test_file)$mtime > last_time_saved)
  function_output <- readRDS(test_file)
  expect_is(function_output, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 12)
  expect_equal(ncol(function_output), 4)
  expect_equal(get_unique_rows_per_grouping(function_output), 3)
  expect_equal(unique(function_output$ts_split_date), c(200608, 200609, 200610))
  expect_equal(colnames(function_output), c(
    "grouping", "ts_object_train", "ts_split_date", "best_fc_model"
  ))
})

test_that("check store_main_forecasting_table_for_fforma output with invalid inputs", {
  expect_error(
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = function_input,
      name = ""
    )
  )
  expect_error(
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = function_input,
      name = 42
    )
  )
  expect_error(
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = function_input,
      name = c("It's", "a", "kind", "of", "pony")
    )
  )
  expect_error(
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = function_input,
      name = "testing",
      dir_fforma = "dont/exist"
    )
  )
})
