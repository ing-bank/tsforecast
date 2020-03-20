
context("get_actuals_from_main_forecasting_table")

test_that("check get_actuals_from_main_forecasting_table with different inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    create_main_forecasting_table(
      min_train_periods = 180
    )
  function_output <- get_actuals_from_main_forecasting_table(
    main_forecasting_table = function_input,
    for_plot = F
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping"))
  expect_equal(class(function_output$period), "numeric")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(min(function_output$period), 199101)
  expect_equal(max(function_output$period), 200611)
  expect_equal(round(min(function_output$col_of_interest),2), 0.78)
  expect_equal(round(max(function_output$col_of_interest),2), 3.46)
  expect_equal(unique(function_output$grouping), c(
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB",
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
  function_output <- get_actuals_from_main_forecasting_table(
    main_forecasting_table = function_input,
    for_plot = T
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 764)
  expect_equal(ncol(function_output), 4)
  expect_equal(colnames(function_output), c("grouping", "period", "fc_model", "value"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$period), "numeric")
  expect_equal(class(function_output$fc_model), "character")
  expect_equal(class(function_output$value), "numeric")
  expect_equal(unique(function_output$grouping), c(
    "state = New York   &   oil_company = CompanyA", 
    "state = New York   &   oil_company = CompanyB",
    "state = Indiana   &   oil_company = CompanyA", 
    "state = Indiana   &   oil_company = CompanyB"
  ))
  expect_equal(min(function_output$period), 199101)
  expect_equal(max(function_output$period), 200611)
  expect_equal(unique(function_output$fc_model), "actuals")
  expect_equal(round(min(function_output$value),2), 0.78)
  expect_equal(round(max(function_output$value),2), 3.46)
  function_input <- dummy_gasprice %>% 
    dplyr::filter(state == "New York" & oil_company == "CompanyA") %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice"
    ) %>% 
    create_main_forecasting_table()
  function_output <- get_actuals_from_main_forecasting_table(
    main_forecasting_table = function_input,
    for_plot = F
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 191)
  expect_equal(ncol(function_output), 3)
  expect_equal(colnames(function_output), c("period", "col_of_interest", "grouping"))
  expect_equal(class(function_output$period), "numeric")
  expect_equal(class(function_output$col_of_interest), "numeric")
  expect_equal(class(function_output$grouping), "character")
  expect_equal(min(function_output$period), 199101)
  expect_equal(max(function_output$period), 200611)
  expect_equal(round(min(function_output$col_of_interest),2), 0.81)
  expect_equal(round(max(function_output$col_of_interest),2), 3.46)
  expect_equal(unique(function_output$grouping), "group = all data")
})

test_that("check get_actuals_from_main_forecasting_table when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table()
  expect_error(
    get_actuals_from_main_forecasting_table(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    get_actuals_from_main_forecasting_table(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    get_actuals_from_main_forecasting_table(
      main_forecasting_table = function_input %>% 
        dplyr::select(-grouping)
    )
  )
  expect_error(
    get_actuals_from_main_forecasting_table(
      main_forecasting_table = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  capture.output(
    expect_error(
      get_actuals_from_main_forecasting_table(
        main_forecasting_table = function_input %>% 
          dplyr::mutate(ts_object_train = ts_object_valid) %>% 
          tail(1)
      )
    ),
    file = 'NUL'
  )
})
