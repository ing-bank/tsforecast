
context("add_hierarchical_fc_models_to_main_forecasting_table")

test_that("check add_hierarchical_fc_models_to_main_forecasting_table with valid univariate inputs", {
  function_input <-  dummy_hierarchical_gasprice %>%
    dplyr::filter(location == "Indiana") %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = "oil_company"
    ) %>%
    dplyr::filter(period >= as.Date("2004-10-31")) %>%
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  function_output <- add_hierarchical_fc_models_to_main_forecasting_table(function_input)
  expect_equal(colnames(function_input), colnames(function_output))
  expect_equal(class(function_input), class(function_output))
  expect_equal(nrow(function_input), nrow(function_output))
  expect_true(all(unique(function_output$grouping) %in% unique(function_input$grouping)))
  expect_equal(function_output$ts_start, function_input$ts_start)
  expect_equal(function_output$ts_split_date, function_input$ts_split_date)
  expect_equal(function_output$ts_end, function_input$ts_end)
  expect_equal(function_output$train_length, function_input$train_length)
  expect_equal(function_output$valid_length, function_input$valid_length)
  expect_equal(class(function_output$ts_object_train), class(function_input$ts_object_train))
  expect_equal(class(function_output$ts_object_valid), class(function_input$ts_object_valid))
  expect_equal(function_output$hierarchy, function_input$hierarchy)
  expect_equal(class(function_output$fc_errors), class(function_input$fc_errors))
  expect_true(any(grepl("_consistent", names(function_output$fc_models[[1]]))))
  expect_true(any(grepl("fc_bottom_up", names(function_output$fc_models[[1]]))))
  expected_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", 
    "fc_drift_l3m", "fc_drift_l6m", "fc_drift_l12m", 
    "fc_naive", "fc_naive_seasonal",
    "fc_linear_trend", "fc_linear_trend_seasonal"
  )
  fc_models <- function_output$fc_models[[1]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), c(expected_models, "fc_naive_seasonal_consistent","fc_bottom_up"))
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200610)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200611)
    expect_equal(fc_models[[exp_model]]$fc_data$period[7], 200705)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
  }
  fc_models <- function_output$fc_models[[6]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), c(expected_models, "fc_drift_l3m_consistent","fc_bottom_up"))
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 12)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200611)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200612)
    expect_equal(fc_models[[exp_model]]$fc_data$period[8], 200707)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 12)
  }
})

test_that("check add_hierarchical_fc_models_to_main_forecasting_table with valid multivariate inputs", {
  function_input <-  dummy_hierarchical_gasprice %>%
    dplyr::filter(currency == "USD") %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      xreg_cols = c("spotprice", "gemprice"),
      hierarchical_col = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-08-31")) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(valid_length != 0) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  function_output <- add_hierarchical_fc_models_to_main_forecasting_table(function_input)
  expect_equal(colnames(function_input), colnames(function_output))
  expect_equal(class(function_input), class(function_output))
  expect_equal(nrow(function_input), nrow(function_output))
  expect_true(all(unique(function_output$grouping) %in% unique(function_input$grouping)))
  expect_equal(function_output$ts_start, function_input$ts_start)
  expect_equal(function_output$ts_split_date, function_input$ts_split_date)
  expect_equal(function_output$ts_end, function_input$ts_end)
  expect_equal(function_output$train_length, function_input$train_length)
  expect_equal(function_output$valid_length, function_input$valid_length)
  expect_equal(class(function_output$ts_object_train), class(function_input$ts_object_train))
  expect_equal(class(function_output$ts_object_valid), class(function_input$ts_object_valid))
  expect_equal(function_output$hierarchy, function_input$hierarchy)
  expect_equal(class(function_output$fc_errors), class(function_input$fc_errors))
  expect_true(any(grepl("_consistent", names(function_output$fc_models[[1]]))))
  expect_true(any(grepl("fc_bottom_up", names(function_output$fc_models[[1]]))))
  expected_models <- c(
    "fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg", 
    "fc_linear_trend_xreg_consistent", "fc_bottom_up"   
  )
  fc_models <- function_output$fc_models[[1]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200608)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200609)
    expect_equal(fc_models[[exp_model]]$fc_data$period[3], 200611)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 3)
  }
  fc_models <- function_output$fc_models[[6]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 1)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200610)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200611)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 1)
  }
})

test_that("check add_hierarchical_fc_models_to_main_forecasting_table with invalid inputs", {
  no_forecasts <-  dummy_hierarchical_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      xreg_cols = c("spotprice", "gemprice"),
      hierarchical_col = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-09-30")) %>%
    create_main_forecasting_table()
  no_matrix <- dummy_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    dplyr::filter(period >= as.Date("2004-09-30")) %>%
    create_main_forecasting_table() %>% 
    dplyr::filter(valid_length != 0) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  invalid_inputs <- list(
    no_forecasts, no_matrix, 
    dummy_hierarchical_gasprice, dummy_gasprice, 
    NA, c("I want", "to be an MFT"), 42
  )
  for (data in invalid_inputs) {
    expect_error(
      add_hierarchical_fc_models_to_main_forecasting_table(data)
    )
  }
})
