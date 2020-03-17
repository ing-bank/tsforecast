
context("add_fc_values_per_split_date")

test_that("check add_fc_values_per_split_date with univariate valid inputs", {
  function_input <- dummy_hierarchical_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200606) %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  best_model_per_group <- function_input %>%
    get_forecast_accuracy_overview(metric = "MAE") %>%
    get_best_forecast_methods(filter_incomplete_fc = T)
  consistent_fc_values <- get_consistent_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = best_model_per_group
  )
  bottom_up_fc_values <- get_bottom_up_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = best_model_per_group
  )
  # Test with bottom_up forecasts
  function_output <- add_fc_values_per_split_date(
    main_forecasting_table = function_input,
    grouped_hierarchical_fc_values = bottom_up_fc_values
  )
  expect_equal(colnames(function_input), colnames(function_output))
  expect_equal(class(function_input), class(function_output))
  expect_equal(nrow(function_input), nrow(function_output))
  expect_equal(function_output$grouping, function_input$grouping)
  expect_equal(function_output$ts_start, function_input$ts_start)
  expect_equal(function_output$ts_split_date, function_input$ts_split_date)
  expect_equal(function_output$ts_end, function_input$ts_end)
  expect_equal(function_output$train_length, function_input$train_length)
  expect_equal(function_output$valid_length, function_input$valid_length)
  expect_equal(function_output$ts_object_train, function_input$ts_object_train)
  expect_equal(function_output$ts_object_valid, function_input$ts_object_valid)
  expect_equal(function_output$hierarchy, function_input$hierarchy)
  expect_equal(function_output$fc_errors, function_input$fc_errors)
  expect_equal(names(function_output$fc_models[[1]]), c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_drift_l3m", 
    "fc_drift_l6m", "fc_drift_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_bottom_up"
  ))
  expect_equal(names(function_output$fc_models[[6]]), c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_drift_l3m", 
    "fc_drift_l6m", "fc_drift_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_bottom_up"
  ))
  expect_equal(round(mean(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 24.155)
  expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 21.437)
  expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 27.593)
  expect_equal(round(mean(function_output$fc_models[[4]]$fc_bottom_up$fc_data$fc_value), 3), 14.623)
  expect_equal(round(min(function_output$fc_models[[4]]$fc_bottom_up$fc_data$fc_value), 3), 13.969)
  expect_equal(round(max(function_output$fc_models[[4]]$fc_bottom_up$fc_data$fc_value), 3), 15.283)
  expect_equal(round(median(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200656.5)
  expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200607)
  expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200706)
  expect_equal(round(median(function_output$fc_models[[3]]$fc_bottom_up$fc_data$period), 3), 200656.5)
  expect_equal(round(min(function_output$fc_models[[3]]$fc_bottom_up$fc_data$period), 3), 200607)
  expect_equal(round(max(function_output$fc_models[[3]]$fc_bottom_up$fc_data$period), 3), 200706)
  expect_equal(unique(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_date), 200606)
  expect_equal(unique(function_output$fc_models[[4]]$fc_bottom_up$fc_data$fc_date), 200606)
  # Test with consistent forecasts
  function_output <- add_fc_values_per_split_date(
    main_forecasting_table = function_input,
    grouped_hierarchical_fc_values = consistent_fc_values
  )
  expect_equal(colnames(function_input), colnames(function_output))
  expect_equal(class(function_input), class(function_output))
  expect_equal(nrow(function_input), nrow(function_output))
  expect_equal(function_output$grouping, function_input$grouping)
  expect_equal(function_output$ts_start, function_input$ts_start)
  expect_equal(function_output$ts_split_date, function_input$ts_split_date)
  expect_equal(function_output$ts_end, function_input$ts_end)
  expect_equal(function_output$train_length, function_input$train_length)
  expect_equal(function_output$valid_length, function_input$valid_length)
  expect_equal(function_output$ts_object_train, function_input$ts_object_train)
  expect_equal(function_output$ts_object_valid, function_input$ts_object_valid)
  expect_equal(function_output$hierarchy, function_input$hierarchy)
  expect_equal(function_output$fc_errors, function_input$fc_errors)
  expect_equal(names(function_output$fc_models[[1]]), c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_drift_l3m", 
    "fc_drift_l6m", "fc_drift_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_drift_l3m_consistent"
  ))
  expect_equal(names(function_output$fc_models[[6]]), c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_drift_l3m", 
    "fc_drift_l6m", "fc_drift_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_mean_l6m_consistent"
  ))
  expect_equal(round(mean(function_output$fc_models[[1]]$fc_drift_l3m_consistent$fc_data$fc_value), 3), 23.64)
  expect_equal(round(min(function_output$fc_models[[1]]$fc_drift_l3m_consistent$fc_data$fc_value), 3), 20.452)
  expect_equal(round(max(function_output$fc_models[[1]]$fc_drift_l3m_consistent$fc_data$fc_value), 3), 27.068)
  expect_equal(round(mean(function_output$fc_models[[6]]$fc_mean_l6m_consistent$fc_data$fc_value), 3), 12.934)
  expect_equal(round(min(function_output$fc_models[[6]]$fc_mean_l6m_consistent$fc_data$fc_value), 3), 10.888)
  expect_equal(round(max(function_output$fc_models[[6]]$fc_mean_l6m_consistent$fc_data$fc_value), 3), 15.071)
  expect_equal(round(median(function_output$fc_models[[1]]$fc_drift_l3m_consistent$fc_data$period), 3), 200656.5)
  expect_equal(round(min(function_output$fc_models[[1]]$fc_drift_l3m_consistent$fc_data$period), 3), 200607)
  expect_equal(round(max(function_output$fc_models[[1]]$fc_drift_l3m_consistent$fc_data$period), 3), 200706)
  expect_equal(round(median(function_output$fc_models[[6]]$fc_mean_l6m_consistent$fc_data$period), 3), 200656.5)
  expect_equal(round(min(function_output$fc_models[[6]]$fc_mean_l6m_consistent$fc_data$period), 3), 200607)
  expect_equal(round(max(function_output$fc_models[[6]]$fc_mean_l6m_consistent$fc_data$period), 3), 200706)
  expect_equal(unique(function_output$fc_models[[1]]$fc_drift_l3m_consistent$fc_data$fc_date), 200606)
  expect_equal(unique(function_output$fc_models[[6]]$fc_mean_l6m_consistent$fc_data$fc_date), 200606)
  # Test invalid inputs for grouped_hierarchical_fc_values
  hierarchical_data <- dummy_hierarchical_gasprice %>%
    dplyr::filter(currency == "EUR") %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "location",
      hierarchical_col = "oil_company"
    )
  invalid_fc_inputs <- list(
    hierarchical_data, 
    dummy_hierarchical_gasprice, 
    dummy_gasprice, 
    c("mft", "I", "might be"), NA, 42
  )
  for (input in invalid_fc_inputs) {
    expect_error(
      add_fc_values_per_split_date(
        main_forecasting_table = function_input,
        grouped_hierarchical_fc_values = input
      )
    )
  }
})

test_that("check add_fc_values_per_split_date with multivariate valid inputs", {
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyB") %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      xreg_cols = c("gemprice", "spotprice"),
      hierarchical_col = "location"
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200606) %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  best_model_per_group <- function_input %>%
    get_forecast_accuracy_overview(metric = "MASE") %>%
    get_best_forecast_methods(filter_incomplete_fc = T)
  consistent_fc_values <- get_consistent_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = best_model_per_group
  )
  bottom_up_fc_values <- get_bottom_up_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = best_model_per_group
  )
  # Test with bottom_up forecasts
  function_output <- add_fc_values_per_split_date(
    main_forecasting_table = function_input,
    grouped_hierarchical_fc_values = bottom_up_fc_values
  )
  expect_equal(colnames(function_input), colnames(function_output))
  expect_equal(class(function_input), class(function_output))
  expect_equal(nrow(function_input), nrow(function_output))
  expect_equal(function_output$grouping, function_input$grouping)
  expect_equal(function_output$ts_start, function_input$ts_start)
  expect_equal(function_output$ts_split_date, function_input$ts_split_date)
  expect_equal(function_output$ts_end, function_input$ts_end)
  expect_equal(function_output$train_length, function_input$train_length)
  expect_equal(function_output$valid_length, function_input$valid_length)
  expect_equal(function_output$ts_object_train, function_input$ts_object_train)
  expect_equal(function_output$ts_object_valid, function_input$ts_object_valid)
  expect_equal(function_output$hierarchy, function_input$hierarchy)
  expect_equal(function_output$fc_errors, function_input$fc_errors)
  expect_equal(names(function_output$fc_models[[1]]), c("fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg", "fc_bottom_up"))
  expect_equal(names(function_output$fc_models[[6]]), c("fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg", "fc_bottom_up"))
  expect_equal(round(mean(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 14.219)
  expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 13.811)
  expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 14.519)
  expect_equal(round(mean(function_output$fc_models[[4]]$fc_bottom_up$fc_data$fc_value), 3), 9.264)
  expect_equal(round(min(function_output$fc_models[[4]]$fc_bottom_up$fc_data$fc_value), 3), 8.996)
  expect_equal(round(max(function_output$fc_models[[4]]$fc_bottom_up$fc_data$fc_value), 3), 9.445)
  expect_equal(round(median(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200609)
  expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200607)
  expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200611)
  expect_equal(round(median(function_output$fc_models[[3]]$fc_bottom_up$fc_data$period), 3), 200609)
  expect_equal(round(min(function_output$fc_models[[3]]$fc_bottom_up$fc_data$period), 3), 200607)
  expect_equal(round(max(function_output$fc_models[[3]]$fc_bottom_up$fc_data$period), 3), 200611)
  expect_equal(unique(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_date), 200606)
  expect_equal(unique(function_output$fc_models[[4]]$fc_bottom_up$fc_data$fc_date), 200606)
  # Test with consistent forecasts
  function_output <- add_fc_values_per_split_date(
    main_forecasting_table = function_input,
    grouped_hierarchical_fc_values = consistent_fc_values
  )
  expect_equal(colnames(function_input), colnames(function_output))
  expect_equal(class(function_input), class(function_output))
  expect_equal(nrow(function_input), nrow(function_output))
  expect_equal(function_output$grouping, function_input$grouping)
  expect_equal(function_output$ts_start, function_input$ts_start)
  expect_equal(function_output$ts_split_date, function_input$ts_split_date)
  expect_equal(function_output$ts_end, function_input$ts_end)
  expect_equal(function_output$train_length, function_input$train_length)
  expect_equal(function_output$valid_length, function_input$valid_length)
  expect_equal(function_output$ts_object_train, function_input$ts_object_train)
  expect_equal(function_output$ts_object_valid, function_input$ts_object_valid)
  expect_equal(function_output$hierarchy, function_input$hierarchy)
  expect_equal(function_output$fc_errors, function_input$fc_errors)
  expect_equal(names(function_output$fc_models[[1]]), c(
    "fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg", "fc_linear_trend_xreg_consistent"
  ))
  expect_equal(names(function_output$fc_models[[6]]), c(
    "fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg", "fc_linear_trend_xreg_consistent"
  ))
  expect_equal(round(mean(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 14.125)
  expect_equal(round(min(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 13.594)
  expect_equal(round(max(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 14.505)
  expect_equal(round(mean(function_output$fc_models[[6]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 3.088)
  expect_equal(round(min(function_output$fc_models[[6]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 2.867)
  expect_equal(round(max(function_output$fc_models[[6]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 3.305)
  expect_equal(round(median(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200609)
  expect_equal(round(min(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200607)
  expect_equal(round(max(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
  expect_equal(round(median(function_output$fc_models[[6]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200609)
  expect_equal(round(min(function_output$fc_models[[6]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200607)
  expect_equal(round(max(function_output$fc_models[[6]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
  expect_equal(unique(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_date), 200606)
  expect_equal(unique(function_output$fc_models[[6]]$fc_linear_trend_xreg_consistent$fc_data$fc_date), 200606)
  # Test invalid inputs for grouped_hierarchical_fc_values
  hierarchical_data <- dummy_hierarchical_gasprice %>%
    dplyr::filter(location == "USA") %>%
    dplyr::filter(currency == "USD") %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      xreg_cols = c("spotprice", "gemprice"),
      hierarchical_cols = "oil_company"
    )
  invalid_fc_inputs <- list(
    hierarchical_data, 
    dummy_hierarchical_gasprice, dummy_gasprice, 
    c("mft", "I", "might be"), NA, 42
  )
  for (input in invalid_fc_inputs) {
    expect_error(
      add_fc_values_per_split_date(
        main_forecasting_table = function_input,
        grouped_hierarchical_fc_values = input
      )
    )
  }
})

test_that("check add_fc_values_per_split_date with invalid inputs", {
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyB") %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_col = "location"
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200606) %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  best_model_per_group <- function_input %>%
    get_forecast_accuracy_overview(metric = "MAPE") %>%
    get_best_forecast_methods(filter_incomplete_fc = T)
  consistent_fc_values <- get_consistent_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = best_model_per_group
  )
  bottom_up_fc_values <- get_bottom_up_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = best_model_per_group
  )
  univariate_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyA") %>%
    dplyr::filter(currency == "EUR") %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200606) %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  multivariate_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyB") %>%
    dplyr::filter(currency == "USD") %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("location", "oil_company"),
      xreg_cols = c("gemprice", "spotprice")
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200606) %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  invalid_inputs <- list(
    univariate_input, multivariate_input, 
    dummy_gasprice, dummy_hierarchical_gasprice, 
    NA, 42, "potatoes"
  )
  fc_input <- list(
    bottom_up_fc_values, 
    consistent_fc_values
  )
  for (input in invalid_inputs) {
    for (fc in fc_input) {
      expect_error(
        add_fc_values_per_split_date(
          main_forecasting_table = input,
          grouped_hierarchical_fc_values = fc 
        )
      ) 
    }
  }
})
