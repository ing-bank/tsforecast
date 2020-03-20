
context("create_combined_fc_errors_table")

test_that("check create_combined_fc_errors_table with valid and invalid univariate inputs", {
  main_forecasting_table <- dummy_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-01-31")) %>%
    dplyr::filter(grouping %in% c(
        "state = New York   &   oil_company = CompanyB", 
        "state = New York   &   oil_company = CompanyA"
      )
    ) %>% 
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    ) 
  first_main_forecasting_table <- main_forecasting_table %>% 
    dplyr::filter(ts_split_date == 200601) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB")
  second_main_forecasting_table <- main_forecasting_table %>% 
    dplyr::filter(ts_split_date == 200601) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  for (operation in c("addition", "subtraction", "multiplication", "division")) {
    for (new_name in c("Company-B over CompanyA", "CompanyB CompanyA")) {
      function_output <- create_combined_fc_errors_table(
        first_main_forecasting_table = first_main_forecasting_table,
        second_main_forecasting_table = second_main_forecasting_table,
        first_fc_model = "fc_linear_trend_seasonal",
        second_fc_model = "fc_linear_trend_seasonal",
        new_fc_model = paste0("fc_", operation),
        operator = operation,
        group_variable = "oil_company",
        new_group_name = new_name
      )
      expect_equal(colnames(function_output), colnames(main_forecasting_table$fc_errors[[1]]))
      expect_equal(nrow(function_output), 37)
      expect_equal(unique(function_output$grouping), paste0("state = New York   &   oil_company = ", new_name)) 
      expect_equal(unique(function_output$fc_model), paste0("fc_", operation))
      expect_equal(unique(function_output$fc_date), 200601)
      expect_equal(mean(function_output$period), 200511.7578)
      expect_equal(min(function_output$period), 200401)
      expect_equal(max(function_output$period), 200701)
      expect_equal(unique(function_output$fc_periods_ahead), c(NA, 1:12))
      expect_equal(unique(function_output$fc_value[1:24]), NA_real_)
      expect_equal(unique(function_output$fc_error[1:24]), NA_real_)
      if (operation == "addition") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 3.254)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 4.380)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 6.102)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 4.717)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 5.311)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 6.162)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -1.07)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.274)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 1.551)
      }
      if (operation == "subtraction") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), -0.822)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 0.006)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 0.679)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), -0.154)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 0.141)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 0.610)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -0.589)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.224)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 0.846)
      }
      if (operation == "multiplication") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 2.646)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 4.914)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 9.139)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 5.525)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 7.064)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 9.477)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -2.813)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.686)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 3.903)
      }
      if (operation == "division") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 0.763)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 1.022)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 1.351)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 0.946)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 1.061)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 1.262)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -0.288)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.064)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 0.357)
      }
    }
    # Test with invalid inputs
    for (invalid_group in c("account", "a group", NA, 42)) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = invalid_group,
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      ) 
    }
    for (invalid_fc_model in c("fc_linear", "fc fc", "forecasting lala")) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = invalid_fc_model,
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
    for (invalid_mft in list(main_forecasting_table, dummy_gasprice, dummy_hierarchical_gasprice, tibble::tibble())) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = invalid_mft,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
    for (invalid_mft in list(main_forecasting_table, dummy_gasprice, dummy_hierarchical_gasprice, tibble::tibble())) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = invalid_mft,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
  }
})

test_that("check create_combined_fc_errors_table with valid and invalid multivariate inputs", {
  main_forecasting_table <- dummy_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      xreg_cols = c("spotprice", "gemprice"),
      group_cols = c("state", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-01-31")) %>%
    dplyr::filter(grouping %in% c(
        "state = New York   &   oil_company = CompanyB", 
        "state = New York   &   oil_company = CompanyA"
      )
    ) %>% 
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    ) 
  first_main_forecasting_table <- main_forecasting_table %>% 
    dplyr::filter(ts_split_date == 200601) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyB")
  second_main_forecasting_table <- main_forecasting_table %>% 
    dplyr::filter(ts_split_date == 200601) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  for (operation in c("addition", "subtraction", "multiplication", "division")) {
    for (new_name in c("Company-B over CompanyA", "CompanyB CompanyA")) {
      function_output <- create_combined_fc_errors_table(
        first_main_forecasting_table = first_main_forecasting_table,
        second_main_forecasting_table = second_main_forecasting_table,
        first_fc_model = "fc_linear_trend_seasonal_xreg",
        second_fc_model = "fc_linear_trend_seasonal_xreg",
        new_fc_model = paste0("fc_", operation),
        operator = operation,
        group_variable = "oil_company",
        new_group_name = new_name
      )
      expect_equal(colnames(function_output), colnames(main_forecasting_table$fc_errors[[1]]))
      expect_equal(nrow(function_output), 35)
      expect_equal(unique(function_output$grouping), paste0("state = New York   &   oil_company = ", new_name)) 
      expect_equal(unique(function_output$fc_model), paste0("fc_", operation))
      expect_equal(unique(function_output$fc_date), 200601)
      expect_equal(round(mean(function_output$period), 1), 200503.5)
      expect_equal(min(function_output$period), 200401)
      expect_equal(max(function_output$period), 200611)
      expect_equal(unique(function_output$fc_periods_ahead), c(NA, 1:10))
      expect_equal(unique(function_output$fc_value[1:24]), NA_real_)
      expect_equal(unique(function_output$fc_error[1:24]), NA_real_)
      if (operation == "addition") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 3.254)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 4.38)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 6.102)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 4.798)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 5.286)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 6.096)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -0.971)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.224)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 1.475)
      }
      if (operation == "subtraction") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), -0.822)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 0.006)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 0.679)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), -0.204)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 0.215)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 0.644)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -0.560)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.273)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 0.889)
      }
      if (operation == "multiplication") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 2.646)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 4.914)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 9.139)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 5.655)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 6.990)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 9.275)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -2.560)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.543)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 3.686)
      }
      if (operation == "division") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 0.763)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 1.022)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 1.351)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 0.928)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 1.094)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 1.304)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -0.256)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.087)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 0.365)
      }
    }
    # Test with invalid inputs
    for (invalid_group in c("account", "a group", NA, 42)) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = invalid_group,
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      ) 
    }
    for (invalid_fc_model in c("fc_linear", "fc fc", "forecasting lala")) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = invalid_fc_model,
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
    for (invalid_mft in list(main_forecasting_table, dummy_gasprice, dummy_hierarchical_gasprice, tibble::tibble())) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = invalid_mft,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
    for (invalid_mft in list(main_forecasting_table, dummy_gasprice, dummy_hierarchical_gasprice, tibble::tibble())) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = invalid_mft,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
  }
})

test_that("check create_combined_fc_errors_table with valid and invalid univariate hierarchical inputs", {
  main_forecasting_table <- dummy_hierarchical_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_cols = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-01-31")) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(grouping %in% c(
        "location = New York   &   oil_company = CompanyB   &   currency = EUR", 
        "location = New York   &   oil_company = CompanyA   &   currency = EUR"
      )
    ) %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    ) 
  first_main_forecasting_table <- main_forecasting_table %>% 
    dplyr::filter(ts_split_date == 200601) %>% 
    dplyr::filter(grouping == "location = New York   &   oil_company = CompanyB   &   currency = EUR")
  second_main_forecasting_table <- main_forecasting_table %>% 
    dplyr::filter(ts_split_date == 200601) %>% 
    dplyr::filter(grouping == "location = New York   &   oil_company = CompanyA   &   currency = EUR")
  for (operation in c("addition", "subtraction", "multiplication", "division")) {
    for (new_name in c("Company-B over CompanyA", "CompanyB CompanyA")) {
      function_output <- create_combined_fc_errors_table(
        first_main_forecasting_table = first_main_forecasting_table,
        second_main_forecasting_table = second_main_forecasting_table,
        first_fc_model = "fc_linear_trend_seasonal",
        second_fc_model = "fc_linear_trend_seasonal",
        new_fc_model = paste0("fc_", operation),
        operator = operation,
        group_variable = "oil_company",
        new_group_name = new_name
      )
      expect_equal(colnames(function_output), colnames(main_forecasting_table$fc_errors[[1]]))
      expect_equal(nrow(function_output), 37)
      expect_equal(unique(function_output$grouping), paste0("location = New York   &   oil_company = ", new_name, "   &   currency = EUR")) 
      expect_equal(unique(function_output$fc_model), paste0("fc_", operation))
      expect_equal(unique(function_output$fc_date), 200601)
      expect_equal(mean(function_output$period), 200511.7568)
      expect_equal(min(function_output$period), 200401)
      expect_equal(max(function_output$period), 200701)
      expect_equal(unique(function_output$fc_periods_ahead), c(NA, 1:12))
      expect_equal(unique(function_output$fc_value[1:24]), NA_real_)
      expect_equal(unique(function_output$fc_error[1:24]), NA_real_)
      if (operation == "addition") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 10.086)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 13.845)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 19.979)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 14.708)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 16.595)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 18.631)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -3.528)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.418)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 4.635)
      }
      if (operation == "subtraction") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), -1.979)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 0.040)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 2.300)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), -0.460)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 0.553)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 1.357)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -2.139)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.642)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 2.925)
      }
      if (operation == "multiplication") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 25.417)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 49.161)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 98.954)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 53.719)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 68.939)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 86.681)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -30.329)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 2.959)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 37.710)
      }
      if (operation == "division") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 0.799)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 1.02)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 1.325)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 0.944)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 1.073)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 1.179)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -0.330)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.061)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 0.379)
      }
    }
    # Test with invalid inputs
    for (invalid_group in c("account", "a group", NA, 42)) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = invalid_group,
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      ) 
    }
    for (invalid_fc_model in c("fc_linear", "fc fc", "forecasting lala")) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = invalid_fc_model,
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
    for (invalid_mft in list(main_forecasting_table, dummy_gasprice, dummy_hierarchical_gasprice, tibble::tibble())) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = invalid_mft,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
    for (invalid_mft in list(main_forecasting_table, dummy_gasprice, dummy_hierarchical_gasprice, tibble::tibble())) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = invalid_mft,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
  }
})

test_that("check create_combined_fc_errors_table with valid and invalid multivariate hierarchical inputs", {
  main_forecasting_table <- dummy_hierarchical_gasprice %>%
    dplyr::filter(currency == "EUR") %>% 
    dplyr::filter(oil_company %in% c("CompanyA", "CompanyB")) %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      xreg_cols = c("spotprice", "gemprice"),
      group_cols = "oil_company",
      hierarchical_cols = "location"
    ) %>%
    dplyr::filter(period >= as.Date("2004-01-31")) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(grouping %in% c(
        "location = New York   &   oil_company = CompanyB", 
        "location = New York   &   oil_company = CompanyA"
      )
    ) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    ) 
  first_main_forecasting_table <- main_forecasting_table %>% 
    dplyr::filter(ts_split_date == 200601) %>% 
    dplyr::filter(grouping == "location = New York   &   oil_company = CompanyB")
  second_main_forecasting_table <- main_forecasting_table %>% 
    dplyr::filter(ts_split_date == 200601) %>% 
    dplyr::filter(grouping == "location = New York   &   oil_company = CompanyA")
  for (operation in c("addition", "subtraction", "multiplication", "division")) {
    for (new_name in c("Company-B over CompanyA", "CompanyB CompanyA")) {
      function_output <- create_combined_fc_errors_table(
        first_main_forecasting_table = first_main_forecasting_table,
        second_main_forecasting_table = second_main_forecasting_table,
        first_fc_model = "fc_linear_trend_seasonal_xreg",
        second_fc_model = "fc_linear_trend_seasonal_xreg",
        new_fc_model = paste0("fc_", operation),
        operator = operation,
        group_variable = "oil_company",
        new_group_name = new_name
      )
      expect_equal(colnames(function_output), colnames(main_forecasting_table$fc_errors[[1]]))
      expect_equal(nrow(function_output), 35)
      expect_equal(unique(function_output$grouping), paste0("location = New York   &   oil_company = ", new_name)) 
      expect_equal(unique(function_output$fc_model), paste0("fc_", operation))
      expect_equal(unique(function_output$fc_date), 200601)
      expect_equal(round(mean(function_output$period), 1), 200503.5)
      expect_equal(min(function_output$period), 200401)
      expect_equal(max(function_output$period), 200611)
      expect_equal(unique(function_output$fc_periods_ahead), c(NA, 1:10))
      expect_equal(unique(function_output$fc_value[1:24]), NA_real_)
      expect_equal(unique(function_output$fc_error[1:24]), NA_real_)
      if (operation == "addition") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 10.086)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 13.845)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 19.979)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 15.399)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 16.932)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 19.149)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -3.249)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.674)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 5.583)
      }
      if (operation == "subtraction") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), -1.979)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 0.040)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 2.300)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), -0.498)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 0.515)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 1.690)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -1.936)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.581)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 2.425)
      }
      if (operation == "multiplication") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 25.417)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 49.161)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 98.954)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 58.735)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 71.861)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 91.672)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -28.034)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 5.190)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 46.562)
      }
      if (operation == "division") {
        expect_equal(round(min(function_output$actual, na.rm = T), 3), 0.799)
        expect_equal(round(mean(function_output$actual, na.rm = T), 3), 1.020)
        expect_equal(round(max(function_output$actual, na.rm = T), 3), 1.325)
        expect_equal(round(min(function_output$fc_value, na.rm = T), 3), 0.938)
        expect_equal(round(mean(function_output$fc_value, na.rm = T), 3), 1.068)
        expect_equal(round(max(function_output$fc_value, na.rm = T), 3), 1.220)
        expect_equal(round(min(function_output$fc_error, na.rm = T), 3), -0.329)
        expect_equal(round(mean(function_output$fc_error, na.rm = T), 3), 0.052)
        expect_equal(round(max(function_output$fc_error, na.rm = T), 3), 0.306)
      }
    }
    # Test with invalid inputs
    for (invalid_group in c("account", "a group", NA, 42)) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = second_main_forecasting_table,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation),
          operator = operation,
          group_variable = invalid_group,
          new_group_name = new_name
        )
      ) 
    }
    for (invalid_fc_model in c("fc_linear", "fc fc", "forecasting lala")) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = invalid_fc_model,
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
    for (invalid_mft in list(main_forecasting_table, dummy_gasprice, dummy_hierarchical_gasprice, tibble::tibble())) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = invalid_mft,
          second_main_forecasting_table = second_main_forecasting_table,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
    for (invalid_mft in list(main_forecasting_table, dummy_gasprice, dummy_hierarchical_gasprice, tibble::tibble())) {
      expect_error(
        create_combined_fc_errors_table(
          first_main_forecasting_table = first_main_forecasting_table,
          second_main_forecasting_table = invalid_mft,
          group_variable = "oil_company",
          new_group_name = new_name,
          operator = operation,
          first_fc_model = "fc_linear_trend_seasonal",
          second_fc_model = "fc_linear_trend_seasonal",
          new_fc_model = paste0("fc_", operation)
        )
      )
    }
  }
})
