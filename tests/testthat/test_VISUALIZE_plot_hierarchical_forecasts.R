
context("plot_hierarchical_forecasts")

test_that("check plot_hierarchical_forecasts with valid univariate inputs",{
  function_input <- dummy_hierarchical_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_cols = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-10-31")) %>%
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("linear")
    ) %>%
    add_hierarchical_fc_models_to_main_forecasting_table() %>%  
    dplyr::filter(ts_split_date == 200611)
  selected_groupings <- c(
    "location = USA   &   oil_company = CompanyC   &   currency = EUR",
    "location = Bronx   &   oil_company = CompanyC   &   currency = EUR",
    "location = New York   &   oil_company = CompanyB   &   currency = USD",
    "location = South Indiana   &   oil_company = CompanyB   &   currency = USD"
  )
  selected_fc_models <- c("consistent", "bottom_up")
  selected_hierarchical_cols <- list(c("location", "oil_company"), "location", "oil_company")
  for (grouping in selected_groupings) {
    for (fc_model in selected_fc_models) {
      for (hierarchical_col in selected_hierarchical_cols) {
        function_output <- plot_hierarchical_forecasts(
          main_forecasting_table = function_input,
          fc_model = fc_model,
          hierarchical_cols = hierarchical_col,
          grouping = grouping
        )
        expect_is(function_output, c("plotly", "htmlwidget"))
        function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
        expect_is(function_output, c("gg", "ggplot"))
        expect_equal(as.character(function_output$mapping), c("~period", "~value", "~grouping", "~grouping"))
        expect_equal(function_output$labels$x, "period")
        expect_equal(function_output$labels$y, "value")
        expect_equal(function_output$labels$fill, "grouping")
        expect_equal(function_output$labels$colour, "grouping")
        expect_equal(function_output$labels$xintercept, "xintercept")
        for (layer in function_output$layers[1:2]) {
          expect_is(layer$data, c("tbl_df", "tbl", "data.frame"))
          expect_equal(colnames(layer$data), c(hierarchical_col, "value", "period", "master_grouping", "grouping"))
          expect_is(layer$data$value, "numeric")
          expect_is(layer$data$period, "Date")
          expect_is(layer$data$master_grouping, "character")
          expect_is(layer$data$grouping, "character")
          expect_equal(min(unique(layer$data$period)), as.Date("2004-10-31"))
          expect_equal(max(unique(layer$data$period)), as.Date("2007-11-30"))
        }
      }
    }
  }
})

test_that("check plot_hierarchical_forecasts with valid multivariate inputs",{
  function_input <- dummy_hierarchical_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      xreg_cols = c("spotprice", "gemprice"),
      hierarchical_cols = c("location", "oil_company")
    ) %>%
    dplyr::mutate(
      col_of_interest = case_when(
        period >= as.Date("2005-01-31") ~ NA_real_,
        TRUE ~ col_of_interest
      )
    ) %>%
    dplyr::filter(period >= as.Date("2003-01-31")) %>%
    create_main_forecasting_table(
      seasonal_periods = 3,
      min_train_periods = 23,
      max_train_periods = Inf
    ) %>% 
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 12,
      fc_methods = c("linear")
    ) %>%      
    dplyr::filter(ts_split_date == 200411) %>% 
    add_hierarchical_fc_models_to_main_forecasting_table()
  selected_groupings <- c(
    "location = USA   &   oil_company = CompanyC   &   currency = EUR",
    "location = Bronx   &   oil_company = CompanyC   &   currency = EUR",
    "location = New York   &   oil_company = CompanyB   &   currency = USD",
    "location = South Indiana   &   oil_company = CompanyB   &   currency = USD"
  )
  selected_fc_models <- c("consistent", "bottom_up")
  selected_hierarchical_cols <- list(c("location", "oil_company"), "location", "oil_company")
  for (grouping in selected_groupings) {
    for (fc_model in selected_fc_models) {
      for (hierarchical_col in selected_hierarchical_cols) {
        function_output <- plot_hierarchical_forecasts(
          main_forecasting_table = function_input,
          fc_model = fc_model,
          hierarchical_cols = hierarchical_col,
          grouping = grouping
        )
        expect_is(function_output, c("plotly", "htmlwidget"))
        function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
        expect_is(function_output, c("gg", "ggplot"))
        expect_equal(as.character(function_output$mapping), c("~period", "~value", "~grouping", "~grouping"))
        expect_equal(function_output$labels$x, "period")
        expect_equal(function_output$labels$y, "value")
        expect_equal(function_output$labels$fill, "grouping")
        expect_equal(function_output$labels$colour, "grouping")
        expect_equal(function_output$labels$xintercept, "xintercept")
        for (layer in function_output$layers[1:2]) {
          expect_is(layer$data, c("tbl_df", "tbl", "data.frame"))
          expect_equal(colnames(layer$data), c(hierarchical_col, "value", "period", "master_grouping", "grouping"))
          expect_is(layer$data$value, "numeric")
          expect_is(layer$data$period, "Date")
          expect_is(layer$data$master_grouping, "character")
          expect_is(layer$data$grouping, "character")
          expect_equal(min(unique(layer$data$period)), as.Date("2003-01-31"))
          expect_equal(max(unique(layer$data$period)), as.Date("2005-11-30"))
        }
      }
    }
  }
})

test_that("check plot_hierarchical_forecasts with invalid inputs",{
  # Set up valid inputs
  valid_mft <- dummy_hierarchical_gasprice %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = "currency",
      hierarchical_cols = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-10-31")) %>%
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic")
    ) %>%
    add_hierarchical_fc_models_to_main_forecasting_table() %>%  
    dplyr::filter(ts_split_date == 200611)
  selected_groupings <- c(
    "location = USA   &   oil_company = CompanyC   &   currency = EUR",
    "location = Bronx   &   oil_company = CompanyC   &   currency = EUR",
    "location = New York   &   oil_company = CompanyB   &   currency = USD",
    "location = South Indiana   &   oil_company = CompanyB   &   currency = USD"
  )
  selected_fc_models <- c("consistent", "bottom_up")
  selected_hierarchical_cols <- list(c("location", "oil_company"), "location", "oil_company")
  # Set up invalid inputs
  invalid_mft <- list(
    dummy_hierarchical_gasprice, dummy_gasprice, 
    tibble::tibble(), c("Hello"),
    valid_mft %>% dplyr::select(-hierarchy)
  )
  invalid_groupings <- list(
    "location = USA & oil_company = CompanyC & currency = EUR",
    "location = USA",
    "Pretty bunnies",
    42,
    NA
  )
  invalid_fc_models <- list(
    "fc_bottom_up",
    "fc_ctree",
    NA,
    42
  )
  invalid_hierarchical_cols <- list(
    "currency",
    "oil_company = CompanyC",
    "hierarchy",
    valid_mft
  )
  # Run with combinations of only one invalid input
  
  # Invalid MFT
  for (grouping in selected_groupings) {
    for (fc_model in selected_fc_models) {
      for (hierarchical_col in selected_hierarchical_cols) {
        for (mft in invalid_mft) {
          expect_error(
            plot_hierarchical_forecasts(
              main_forecasting_table = mft,
              fc_model = fc_model,
              hierarchical_cols = hierarchical_col,
              grouping = grouping
            )
          )
        }
      }
    }
  }
  # Invalid groupings
  for (grouping in invalid_groupings) {
    for (fc_model in selected_fc_models) {
      for (hierarchical_col in selected_hierarchical_cols) {
        expect_error(
          plot_hierarchical_forecasts(
            main_forecasting_table = valid_mft,
            fc_model = fc_model,
            hierarchical_cols = hierarchical_col,
            grouping = grouping
          )
        )
      }
    }
  }
  # invalid fc_models
  for (grouping in selected_groupings) {
    for (fc_model in invalid_fc_models) {
      for (hierarchical_col in selected_hierarchical_cols) {
        expect_error(
          plot_hierarchical_forecasts(
            main_forecasting_table = valid_mft,
            fc_model = fc_model,
            hierarchical_cols = hierarchical_col,
            grouping = grouping
          )
        )
      }
    }
  }
  # Invalid hierarchical_cols
  for (grouping in selected_groupings) {
    for (fc_model in selected_fc_models) {
      for (hierarchical_col in invalid_hierarchical_cols) {
        expect_error(
          suppressWarnings(
            plot_hierarchical_forecasts(
              main_forecasting_table = valid_mft,
              fc_model = fc_model,
              hierarchical_cols = hierarchical_col,
              grouping = grouping
            ) 
          )
        )
      }
    }
  }
})
