
context("plot_xreg")

test_that("check plot_xreg with valid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 120,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(ts_split_date == 200503) %>%
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("prophet")
    )
  function_output <- plot_xreg(
    main_forecasting_table = function_input,
    xreg = "spotprice"
  )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~period", "~value", "~variable"))
  expect_equal(function_output$labels$x, "period")
  expect_equal(function_output$labels$y, "value")
  expect_equal(function_output$labels$colour, "variable")
  expect_equal(function_output$labels$text, "paste0(\"Variable: \", variable, \"<br>Period: \", format.Date(period, ...")
  expect_equal(function_output$labels$xintercept, "xintercept")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 382)
  expect_equal(ncol(function_output$data), 4)
  expect_equal(colnames(function_output$data), c("period", "variable", "original_value", "value"))
  expect_is(function_output$data$variable, "factor")
  expect_is(function_output$data$period, "Date")
  expect_is(function_output$data$original_value, "numeric")
  expect_is(function_output$data$value, "numeric")
  expect_equal(as.vector(unique(function_output$data$variable)), c(
    "spotprice", 
    "col_of_interest" 
  ))
  expect_equal(min(function_output$data$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$data$period), as.Date("2006-11-30"))
  expect_equal(round(min(function_output$data$original_value),2), 0.81)
  expect_equal(round(max(function_output$data$original_value),2), 86.61)
  expect_equal(min(function_output$data$value), 0)
  expect_equal(max(function_output$data$value), 1)
  function_output <- plot_xreg(
    main_forecasting_table = function_input,
    xreg = c("spotprice", "gemprice")
  )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~period", "~value", "~variable"))
  expect_equal(function_output$labels$x, "period")
  expect_equal(function_output$labels$y, "value")
  expect_equal(function_output$labels$colour, "variable")
  expect_equal(function_output$labels$text, "paste0(\"Variable: \", variable, \"<br>Period: \", format.Date(period, ...")
  expect_equal(function_output$labels$xintercept, "xintercept")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 573)
  expect_equal(ncol(function_output$data), 4)
  expect_equal(colnames(function_output$data), c("period", "variable", "original_value", "value"))
  expect_is(function_output$data$variable, "factor")
  expect_is(function_output$data$period, "Date")
  expect_is(function_output$data$original_value, "numeric")
  expect_is(function_output$data$value, "numeric")
  expect_equal(as.vector(unique(function_output$data$variable)), c(
    "spotprice", 
    "gemprice",
    "col_of_interest"
  ))
  expect_equal(min(function_output$data$period), as.Date("1991-01-31"))
  expect_equal(max(function_output$data$period), as.Date("2006-11-30"))
  expect_equal(min(function_output$data$value), 0)
  expect_equal(max(function_output$data$value), 1)
  expect_equal(round(min(function_output$data$original_value),2), 0.81)
  expect_equal(round(max(function_output$data$original_value),2), 86.61)
})

test_that("test plot_xreg with invalid inputs", {
  univariate_input <- tstools::initialize_ts_forecast_data(
    data = dummy_gasprice,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = c("state", "oil_company")
  ) %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 120,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(ts_split_date == 200503) %>%
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("basic")
    )
  xreg_candidates = list(c("gemprice", "spotprice"), "spotprice", "", "gemprice", NULL, NA)
  for (xreg in xreg_candidates) {
    expect_error(
      plot_xreg(
        main_forecasting_table = univariate_input,
        xreg = xreg
      )
    ) 
  }
  invalid_xreg_candidates = list("", NULL, NA, c("spotprice", "cute little rabbits"), "gasprice")
  for (xreg in invalid_xreg_candidates) {
    expect_error(
      plot_xreg(
        main_forecasting_table = function_input,
        xreg = xreg
      )
    ) 
  }
  invalid_data_inputs = list(univariate_input, tibble::tibble(), "Icy dead people", dummy_gasprice)
  for (data in invalid_data_inputs) {
    expect_error(
      plot_xreg(
        main_forecasting_table = data,
        xreg = "spotprice"
      )
    )
  }
})
