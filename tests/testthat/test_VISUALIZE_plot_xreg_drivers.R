
context("plot_xreg_drivers")

test_that("check plot_xreg_drivers with valid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200503) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 12,
      keep_fc_model_objects = T
    ) 
  function_output <- plot_xreg_drivers(
    main_forecasting_table = function_input,
    xreg = "spotprice",
    granularity = 50
  )
  function_output <- suppressWarnings(ggplot2::last_plot()) # Because its easier to test a ggplot2 object than a plotly object
  expect_is(function_output, c("gg", "ggplot"))
  expect_equal(as.character(function_output$mapping), c("~xreg_value", "~fitted", "~fc_model"))
  expect_equal(function_output$labels$x, "xreg_value")
  expect_equal(function_output$labels$y, "fitted")
  expect_equal(function_output$labels$colour, "fc_model")
  expect_equal(function_output$labels$text, "paste0(\"Group: \", main_forecasting_table$grouping[1], \"<br>Regressor: \", ...")
  expect_is(function_output$data, c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output$data), 700)
  expect_equal(ncol(function_output$data), 3)
  expect_equal(colnames(function_output$data), c("xreg_value", "fitted", "fc_model"))
  expect_is(function_output$data$xreg_value, "numeric")
  expect_is(function_output$data$fitted, "numeric")
  expect_is(function_output$data$fc_model, "character")
  expect_equal(unique(function_output$data$fc_model), c(
    "fc_rpart_xreg", 
    "fc_ctree_xreg", 
    "fc_randomforest_xreg",
    "fc_linear_trend_xreg",
    "fc_linear_trend_seasonal_xreg", 
    "fc_arima_xreg",
    "fc_arima_stl_xreg",
    "fc_nn_5n_0decay_xreg",
    "fc_nn_25n_0decay_xreg", 
    "fc_nn_5n_50decay_xreg", 
    "fc_nn_25n_50decay_xreg", 
    "fc_prophet_005cps_xreg", 
    "fc_prophet_050cps_xreg",
    "fc_prophet_500cps_xreg"
  ))
  expect_equal(round(min(function_output$data$xreg_value), 2), 9.83)
  expect_equal(round(max(function_output$data$xreg_value), 2), 86.61)
  expect_equal(round(min(function_output$data$fitted), 2), 1.16)
  expect_equal(round(max(function_output$data$fitted), 2), 2.06)
})

test_that("test plot_xreg_drivers with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    create_main_forecasting_table() %>% 
    dplyr::filter(ts_split_date == 200503) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
    add_fc_models_to_main_forecasting_table(
      periods_ahead = 36,
      fc_methods = c("linear")
    )
  xreg_candidates = list(c("bitsy", "spotprice"), "bunny rabbits", "", NULL, NA)
  for (xreg in xreg_candidates) {
    expect_error(
      plot_xreg_drivers(
        main_forecasting_table = function_input,
        xreg = xreg
      )
    ) 
  }
  invalid_table_inputs = list(dummy_gasprice, tibble::tibble(a = 1), "I am not a data frame", NULL, NA)
  for (table in invalid_table_inputs) {
    expect_error(
      plot_xreg_drivers(
        main_forecasting_table = table,
        xreg = "spotprice"
      )
    ) 
  }
  invalid_granularity_inputs = list(0, -100, NA, NULL, "I am not a number")
  for (granularity in invalid_granularity_inputs) {
    expect_error(
      plot_xreg_drivers(
        main_forecasting_table = function_input,
        xreg = "spotprice",
        granularity = granularity
      )
    )
  }
})
