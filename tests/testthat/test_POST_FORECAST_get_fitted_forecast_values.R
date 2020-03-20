context("get_fitted_forecast_values")

main_forecasting_table <- tstools::initialize_ts_forecast_data(
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
    fc_methods = c("linear", "arima", "nn"),
    keep_fc_model_objects = T
  )
fc_models <- names(main_forecasting_table$fc_models[[1]])
main_fit_table <- tibble::tibble(
  xreg_value = vector(length = 100 * length(fc_models)),
  fitted = vector(length = 100 * length(fc_models)),
  fc_model = rep(fc_models, each = 100)
)

test_that("check get_fitted_forecast_values with valid inputs", {
  function_output <- get_fitted_forecast_values(
    main_forecasting_table = main_forecasting_table,
    main_fit_table = main_fit_table,
    xreg = "spotprice"
  )
  expect_is(function_output,c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(function_output), c("xreg_value", "fitted", "fc_model"))
  expect_equal(ncol(function_output), 3)
  expect_equal(nrow(function_output), 800)
  expect_is(function_output$xreg_value, "numeric")
  expect_is(function_output$fitted, "numeric")
  expect_is(function_output$fc_model, "character")
  expect_equal(unique(function_output$fc_model), c(
    "fc_linear_trend_xreg",
    "fc_linear_trend_seasonal_xreg",
    "fc_arima_xreg",
    "fc_arima_stl_xreg",
    "fc_nn_5n_0decay_xreg",
    "fc_nn_25n_0decay_xreg",
    "fc_nn_5n_50decay_xreg",
    "fc_nn_25n_50decay_xreg"
  ))
  expect_equal(round(min(function_output$xreg_value), 2), 9.83)
  expect_equal(round(max(function_output$xreg_value), 2), 57.19)
  expect_equal(round(min(function_output$fitted), 2), 1.16)
  expect_equal(round(max(function_output$fitted), 2), 2.06)
  # Check with other xreg candidate
  function_output <- get_fitted_forecast_values(
    main_forecasting_table = main_forecasting_table,
    main_fit_table = main_fit_table,
    xreg = "gemprice"
  )
  expect_is(function_output,c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(function_output), c("xreg_value", "fitted", "fc_model"))
  expect_equal(ncol(function_output), 3)
  expect_equal(nrow(function_output), 800)
  expect_is(function_output$xreg_value, "numeric")
  expect_is(function_output$fitted, "numeric")
  expect_is(function_output$fc_model, "character")
  expect_equal(unique(function_output$fc_model), c(
    "fc_linear_trend_xreg",
    "fc_linear_trend_seasonal_xreg",
    "fc_arima_xreg",
    "fc_arima_stl_xreg",
    "fc_nn_5n_0decay_xreg",
    "fc_nn_25n_0decay_xreg",
    "fc_nn_5n_50decay_xreg",
    "fc_nn_25n_50decay_xreg"
  ))
  expect_equal(round(min(function_output$xreg_value), 2), 19.06)
  expect_equal(round(max(function_output$xreg_value), 2), 67.24)
  expect_equal(round(min(function_output$fitted), 2), 1.31)
  expect_equal(round(max(function_output$fitted), 2), 1.67)
})

test_that("check get_fitted_forecast_values with invalid inputs", {
  invalid_forecasting_table <- c("Uuulalalala", dummy_gasprice, NA, NULL, 42)
  for (table in invalid_forecasting_table) {
    expect_error(
      get_fitted_forecast_values(
        main_forecasting_table = table,
        main_fit_table = main_fit_table,
        xreg = "spotprice"
      )
    )
    expect_error(
      get_fitted_forecast_values(
        main_forecasting_table = table,
        main_fit_table = main_fit_table,
        xreg = "gemprice"
      )
    )
  }
  invalid_fit_table <- c("Uuulalalala", dummy_gasprice, NA, NULL, 42, tibble::tibble(xreg_value = 1, fitted = NA, fc_model = "lala"))
  for (table in invalid_fit_table) {
    expect_error(
      get_fitted_forecast_values(
        main_forecasting_table = main_forecasting_table,
        main_fit_table = table,
        xreg = "spotprice"
      )
    )
    expect_error(
      get_fitted_forecast_values(
        main_forecasting_table = main_forecasting_table,
        main_fit_table = table,
        xreg = "gemprice"
      )
    )
  }
  invalid_xreg <- c("somethingprice", 42, NA, NULL)
  for (xreg in invalid_xreg) {
    expect_error(
      get_fitted_forecast_values(
        main_forecasting_table = main_forecasting_table,
        main_fit_table = main_fit_table,
        xreg = xreg
      )
    )
  }
})