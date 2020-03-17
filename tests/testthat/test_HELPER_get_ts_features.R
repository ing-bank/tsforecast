
context("get_ts_features")

test_that("check get_ts_features with valid non-hierarchical univariate and multivariate data", {
  # Univariate input/output
  input_univar <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("oil_company", "state")
    ) %>% 
    create_main_forecasting_table(
      min_train_periods = 180
    ) %>% 
    dplyr::slice(1:1) %>% 
    dplyr::pull(ts_object_train) 
  output_univar <- get_ts_features(input_univar)
  # Multivariate input/output
  input_multivar <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("oil_company", "state"),
      xreg_cols = c("gemprice")
    ) %>% 
    create_main_forecasting_table(
      min_train_periods = 180
    ) %>% 
    dplyr::slice(1:1) %>% 
    dplyr::pull(ts_object_train) 
  output_multivar <- get_ts_features(input_multivar)
  # Run tests on both
  for (output in list(output_multivar, output_univar)) {
    expect_is(output, c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(output), 1)
    expect_equal(ncol(output), 44)
    expect_equal(colnames(output), c(
      "frequency", "series_length", "diff_std_err", "unitroot_kpss", 
      "unitroot_pp", "nperiods", "seasonal_period", "trend", "spike", 
      "linearity", "curvature", "e_acf1", "e_acf10", "seasonal_strength", 
      "peak", "trough", "crossing_points", "flat_spots", "lumpiness", 
      "stability", "entropy", "hurst", "nonlinearity", "ARCH.LM", "x_acf1", 
      "x_acf10", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10", 
      "seas_acf1", "x_pacf5", "diff1x_pacf5", "diff2x_pacf5", "seas_pacf", 
      "arch_acf", "garch_acf", "arch_r2", "garch_r2", "alpha", "beta", 
      "alpha1", "beta1", "gamma"
    ))
    expect_equal(round(min(output), 2), -26.71)
    expect_equal(round(max(output), 2), 180)
    expect_equal(output$frequency, 12)
    expect_equal(output$seasonal_period, 12)
  }
})

test_that("check get_ts_features with valid hierarchical univariate and multivariate data", {
  # Univariate input/output
  input_univar <- dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("oil_company", "location", "currency"),
      hierarchical_cols = c("location", "oil_company")
    ) %>% 
    create_main_forecasting_table(
      min_train_periods = 180
    ) %>% 
    dplyr::slice(1:1) %>% 
    dplyr::pull(ts_object_train) 
  output_univar <- get_ts_features(input_univar)
  # Multivariate input/output
  input_multivar <- dummy_hierarchical_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("oil_company", "location", "currency"),
      hierarchical_cols = c("location", "oil_company"),
      xreg_cols = c("gemprice")
    ) %>% 
    create_main_forecasting_table(
      min_train_periods = 180
    ) %>% 
    dplyr::slice(1:1) %>% 
    dplyr::pull(ts_object_train) 
  output_multivar <- get_ts_features(input_multivar)
  # Run tests on both
  for (output in list(output_multivar, output_univar)) {
    expect_is(output, c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(output), 1)
    expect_equal(ncol(output), 44)
    expect_equal(colnames(output), c(
      "frequency", "series_length", "diff_std_err", "unitroot_kpss", 
      "unitroot_pp", "nperiods", "seasonal_period", "trend", "spike", 
      "linearity", "curvature", "e_acf1", "e_acf10", "seasonal_strength", 
      "peak", "trough", "crossing_points", "flat_spots", "lumpiness", 
      "stability", "entropy", "hurst", "nonlinearity", "ARCH.LM", "x_acf1", 
      "x_acf10", "diff1_acf1", "diff1_acf10", "diff2_acf1", "diff2_acf10", 
      "seas_acf1", "x_pacf5", "diff1x_pacf5", "diff2x_pacf5", "seas_pacf", 
      "arch_acf", "garch_acf", "arch_r2", "garch_r2", "alpha", "beta", 
      "alpha1", "beta1", "gamma"
    ))
    expect_equal(round(min(output), 2), -5.24)
    expect_equal(round(max(output), 2), 180)
    expect_equal(output$frequency, 12)
    expect_equal(output$seasonal_period, 12)
  }
})

test_that("check get ts_features with invalid inputs", {
  univar_data <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("oil_company", "state")
    )
  univar_mft <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("oil_company", "state")
    ) %>% 
    create_main_forecasting_table(
      min_train_periods = 180
    )
  multivar_data <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("oil_company", "state"),
      xreg_cols = c("spotprice")
    )
  multivar_mft <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("oil_company", "state"),
      xreg_cols = c("spotprice")
    ) %>% 
    create_main_forecasting_table(
      min_train_periods = 180
    )
  invalid_inputs <- list(
    univar_data, univar_mft, 
    multivar_data, multivar_mft, 
    c("We have", "a pony plan"), 42, NA, 
    dummy_gasprice, dummy_hierarchical_gasprice
  )
  for (input in invalid_inputs) {
    expect_error(
      get_ts_features(input)
    )
  }
})
