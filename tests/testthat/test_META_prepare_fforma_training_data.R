
context("prepare_fforma_training_data")

test_that("check prepare_fforma_training_data with valid univariate and multivariate data", {
  # Univariate forecasts
  univar_input <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = NULL,
      min_train_periods = 188
    ) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    ) %>% 
    dplyr::filter(valid_length != 0)
  univar_output <- prepare_fforma_training_data(univar_input)
  # Multivariate forecasts
  multivar_input <- dummy_gasprice %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("gemprice", "spotprice")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = NULL,
      min_train_periods = 188
    ) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("linear")
    ) %>% 
    dplyr::filter(valid_length != 0)
  multivar_output <- prepare_fforma_training_data(multivar_input)
  for (output in list(univar_output, multivar_output)) {
    expect_is(output, c("tbl_df", "tbl", "data.frame"))
    expect_equal(nrow(output), 12)
    expect_equal(ncol(output), 4)
    expect_equal(colnames(output), c(
      "grouping", "ts_object_train", "ts_split_date", "best_fc_model"
    ))
    expect_equal(unique(output$grouping), c(
      "state = New York   &   oil_company = CompanyA", "state = New York   &   oil_company = CompanyB", 
      "state = Indiana   &   oil_company = CompanyA", "state = Indiana   &   oil_company = CompanyB"
    ))
    expect_equal(unique(output$ts_split_date), c(200608, 200609, 200610))
    expect_is(output$ts_object_train, "list")
  }
  expect_equal(unique(univar_output$best_fc_model), c(
    "fc_linear_trend", "fc_mean_l12m", "fc_drift_l3m", "fc_linear_trend_seasonal", 
    "fc_mean_l6m", "fc_mean_l3m", "fc_naive_seasonal"
  ))
  expect_equal(unique(multivar_output$best_fc_model), c(
    "fc_linear_trend_seasonal_xreg", "fc_linear_trend_xreg"
  ))
})

test_that("check prepare_fforma_training_data with invalid inputs", {
  incomplete_mft <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 190,
      max_train_periods = Inf
    ) 
  invalid_inputs <- list(
    incomplete_mft, 
    dummy_gasprice, dummy_hierarchical_gasprice, 
    c("Ponies", "everywhere"), NA, 42
  )
  for (input in invalid_inputs) {
    expect_error(
      prepare_fforma_training_data(input)
    )
  }
})
