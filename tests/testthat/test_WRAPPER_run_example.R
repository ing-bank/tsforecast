
context("run_example")

test_that("check run_example for correctly loading data for AirPassengers", {
  function_output <- run_example(
    dataset = "AirPassengers",
    test_mode = T
  )
  expect_is(function_output, "character")
  expect_true(file.exists(function_output))
  available_files <- list.files(function_output, recursive = T)
  expect_equal(available_files, c(
    "FD_external_regressor_drivers_tab_ui.R", 
    "FD_external_regressors_tab_ui.R", "FD_forecast_deepdive_server.R", 
    "FD_forecast_deepdive_ui.R", "FD_forecast_performance_tab_ui.R", 
    "FD_forecasts_vs_actuals_tab_ui.R", "FD_hierarchical_forecasts_tab_ui.R", 
    "main_server.R", "server.R", "sidebar_ui.R", "ui.R", 
    "VH_view_hierarchy_server.R", "VH_view_hierarchy_ui.R", "www/favicon.ico", 
    "www/style.css", "www/tsforecast_logo.png", "www/white_space.png"
  ))
  expect_is(app_data, "list")
  expect_equal(names(app_data), c(
    "main_forecasting_table", "groupings", "fc_models", 
    "is_hierarchical", "hierarchy", "hierarchical_cols", "is_multivariate", 
    "has_original_actuals", "latest_split_date", 
    "documentation"
  ))
  expect_is(app_data$main_forecasting_table, "data.frame")
  expect_is(app_data$groupings, "data.frame")
  expect_is(app_data$fc_models, "character")
  expect_equal(app_data$is_hierarchical, FALSE)
  expect_equal(app_data$hierarchy, NA)
  expect_equal(app_data$hierarchical_cols, "")
  expect_equal(app_data$is_multivariate, FALSE)
  expect_equal(app_data$has_original_actuals, FALSE)
  expect_is(app_data$latest_split_date, "numeric")
  expect_equal(app_data$documentation, list())
  expect_equal(nrow(app_data$main_forecasting_table), 255)
  expect_equal(ncol(app_data$main_forecasting_table), 9)
  expect_equal(app_data$groupings$grouping, c(
    "dataset = AirPassengers   &   type = original", 
    "dataset = AirPassengers   &   type = reverse", 
    "dataset = AirPassengers   &   type = noisy"
  ))
  expect_equal(app_data$groupings$dataset, c(
    "AirPassengers", "AirPassengers", "AirPassengers"
  ))
  expect_equal(app_data$groupings$type, c(
    "original", "reverse", "noisy"
  ))
  expect_equal(app_data$fc_models, c(
    "fc_arima", "fc_arima_stl", "fc_bats", "fc_ctree", "fc_drift_l12m", 
    "fc_drift_l3m", "fc_drift_l6m", "fc_ensemble_aefnst", "fc_ets_addiv", 
    "fc_ets_addiv_damped", "fc_ets_multip", "fc_ets_multip_damped", 
    "fc_ets_stl", "fc_fforma", "fc_holt_winters_addiv", "fc_holt_winters_multip", 
    "fc_kalman_poly", "fc_kalman_seas_12", "fc_linear_trend", "fc_linear_trend_seasonal", 
    "fc_mean_l12m", "fc_mean_l3m", "fc_mean_l6m", "fc_naive", "fc_naive_seasonal", 
    "fc_nn_25n_0decay", "fc_nn_25n_50decay", "fc_nn_25n_elm", "fc_nn_5n_0decay", 
    "fc_nn_5n_50decay", "fc_nn_5n_mlp", "fc_prophet_005cps", "fc_prophet_050cps", 
    "fc_prophet_500cps", "fc_randomforest", "fc_rec_cforest", "fc_rec_ctree", 
    "fc_rec_rpart", "fc_rec_svmradsig", "fc_rpart", "fc_tbats"
  ))
  expect_equal(app_data$latest_split_date, 196012)
})

test_that("check run_example for correctly loading data for nottem", {
  function_output <- run_example(
    dataset = "nottem",
    test_mode = T
  )
  expect_is(function_output, "character")
  expect_true(file.exists(function_output))
  available_files <- list.files(function_output, recursive = T)
  expect_equal(available_files, c(
    "FD_external_regressor_drivers_tab_ui.R", 
    "FD_external_regressors_tab_ui.R", "FD_forecast_deepdive_server.R", 
    "FD_forecast_deepdive_ui.R", "FD_forecast_performance_tab_ui.R", 
    "FD_forecasts_vs_actuals_tab_ui.R", "FD_hierarchical_forecasts_tab_ui.R", 
    "main_server.R", "server.R", "sidebar_ui.R", "ui.R", 
    "VH_view_hierarchy_server.R", "VH_view_hierarchy_ui.R", "www/favicon.ico", 
    "www/style.css", "www/tsforecast_logo.png", "www/white_space.png"
  ))
  expect_is(app_data, "list")
  expect_equal(names(app_data), c(
    "main_forecasting_table", "groupings", "fc_models", 
    "is_hierarchical", "hierarchy", "hierarchical_cols", "is_multivariate", 
    "has_original_actuals", "latest_split_date", 
    "documentation"
  ))
  expect_is(app_data$main_forecasting_table, "data.frame")
  expect_is(app_data$groupings, "data.frame")
  expect_is(app_data$fc_models, "character")
  expect_equal(app_data$is_hierarchical, FALSE)
  expect_equal(app_data$hierarchy, NA)
  expect_equal(app_data$hierarchical_cols, "")
  expect_equal(app_data$is_multivariate, FALSE)
  expect_equal(app_data$has_original_actuals, FALSE)
  expect_is(app_data$latest_split_date, "numeric")
  expect_equal(app_data$documentation, list())
  expect_equal(nrow(app_data$main_forecasting_table), 543)
  expect_equal(ncol(app_data$main_forecasting_table), 9)
  expect_equal(app_data$groupings$grouping, c(
    "dataset = nottem   &   type = original", 
    "dataset = nottem   &   type = reverse", 
    "dataset = nottem   &   type = noisy"
  ))
  expect_equal(app_data$groupings$dataset, c(
    "nottem", "nottem", "nottem"
  ))
  expect_equal(app_data$groupings$type, c(
    "original", "reverse", "noisy"
  ))
  expect_equal(app_data$fc_models, c(
    "fc_arima", "fc_arima_stl", "fc_bats", "fc_ctree", "fc_drift_l12m", 
    "fc_drift_l3m", "fc_drift_l6m", "fc_ensemble_aefnst", "fc_ets_addiv", 
    "fc_ets_addiv_damped", "fc_ets_multip", "fc_ets_multip_damped", 
    "fc_ets_stl", "fc_fforma", "fc_holt_winters_addiv", "fc_holt_winters_multip", 
    "fc_kalman_poly", "fc_kalman_seas_12", "fc_linear_trend", "fc_linear_trend_seasonal", 
    "fc_mean_l12m", "fc_mean_l3m", "fc_mean_l6m", "fc_naive", "fc_naive_seasonal", 
    "fc_nn_25n_0decay", "fc_nn_25n_50decay", "fc_nn_25n_elm", "fc_nn_5n_0decay", 
    "fc_nn_5n_50decay", "fc_nn_5n_mlp", "fc_prophet_005cps", "fc_prophet_050cps", 
    "fc_prophet_500cps", "fc_randomforest", "fc_rec_cforest", "fc_rec_ctree", 
    "fc_rec_rpart", "fc_rec_svmradsig", "fc_rpart", "fc_tbats"
  ))
  expect_equal(app_data$latest_split_date, 193912)
})

test_that("check run_example for correctly loading data for UKDriverDeaths", {
  function_output <- run_example(
    dataset = "UKDriverDeaths",
    test_mode = T
  )
  expect_is(function_output, "character")
  expect_true(file.exists(function_output))
  available_files <- list.files(function_output, recursive = T)
  expect_equal(available_files, c(
    "FD_external_regressor_drivers_tab_ui.R", 
    "FD_external_regressors_tab_ui.R", "FD_forecast_deepdive_server.R", 
    "FD_forecast_deepdive_ui.R", "FD_forecast_performance_tab_ui.R", 
    "FD_forecasts_vs_actuals_tab_ui.R", "FD_hierarchical_forecasts_tab_ui.R", 
    "main_server.R", "server.R", "sidebar_ui.R", "ui.R", 
    "VH_view_hierarchy_server.R", "VH_view_hierarchy_ui.R", "www/favicon.ico", 
    "www/style.css", "www/tsforecast_logo.png", "www/white_space.png"
  ))
  expect_is(app_data, "list")
  expect_equal(names(app_data), c(
    "main_forecasting_table", "groupings", "fc_models", 
    "is_hierarchical", "hierarchy", "hierarchical_cols", "is_multivariate", 
    "has_original_actuals", "latest_split_date", 
    "documentation"
  ))
  expect_is(app_data$main_forecasting_table, "data.frame")
  expect_is(app_data$groupings, "data.frame")
  expect_is(app_data$fc_models, "character")
  expect_equal(app_data$is_hierarchical, FALSE)
  expect_equal(app_data$hierarchy, NA)
  expect_equal(app_data$hierarchical_cols, "")
  expect_equal(app_data$is_multivariate, FALSE)
  expect_equal(app_data$has_original_actuals, FALSE)
  expect_is(app_data$latest_split_date, "numeric")
  expect_equal(app_data$documentation, list())
  expect_equal(nrow(app_data$main_forecasting_table), 399)
  expect_equal(ncol(app_data$main_forecasting_table), 9)
  expect_equal(app_data$groupings$grouping, c(
    "dataset = UKDriverDeaths   &   type = original", 
    "dataset = UKDriverDeaths   &   type = reverse", 
    "dataset = UKDriverDeaths   &   type = noisy"
  ))
  expect_equal(app_data$groupings$dataset, c(
    "UKDriverDeaths", "UKDriverDeaths", "UKDriverDeaths"
  ))
  expect_equal(app_data$groupings$type, c(
    "original", "reverse", "noisy"
  ))
  expect_equal(app_data$fc_models, c(
    "fc_arima", "fc_arima_stl", "fc_bats", "fc_ctree", "fc_drift_l12m", 
    "fc_drift_l3m", "fc_drift_l6m", "fc_ensemble_aefnst", "fc_ets_addiv", 
    "fc_ets_addiv_damped", "fc_ets_multip", "fc_ets_multip_damped", 
    "fc_ets_stl", "fc_fforma", "fc_holt_winters_addiv", "fc_holt_winters_multip", 
    "fc_kalman_poly", "fc_kalman_seas_12", "fc_linear_trend", "fc_linear_trend_seasonal", 
    "fc_mean_l12m", "fc_mean_l3m", "fc_mean_l6m", "fc_naive", "fc_naive_seasonal", 
    "fc_nn_25n_0decay", "fc_nn_25n_50decay", "fc_nn_25n_elm", "fc_nn_5n_0decay", 
    "fc_nn_5n_50decay", "fc_nn_5n_mlp", "fc_prophet_005cps", "fc_prophet_050cps", 
    "fc_prophet_500cps", "fc_randomforest", "fc_rec_cforest", "fc_rec_ctree", 
    "fc_rec_rpart", "fc_rec_svmradsig", "fc_rpart", "fc_tbats"
  ))
  expect_equal(app_data$latest_split_date, 198412)
})

test_that("check run_example for correctly loading data for hierarchical", {
  function_output <- run_example(
    dataset = "hierarchical",
    test_mode = T
  )
  expect_is(function_output, "character")
  expect_true(file.exists(function_output))
  available_files <- list.files(function_output, recursive = T)
  expect_equal(available_files, c(
    "FD_external_regressor_drivers_tab_ui.R", 
    "FD_external_regressors_tab_ui.R", "FD_forecast_deepdive_server.R", 
    "FD_forecast_deepdive_ui.R", "FD_forecast_performance_tab_ui.R", 
    "FD_forecasts_vs_actuals_tab_ui.R", "FD_hierarchical_forecasts_tab_ui.R", 
    "main_server.R", "server.R", "sidebar_ui.R", "ui.R", 
    "VH_view_hierarchy_server.R", "VH_view_hierarchy_ui.R", "www/favicon.ico", 
    "www/style.css", "www/tsforecast_logo.png", "www/white_space.png"
  ))
  expect_is(app_data, "list")
  expect_equal(names(app_data), c(
    "main_forecasting_table", "groupings", "fc_models", 
    "is_hierarchical", "hierarchy", "hierarchical_cols", "is_multivariate", 
    "has_original_actuals", "latest_split_date", 
    "documentation"
  ))
  expect_is(app_data$main_forecasting_table, "data.frame")
  expect_is(app_data$groupings, "data.frame")
  expect_is(app_data$fc_models, "character")
  expect_equal(app_data$is_hierarchical, TRUE)
  expect_is(app_data$hierarchy, "list")
  expect_is(app_data$hierarchical_cols, "character")
  expect_equal(app_data$is_multivariate, FALSE)
  expect_equal(app_data$has_original_actuals, FALSE)
  expect_is(app_data$latest_split_date, "numeric")
  expect_is(app_data$documentation, "list")
  expect_equal(nrow(app_data$main_forecasting_table), 1296)
  expect_equal(ncol(app_data$main_forecasting_table), 10)
  expect_equal(app_data$groupings$grouping, c(
    "location = USA   &   oil_company = CompanyC   &   currency = EUR", 
    "location = USA   &   oil_company = CompanyA   &   currency = EUR", 
    "location = USA   &   oil_company = CompanyB   &   currency = EUR", 
    "location = New York   &   oil_company = CompanyC   &   currency = EUR", 
    "location = New York   &   oil_company = CompanyA   &   currency = EUR", 
    "location = New York   &   oil_company = CompanyB   &   currency = EUR", 
    "location = North New York   &   oil_company = CompanyC   &   currency = EUR", 
    "location = North New York   &   oil_company = CompanyA   &   currency = EUR", 
    "location = North New York   &   oil_company = CompanyB   &   currency = EUR", 
    "location = South New York   &   oil_company = CompanyC   &   currency = EUR", 
    "location = South New York   &   oil_company = CompanyA   &   currency = EUR", 
    "location = South New York   &   oil_company = CompanyB   &   currency = EUR", 
    "location = Bronx   &   oil_company = CompanyC   &   currency = EUR", 
    "location = Bronx   &   oil_company = CompanyA   &   currency = EUR", 
    "location = Bronx   &   oil_company = CompanyB   &   currency = EUR", 
    "location = Queens   &   oil_company = CompanyC   &   currency = EUR", 
    "location = Queens   &   oil_company = CompanyA   &   currency = EUR", 
    "location = Queens   &   oil_company = CompanyB   &   currency = EUR", 
    "location = Indiana   &   oil_company = CompanyC   &   currency = EUR", 
    "location = Indiana   &   oil_company = CompanyA   &   currency = EUR", 
    "location = Indiana   &   oil_company = CompanyB   &   currency = EUR", 
    "location = North Indiana   &   oil_company = CompanyC   &   currency = EUR", 
    "location = North Indiana   &   oil_company = CompanyA   &   currency = EUR", 
    "location = North Indiana   &   oil_company = CompanyB   &   currency = EUR", 
    "location = South Indiana   &   oil_company = CompanyC   &   currency = EUR", 
    "location = South Indiana   &   oil_company = CompanyA   &   currency = EUR", 
    "location = South Indiana   &   oil_company = CompanyB   &   currency = EUR", 
    "location = USA   &   oil_company = CompanyC   &   currency = USD", 
    "location = USA   &   oil_company = CompanyA   &   currency = USD", 
    "location = USA   &   oil_company = CompanyB   &   currency = USD", 
    "location = New York   &   oil_company = CompanyC   &   currency = USD", 
    "location = New York   &   oil_company = CompanyA   &   currency = USD", 
    "location = New York   &   oil_company = CompanyB   &   currency = USD", 
    "location = North New York   &   oil_company = CompanyC   &   currency = USD", 
    "location = North New York   &   oil_company = CompanyA   &   currency = USD", 
    "location = North New York   &   oil_company = CompanyB   &   currency = USD", 
    "location = South New York   &   oil_company = CompanyC   &   currency = USD", 
    "location = South New York   &   oil_company = CompanyA   &   currency = USD", 
    "location = South New York   &   oil_company = CompanyB   &   currency = USD", 
    "location = Bronx   &   oil_company = CompanyC   &   currency = USD", 
    "location = Bronx   &   oil_company = CompanyA   &   currency = USD", 
    "location = Bronx   &   oil_company = CompanyB   &   currency = USD", 
    "location = Queens   &   oil_company = CompanyC   &   currency = USD", 
    "location = Queens   &   oil_company = CompanyA   &   currency = USD", 
    "location = Queens   &   oil_company = CompanyB   &   currency = USD", 
    "location = Indiana   &   oil_company = CompanyC   &   currency = USD", 
    "location = Indiana   &   oil_company = CompanyA   &   currency = USD", 
    "location = Indiana   &   oil_company = CompanyB   &   currency = USD", 
    "location = North Indiana   &   oil_company = CompanyC   &   currency = USD", 
    "location = North Indiana   &   oil_company = CompanyA   &   currency = USD", 
    "location = North Indiana   &   oil_company = CompanyB   &   currency = USD", 
    "location = South Indiana   &   oil_company = CompanyC   &   currency = USD", 
    "location = South Indiana   &   oil_company = CompanyA   &   currency = USD", 
    "location = South Indiana   &   oil_company = CompanyB   &   currency = USD"
  ))
  expect_equal(unique(app_data$groupings$location), c(
    "USA", "New York", "North New York", "South New York", "Bronx", 
    "Queens", "Indiana", "North Indiana", "South Indiana"
  ))
  expect_equal(unique(app_data$groupings$oil_company), c(
    "CompanyC", "CompanyA", "CompanyB"
  ))
  expect_equal(unique(app_data$groupings$currency), c(
    "EUR", "USD"
  ))
  expect_equal(app_data$fc_models, c(
    "fc_arima", "fc_arima_stl", "fc_bats", "fc_bottom_up", "fc_ctree", 
    "fc_drift_l12m", "fc_drift_l3m", "fc_drift_l6m", "fc_ensemble_aefnst", 
    "fc_ets_addiv", "fc_ets_addiv_damped", "fc_ets_multip", "fc_ets_multip_damped", 
    "fc_ets_stl", "fc_fforma", "fc_holt_winters_addiv", "fc_holt_winters_multip", 
    "fc_kalman_poly", "fc_kalman_seas_12", "fc_linear_trend", "fc_linear_trend_seasonal", 
    "fc_mean_l12m", "fc_mean_l3m", "fc_mean_l6m", "fc_naive", "fc_naive_seasonal", 
    "fc_nn_25n_0decay", "fc_nn_25n_50decay", "fc_nn_25n_elm", "fc_nn_5n_0decay", 
    "fc_nn_5n_50decay", "fc_nn_5n_mlp", "fc_prophet_005cps", "fc_prophet_050cps", 
    "fc_prophet_500cps", "fc_prophet_500cps_consistent", "fc_randomforest", 
    "fc_rec_cforest", "fc_rec_ctree", "fc_rec_rpart", "fc_rec_svmradsig", 
    "fc_rpart", "fc_tbats"
  ))
  expect_equal(names(app_data$hierarchy), c("matrix", "data"))
  expect_is(app_data$hierarchy$matrix, "matrix")
  expect_equal(nrow(app_data$hierarchy$matrix), 27)
  expect_equal(ncol(app_data$hierarchy$matrix), 10)
  expect_is(app_data$hierarchy$data, "data.frame")
  expect_equal(nrow(app_data$hierarchy$data), 27)
  expect_equal(ncol(app_data$hierarchy$data), 9)
  expect_equal(app_data$hierarchical_cols, c("location", "oil_company"))
  expect_equal(app_data$latest_split_date, 200611)
  expect_equal(app_data$documentation, list())
})

test_that("check run_example with invalid inputs", {
  expect_error(
    run_example(
      dataset = "potato"
    )
  )
  expect_error(
    run_example(
      dataset = 42
    )
  )
})
