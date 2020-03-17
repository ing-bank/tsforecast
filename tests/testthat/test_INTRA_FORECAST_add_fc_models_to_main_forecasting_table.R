
context("add_fc_models_to_main_forecasting_table")

test_that("check add_fc_models_to_main_forecasting_table without xreg_cols and default periods_ahead", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) # To limit the runtime of the tests
  function_output <- add_fc_models_to_main_forecasting_table(
    main_forecasting_table = function_input,
    periods_ahead = 24,
    fc_methods = c("basic", "linear", "prophet", "tree", "ensemble"),
    add_fc_errors = F,
    parallel = T
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 6)
  expect_equal(ncol(function_output), 9)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid", "fc_models"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$fc_models), "list")
  expected_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", 
    "fc_drift_l3m", "fc_drift_l6m", "fc_drift_l12m", 
    "fc_naive", "fc_naive_seasonal",
    "fc_linear_trend", "fc_linear_trend_seasonal", 
    "fc_prophet_005cps", "fc_prophet_050cps", "fc_prophet_500cps",
    "fc_rpart", "fc_ctree",
    "fc_ensemble_aefnst"
  )
  fc_models <- function_output$fc_models[[1]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 24)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 199301)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 199302)
    expect_equal(fc_models[[exp_model]]$fc_data$period[24], 199501)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 24)
  }
  fc_models <- function_output$fc_models[[6]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 24)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200611)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200612)
    expect_equal(fc_models[[exp_model]]$fc_data$period[24], 200811)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 24)
  }
})

test_that("check add_fc_models_to_main_forecasting_table with xreg_cols and default periods_ahead", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = Inf
    ) %>%
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) # To limit the runtime of the tests
  function_output <- add_fc_models_to_main_forecasting_table(
    main_forecasting_table = function_input,
    periods_ahead = 24,
    fc_methods = c("arima", "nn"),
    parallel = F
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 6)
  expect_equal(ncol(function_output), 10)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$fc_models), "list")
  expect_equal(class(function_output$fc_errors), "list")
  expected_models <- c(
    "fc_arima_xreg", "fc_arima_stl_xreg", 
    "fc_nn_5n_0decay_xreg", "fc_nn_25n_0decay_xreg", "fc_nn_5n_50decay_xreg", "fc_nn_25n_50decay_xreg")
  fc_models <- function_output$fc_models[[1]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 24)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 199301)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 199302)
    expect_equal(fc_models[[exp_model]]$fc_data$period[24], 199501)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 24)
  }
  fc_models <- function_output$fc_models[[5]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 23)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 200412)
    expect_equal(fc_models[[exp_model]]$fc_data$period[1], 200501)
    expect_equal(fc_models[[exp_model]]$fc_data$period[23], 200611)
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 23)
  }
  fc_models <- function_output$fc_models[[6]]
  expect_equal(class(fc_models), "list")
  expect_equal(fc_models, list())
})

test_that("check add_fc_models_to_main_forecasting_table with xreg_cols and missing col_of_interest", {
  data <- dummy_gasprice %>% 
    dplyr::mutate(gasprice = ifelse(
        test = year_month >= as.Date("2006-01-31"), 
        yes = NA,
        no = gasprice
      )
    )
  function_input <- tstools::initialize_ts_forecast_data(
      data = data,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = Inf
    ) %>%
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) # To limit the runtime of the tests
  function_output <- add_fc_models_to_main_forecasting_table(
    main_forecasting_table = function_input,
    periods_ahead = 24,
    fc_methods = c("forest"),
    add_fc_errors = F,
    parallel = T
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 6)
  expect_equal(ncol(function_output), 9)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid", "fc_models"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$fc_models), "list")
  fc_models <- function_output$fc_models[[1]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), "fc_randomforest_xreg")
  expect_equal(names(fc_models$fc_randomforest_xreg), c("model", "fc_data"))
  expect_true(is.data.frame(fc_models$fc_randomforest_xreg$fc_data))
  expect_equal(nrow(fc_models$fc_randomforest_xreg$fc_data), 24)
  expect_equal(ncol(fc_models$fc_randomforest_xreg$fc_data), 3)
  expect_equal(unique(fc_models$fc_randomforest_xreg$fc_data$fc_date), 199301)
  expect_equal(fc_models$fc_randomforest_xreg$fc_data$period[1], 199302)
  expect_equal(fc_models$fc_randomforest_xreg$fc_data$period[24], 199501)
  expect_equal(length(fc_models$fc_randomforest_xreg$fc_data$period), 24)
  fc_models <- function_output$fc_models[[5]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), "fc_randomforest_xreg")
  expect_equal(names(fc_models$fc_randomforest_xreg), c("model", "fc_data"))
  expect_true(is.data.frame(fc_models$fc_randomforest_xreg$fc_data))
  expect_equal(nrow(fc_models$fc_randomforest_xreg$fc_data), 23)
  expect_equal(ncol(fc_models$fc_randomforest_xreg$fc_data), 3)
  expect_equal(unique(fc_models$fc_randomforest_xreg$fc_data$fc_date), 200412)
  expect_equal(fc_models$fc_randomforest_xreg$fc_data$period[1], 200501)
  expect_equal(fc_models$fc_randomforest_xreg$fc_data$period[23], 200611)
  expect_equal(length(fc_models$fc_randomforest_xreg$fc_data$period), 23)
  fc_models <- function_output$fc_models[[6]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), "fc_randomforest_xreg")
})

test_that("check that add_fc_models_to_main_forecasting_table works with adding 
          forecasts of longer horizon with previous forecasts", {
  data <- dummy_gasprice %>% 
    dplyr::mutate(gasprice = ifelse(
        test = year_month >= as.Date("2006-01-31"), 
        yes = NA,
        no = gasprice
      )
    )
  function_input <- tstools::initialize_ts_forecast_data(
      data = data,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
    create_main_forecasting_table(
      seasonal_periods = c(3),
      min_train_periods = 12,
      max_train_periods = Inf
    ) %>%
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) # To limit the runtime of the tests
  # First creates some forcasts with shorter horizon
  function_output_1 <- add_fc_models_to_main_forecasting_table(
    main_forecasting_table = function_input,
    periods_ahead = 4,
    fc_methods = c("linear"),
    add_fc_errors = F,
    parallel = F
  )
  fc_models_1 <- function_output_1$fc_models[[1]]
  expect_equal(nrow(fc_models_1$fc_linear_trend$fc_data), 4)
  # Then tests to create forecats with longer horizon
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_output_1,
      periods_ahead = 8,
      fc_methods = c("linear"),
      add_fc_errors = F,
      overwrite_fc = FALSE,
      parallel = T
    )
  )
  function_output_2 <- add_fc_models_to_main_forecasting_table(
    main_forecasting_table = function_output_1,
    periods_ahead = 8,
    fc_methods = c("linear"),
    add_fc_errors = F,
    overwrite_fc = TRUE,
    parallel = F
  )
  fc_models_2 <- function_output_2$fc_models[[1]]
  expect_equal(nrow(fc_models_2$fc_linear_trend$fc_data), 8)
})

test_that("check that add_fc_models_to_main_forecasting_table works with adding 
          forecasts of longer horizon with previous forecasts.", {
  data <- dummy_gasprice %>% 
    dplyr::mutate(gasprice = ifelse(
        test = year_month >= as.Date("2006-01-31"), 
        yes = NA,
        no = gasprice
      )
    )
  function_input <- tstools::initialize_ts_forecast_data(
      data = data,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>%
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>%
    create_main_forecasting_table(
      seasonal_periods = c(3),
      min_train_periods = 12,
      max_train_periods = Inf
    ) %>%
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) # To limit the runtime of the tests
  # First creates some forcasts with shorter horizon
  function_output_1 <- add_fc_models_to_main_forecasting_table(
    main_forecasting_table = function_input,
    periods_ahead = 4,
    fc_methods = c("linear"),
    add_fc_errors = F,
    parallel = T
  )
  fc_models_1 <- function_output_1$fc_models[[1]]
  expect_equal(nrow(fc_models_1$fc_linear_trend$fc_data), 4)
  # Then tests to create forecats with longer horizon
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_output_1,
      periods_ahead = 8,
      fc_methods = c("linear"),
      add_fc_errors = F,
      overwrite_fc = F,
      parallel = F
    )
  )
  function_output_2 <- add_fc_models_to_main_forecasting_table(
    main_forecasting_table = function_output_1,
    periods_ahead = 8,
    fc_methods = c("linear"),
    add_fc_errors = F,
    overwrite_fc = TRUE,
    parallel = T
  )
  fc_models_2 <- function_output_2$fc_models[[1]]
  expect_equal(nrow(fc_models_2$fc_linear_trend$fc_data), 8)
})

test_that("check add_fc_models_to_main_forecasting_table with fforma", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 25,
      max_train_periods = Inf
    ) %>% 
    dplyr::filter(train_length %% 42 == 0 | valid_length == max(valid_length) | valid_length == 0) # To limit the runtime of the tests
  function_output <- add_fc_models_to_main_forecasting_table(
    main_forecasting_table = function_input,
    periods_ahead = 24,
    fc_methods = c("basic", "linear", "tree", "fforma"),
    add_fc_errors = T,
    parallel = T
  )
  expect_equal(class(function_output), c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(function_output), 6)
  expect_equal(ncol(function_output), 10)
  expect_equal(colnames(function_output), c("grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", "ts_object_train", "ts_object_valid", "fc_models", "fc_errors"))
  expect_equal(class(function_output$grouping), "character")
  expect_equal(class(function_output$ts_start), "numeric")
  expect_equal(class(function_output$ts_split_date), "numeric")
  expect_equal(class(function_output$ts_end), "numeric")
  expect_equal(class(function_output$train_length), "numeric")
  expect_equal(class(function_output$valid_length), "numeric")
  expect_equal(class(function_output$ts_object_train), "list")
  expect_equal(class(function_output$ts_object_valid), "list")
  expect_equal(class(function_output$fc_models), "list")
  expect_equal(class(function_output$fc_errors), "list")
  expected_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_drift_l3m", 
    "fc_drift_l6m", "fc_drift_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal", "fc_rpart", "fc_ctree", 
    "fc_fforma"
  )
  expected_model_classes <- c(
    "meanf", "lagwalk", "numeric", "character", "tbl_df", "tbl", "data.frame"
  )
  fc_models <- function_output$fc_models[[1]]
  expect_equal(class(fc_models), "list")
  expect_equal(names(fc_models), expected_models)
  for (exp_model in expected_models) {
    expect_equal(names(fc_models[[exp_model]]), c("model", "fc_data"))
    expect_true(is.data.frame(fc_models[[exp_model]]$fc_data))
    expect_true(all(class(fc_models[[exp_model]]$model) %in% expected_model_classes))
    expect_equal(nrow(fc_models[[exp_model]]$fc_data), 24)
    expect_equal(ncol(fc_models[[exp_model]]$fc_data), 3)
    expect_equal(unique(fc_models[[exp_model]]$fc_data$fc_date), 199301)
    expect_equal(fc_models[[exp_model]]$fc_data$period, c(199302:199312, 199401:199412, 199501))
    expect_equal(length(fc_models[[exp_model]]$fc_data$period), 24)
  }
  for (i in 1:nrow(function_output)) {
    fc_errors <- function_output$fc_errors[[i]]
    expect_equal(nrow(fc_errors), 312)
    expect_equal(ncol(fc_errors), 9)
    expect_equal(colnames(fc_errors), c(
      "grouping", "fc_model", "fc_date", "period", "fc_periods_ahead", 
      "fc_value", "actual", "fc_error", "MASE"
    ))
    ext_fc_models <- unique(fc_errors$fc_model)
    expect_equal(ext_fc_models, sort(expected_models))
    expect_equal(length(unique(fc_errors$fc_date)), 1)
    expect_equal(length(unique(fc_errors$period )), 24)
    expect_equal(unique(fc_errors$fc_periods_ahead), 1:24)
  }
})

test_that("check add_fc_models_to_main_forecasting_table when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    create_main_forecasting_table(
      seasonal_periods = c(12,3),
      min_train_periods = 180,
      max_train_periods = Inf
    )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_input %>% 
        dplyr::select(-ts_object_train)
    )
  )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_input %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_input,
      periods_ahead = -42
    )
  )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_input,
      periods_ahead = 4.2
    )
  )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_input,
      fc_methods = c("basic", "linear", "prophet", "it's", "a", "kind", "of", "magic")
    )
  )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_input,
      fc_methods = list()
    )
  )
  expect_error(
    add_fc_models_to_main_forecasting_table(
      main_forecasting_table = function_input,
      fc_methods = c("basic")
    )
  )
})
