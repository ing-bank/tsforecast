
context("add_multivariate_forecast_model")

test_that("check add_multivariate_forecast_model with different inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  ts_object_train <- function_input %>%
    dplyr::slice(1:179) %>% 
    tstools::transform_data_to_ts_object()
  ts_object_valid <- function_input %>%
    dplyr::slice(180:191) %>% 
    tstools::transform_data_to_ts_object()
  ts_object_valid_adjusted <- ts_object_valid
  ts_object_valid_adjusted[,'col_of_interest'] <- NA
  fc_models <- expect_silent(
    list() %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_linear_trend_xreg",
        fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
        periods_ahead = 12,
        verbose = F
      ) %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid,
        fc_name = "fc_arima_xreg",
        fc_formula = "forecast::auto.arima(force_to_univariate_ts_object(ts_object_train), stepwise = F, approximation = T, parallel = T, xreg = ts_object_train[,c('spotprice', 'gemprice')])",
        periods_ahead = 12,
        periods_history = 36,
        verbose = F
      )
  )
  capture.output(
    fc_models <- fc_models %>% 
      add_multivariate_forecast_model(
        ts_object_train = ts_object_train,
        ts_object_valid = ts_object_valid_adjusted,
        fc_name = "fc_nn_5hln_50decay_xreg",
        fc_formula = paste0("forecast::nnetar(force_to_univariate_ts_object(ts_object_train), size = 5, decay = 0.50, xreg = ts_object_train[,c('spotprice', 'gemprice')])"),
        periods_ahead = 12,
        periods_history = 1000,
        verbose = T
      ),
    file = 'NUL'
  )
  expect_true(is.list(fc_models))
  expect_equal(names(fc_models), c("fc_linear_trend_xreg", "fc_arima_xreg", "fc_nn_5hln_50decay_xreg"))
  expect_equal(names(fc_models$fc_linear_trend_xreg), c("model", "fc_data"))
  expect_equal(names(fc_models$fc_arima_xreg), c("model", "fc_data"))
  expect_equal(names(fc_models$fc_nn_5hln_50decay_xreg), c("model", "fc_data"))
  expect_true(is.data.frame(fc_models$fc_linear_trend_xreg$fc_data))
  expect_true(is.data.frame(fc_models$fc_arima_xreg$fc_data))
  expect_true(is.data.frame(fc_models$fc_nn_5hln_50decay_xreg$fc_data))
  expect_equal(class(fc_models$fc_linear_trend_xreg$model), "numeric")
  expect_equal(class(fc_models$fc_arima_xreg$model), "numeric")
  expect_equal(class(fc_models$fc_nn_5hln_50decay_xreg$model), "character")
  expect_equal(nrow(fc_models$fc_linear_trend_xreg$fc_data), 12)
  expect_equal(ncol(fc_models$fc_linear_trend_xreg$fc_data), 3)
  expect_equal(unique(fc_models$fc_linear_trend_xreg$fc_data$fc_date), 200511)
  expect_equal(min(fc_models$fc_linear_trend_xreg$fc_data$period), 200512)
  expect_equal(max(fc_models$fc_linear_trend_xreg$fc_data$period), 200611)
  expect_equal(length(fc_models$fc_linear_trend_xreg$fc_data$period), 12)
  expect_equal(nrow(fc_models$fc_arima_xreg$fc_data), 12)
  expect_equal(ncol(fc_models$fc_arima_xreg$fc_data), 3)
  expect_equal(unique(fc_models$fc_arima_xreg$fc_data$fc_date), 200511)
  expect_equal(min(fc_models$fc_arima_xreg$fc_data$period), 200512)
  expect_equal(max(fc_models$fc_arima_xreg$fc_data$period), 200611)
  expect_equal(length(fc_models$fc_arima_xreg$fc_data$period), 12)
  expect_equal(nrow(fc_models$fc_nn_5hln_50decay_xreg$fc_data), 12)
  expect_equal(ncol(fc_models$fc_nn_5hln_50decay_xreg$fc_data), 3)
  expect_equal(unique(fc_models$fc_nn_5hln_50decay_xreg$fc_data$fc_date), 200511)
  expect_equal(min(fc_models$fc_nn_5hln_50decay_xreg$fc_data$period), 200512)
  expect_equal(max(fc_models$fc_nn_5hln_50decay_xreg$fc_data$period), 200611)
  expect_equal(length(fc_models$fc_nn_5hln_50decay_xreg$fc_data$period), 12)
})

test_that("check add_multivariate_forecast_model when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  ts_object_train <- function_input %>%
    dplyr::slice(1:179) %>% 
    tstools::transform_data_to_ts_object()
  ts_object_valid <- function_input %>%
    dplyr::slice(180:191) %>% 
    tstools::transform_data_to_ts_object()
  expect_error(
    add_multivariate_forecast_model(
      fc_models = data.frame(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_name = "fc_linear_trend_xreg",
      fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
      periods_ahead = 12
    )
  )
  expect_error(
    add_multivariate_forecast_model(
      fc_models = list(),
      ts_object_train = data.frame(),
      ts_object_valid = ts_object_valid,
      fc_name = "fc_linear_trend_xreg",
      fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
      periods_ahead = 12
    )
  )
  expect_error(
    add_multivariate_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = data.frame(),
      fc_name = "fc_linear_trend_xreg",
      fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
      periods_ahead = 12
    )
  )
  expect_error(
    add_multivariate_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_name = 42,
      fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)"
    )
  )
  expect_error(
    add_multivariate_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_name = "fc_linear_trend_xreg",
      fc_formula = 42
    )
  )
  expect_error(
    add_multivariate_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_name = "fc_linear_trend_xreg",
      fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
      periods_ahead = -42
    )
  )
  expect_error(
    add_multivariate_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_name = "fc_linear_trend_xreg",
      fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
      periods_ahead = 4.2
    )
  )
  expect_error(
    add_multivariate_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_name = "fc_linear_trend_xreg",
      fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
      periods_history = -42
    )
  )
  expect_error(
    add_multivariate_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      fc_name = "fc_linear_trend_xreg",
      fc_formula = "forecast::tslm(col_of_interest ~ trend + spotprice + gemprice, data = ts_object_train)",
      periods_history = 4.2
    )
  )
})
