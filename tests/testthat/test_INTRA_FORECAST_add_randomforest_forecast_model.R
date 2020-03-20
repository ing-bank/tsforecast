context("add_randomforest_forecast_model")

test_that("check add_randomforest_forecast_model with valid, differing, inputs", {
  xregs <- list("spotprice", "gemprice", c("spotprice", "gemprice"))
  for (xreg_cols in xregs) {
    function_input <- tstools::initialize_ts_forecast_data(
        data = dummy_gasprice,
        date_col = "year_month",
        col_of_interest = "gasprice",
        group_cols = c("state", "oil_company"),
        xreg_cols = xreg_cols
      ) %>% 
      dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
    ts_object_train <- function_input %>% 
      dplyr::slice(1:189) %>% 
      tstools::transform_data_to_ts_object()
    ts_object_valid <- function_input %>% 
      dplyr::slice(190:191) %>% 
      tstools::transform_data_to_ts_object()
    expect_silent(
      function_output <- list() %>% 
        add_randomforest_forecast_model(
          ts_object_train = ts_object_train,
          ts_object_valid = ts_object_valid,
          periods_ahead = 2,
          fc_name = "fc_randomforest_1",
          model_type = "multivariate",
          verbose = F
        ) %>% 
        add_randomforest_forecast_model(
          ts_object_train = ts_object_train,
          ts_object_valid = ts_object_valid,
          periods_ahead = 2,
          fc_name = "fc_randomforest_2",
          model_type = "multivariate",
          verbose = F
        ) %>% 
        add_randomforest_forecast_model(
          ts_object_train = ts_object_train,
          ts_object_valid = ts_object_valid,
          periods_ahead = 2,
          fc_name = "fc_randomforest_2",
          model_type = "multivariate",
          verbose = F
        )
    )
    expect_true(is.list(function_output))
    expect_equal(names(function_output), c("fc_randomforest_1", "fc_randomforest_2"))
    expect_equal(names(function_output$fc_randomforest_1), c("model", "fc_data"))
    expect_equal(names(function_output$fc_randomforest_2), c("model", "fc_data"))
    expect_equal(class(function_output$fc_randomforest_1$model), "numeric")
    expect_equal(class(function_output$fc_randomforest_2$model), "numeric")
    expect_equal(nrow(function_output$fc_randomforest_1$fc_data), 2)
    expect_equal(ncol(function_output$fc_randomforest_1$fc_data), 3)
    expect_equal(nrow(function_output$fc_randomforest_2$fc_data), 2)
    expect_equal(ncol(function_output$fc_randomforest_2$fc_data), 3)
    expect_equal(function_output$fc_randomforest_1$fc_data$period, c(200610, 200611))
    expect_equal(function_output$fc_randomforest_1$fc_data$fc_date %>% unique(), 200609)
    expect_equal(function_output$fc_randomforest_2$fc_data$period, c(200610, 200611))
    expect_equal(function_output$fc_randomforest_2$fc_data$fc_date %>% unique(), 200609)
  }
})

test_that("check add_randomforest_forecast_model for univariate models with valid, differing, inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object()
  capture.output(
    function_output <- list() %>% 
      add_randomforest_forecast_model(
        ts_object_train = function_input,
        periods_ahead = 2,
        fc_name = "fc_randomforest_1",
        model_type = "univariate",
        verbose = T
      ) %>% 
      add_randomforest_forecast_model(
        ts_object_train = function_input,
        periods_ahead = 2,
        fc_name = "fc_randomforest_2",
        model_type = "univariate",
        verbose = T
      ) %>% 
      add_randomforest_forecast_model(
        ts_object_train = function_input,
        periods_ahead = 2,
        fc_name = "fc_randomforest_2",
        model_type = "univariate",
        verbose = T
      ),
    file = 'NUL'
  )
  expect_true(is.list(function_output))
  expect_equal(names(function_output), c("fc_randomforest_1", "fc_randomforest_2"))
  expect_equal(names(function_output$fc_randomforest_1), c("model", "fc_data"))
  expect_equal(names(function_output$fc_randomforest_2), c("model", "fc_data"))
  expect_equal(class(function_output$fc_randomforest_1$model), "numeric")
  expect_equal(class(function_output$fc_randomforest_2$model), "numeric")
  expect_equal(nrow(function_output$fc_randomforest_1$fc_data), 2)
  expect_equal(ncol(function_output$fc_randomforest_1$fc_data), 3)
  expect_equal(nrow(function_output$fc_randomforest_2$fc_data), 2)
  expect_equal(ncol(function_output$fc_randomforest_2$fc_data), 3)
  expect_equal(function_output$fc_randomforest_1$fc_data$period, c(200612, 200701))
  expect_equal(function_output$fc_randomforest_1$fc_data$fc_date %>% unique(), 200611)
  expect_equal(function_output$fc_randomforest_2$fc_data$period, c(200612, 200701))
  expect_equal(function_output$fc_randomforest_2$fc_data$fc_date %>% unique(), 200611)
})

test_that("check add_randomforest_forecast_model for univariate models with invalid inputs", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA")
  ts_object_train <- function_input %>% 
    dplyr::slice(1:189) %>% 
    tstools::transform_data_to_ts_object()
  ts_object_valid <- function_input %>% 
    dplyr::slice(190:191) %>% 
    tstools::transform_data_to_ts_object()
  expect_error(
    add_randomforest_forecast_model(
      fc_models = "potato"
    )
  )
  expect_error(
    add_randomforest_forecast_model(
      fc_models = list(),
      ts_object_train = dummy_gasprice,
      periods_ahead = 12,
      fc_name = "fc_randomforest_1",
      model_type = "univariate"
    )
  )
  expect_error(
    add_randomforest_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = dummy_gasprice,
      periods_ahead = 12,
      fc_name = "fc_randomforest_1",
      model_type = "multivariate"
    )
  )
  expect_error(
    add_randomforest_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 2,
      fc_name = 42,
      model_type = "univariate"
    )
  )
  expect_error(
    add_randomforest_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 12,
      fc_name = "fc_randomforest_1",
      model_type = "omnivariate"
    )
  )
  expect_error(
    add_randomforest_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 12,
      fc_name = "fc_randomforest_1",
      model_type = "multivariate"
    )
  )
  expect_error(
    add_randomforest_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      ts_object_valid = ts_object_valid,
      periods_ahead = 12,
      fc_name = "fc_randomforest_1",
      model_type = "multivariate"
    )
  )
  expect_error(
    add_randomforest_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = -3,
      fc_name = "fc_randomforest_1",
      model_type = "univariate"
    )
  )
  expect_error(
    add_randomforest_forecast_model(
      fc_models = list(),
      ts_object_train = ts_object_train,
      periods_ahead = 12,
      periods_history = -3,
      fc_name = "fc_randomforest_1",
      model_type = "univariate"
    )
  )
})
