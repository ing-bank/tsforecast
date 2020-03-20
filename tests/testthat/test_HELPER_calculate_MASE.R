
context("calculate_MASE")

test_that("check calculate_MASE with fc_linear_trend_seasonal", {
  ts_split_dates <- c(200207:200212, 200301:200306)
  groupings <- c(
    "state = New York   &   oil_company = CompanyA",
    "state = New York   &   oil_company = CompanyB",
    "state = Indiana   &   oil_company = CompanyA",
    "state = Indiana   &   oil_company = CompanyB"
  )
  for (ts_split_date in ts_split_dates) {
    for (group in groupings) {
      ts_objects <- tstools::initialize_ts_forecast_data(
          data = dummy_gasprice,
          date_col = "year_month",
          col_of_interest = "gasprice",
          group_cols = c("state", "oil_company")
        ) %>% 
        dplyr::filter(grouping == group) %>% 
        tstools::transform_data_to_ts_object() %>% 
        split_ts_object(ts_split_date = ts_split_date)
      fc_errors <- calculate_forecast_errors(
        fc_models = add_all_univariate_forecast_models(
          ts_object_train = ts_objects$train,
          periods_ahead = 36,
          fc_methods = c("linear")
        ),
        ts_object_train = ts_objects$train,
        ts_object_valid = ts_objects$valid
      )
      function_output <- calculate_MASE(
        ts_object_train = ts_objects$train,
        fc_error = fc_errors %>% 
          dplyr::filter(fc_model == "fc_linear_trend_seasonal") %>% 
          dplyr::pull(fc_error)
      )
      expect_is(function_output, "numeric")
      expect_equal(length(function_output), 36)
      fc_model <- forecast::tslm(col_of_interest ~ trend + season, data = ts_objects$train)
      forecast_package_accuracy <- accuracy(
        f = forecast(fc_model, h = 36), 
        x = ts_objects$valid %>% 
          trim_ts_object(max_length = 36)
      )
      MASE_from_forecast_package <- forecast_package_accuracy[2,6]
      expect_equal(
        mean(function_output, na.rm = T),
        MASE_from_forecast_package
      )
    }
  }
})

test_that("check calculate_MASE when invalid inputs are used", {
  function_input <- tstools::initialize_ts_forecast_data(
      data = dummy_gasprice,
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("state", "oil_company"),
      xreg_cols = c("spotprice", "gemprice")
    ) %>% 
    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
    tstools::transform_data_to_ts_object() %>% 
    split_ts_object(ts_split_date = 200301)
  expect_error(
    calculate_MASE(
      ts_object_train = dummy_gasprice
    )
  )
  expect_error(
    calculate_MASE(
      ts_object_train = data.frame()
    )
  )
  expect_error(
    calculate_MASE(
      ts_object_train = list()
    )
  )
  expect_error(
    calculate_MASE(
      ts_object_train = function_input$train,
      fc_error = data.frame()
    )
  )
  expect_error(
    calculate_MASE(
      ts_object_train = function_input$train,
      fc_error = list()
    )
  )
  expect_error(
    calculate_MASE(
      ts_object_train = function_input$train,
      fc_error = c("We", "got", "a", "pony", "plan")
    )
  )
})
