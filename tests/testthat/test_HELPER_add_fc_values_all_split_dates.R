
context("add_fc_values_all_split_dates")

test_that("check add_fc_values_all_split_dates for valid univariate input", {
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company %in% c("CompanyA", "CompanyB")) %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("currency", "oil_company"),
      hierarchical_col = "location"
    ) %>%
    dplyr::filter(period >= as.Date("2004-09-30")) %>%
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  best_model_per_group <- function_input %>%
    get_forecast_accuracy_overview(metric = "MAE") %>%
    get_best_forecast_methods(filter_incomplete_fc = T)
  model_types <- list(
    c("consistent", "bottom-up"),
    "consistent",
    "bottom-up"
  )
  base_models <- c(
    "fc_mean_l3m", "fc_mean_l6m", "fc_mean_l12m", "fc_drift_l3m", 
    "fc_drift_l6m", "fc_drift_l12m", "fc_naive", "fc_naive_seasonal", 
    "fc_linear_trend", "fc_linear_trend_seasonal"
  )
  for (m in model_types) {
    function_output <- add_fc_values_all_split_dates(
      main_forecasting_table = function_input,
      model_types = m,
      best_model_per_group = best_model_per_group
    )
    expect_equal(colnames(function_input), colnames(function_output))
    expect_equal(class(function_input), class(function_output))
    expect_equal(nrow(function_input), nrow(function_output))
    expect_equal(function_output$grouping, function_input$grouping)
    expect_equal(function_output$ts_start, function_input$ts_start)
    expect_equal(function_output$ts_split_date, function_input$ts_split_date)
    expect_equal(function_output$ts_end, function_input$ts_end)
    expect_equal(function_output$train_length, function_input$train_length)
    expect_equal(function_output$valid_length, function_input$valid_length)
    expect_equal(function_output$ts_object_train, function_input$ts_object_train)
    expect_equal(function_output$ts_object_valid, function_input$ts_object_valid)
    expect_equal(function_output$hierarchy, function_input$hierarchy)
    expect_equal(function_output$fc_errors, function_input$fc_errors)
    if (all(m == c("consistent", "bottom-up"))) {
      expect_equal(names(function_output$fc_models[[1]]), c(base_models, "fc_mean_l12m_consistent", "fc_bottom_up"))
      expect_equal(names(function_output$fc_models[[33]]), c(base_models, "fc_drift_l6m_consistent", "fc_bottom_up"))
      expect_equal(round(mean(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 13.549)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 13.549)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 13.549)
      expect_equal(round(mean(function_output$fc_models[[50]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 2.563)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 2.563)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 2.563)
      expect_equal(round(median(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$period), 3), 200703.5)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$period), 3), 200610)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$period), 3), 200709)
      expect_equal(round(median(function_output$fc_models[[24]]$fc_drift_l3m_consistent$fc_data$period), 3), 200705.5)
      expect_equal(round(min(function_output$fc_models[[24]]$fc_drift_l3m_consistent$fc_data$period), 3), 200612)
      expect_equal(round(max(function_output$fc_models[[24]]$fc_drift_l3m_consistent$fc_data$period), 3), 200711)
      expect_equal(unique(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$fc_date), 200609)
      expect_equal(unique(function_output$fc_models[[50]]$fc_mean_l12m_consistent$fc_data$fc_date), 200610)
      expect_equal(round(mean(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 13.549)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 13.549)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 13.549)
      expect_equal(round(mean(function_output$fc_models[[43]]$fc_bottom_up$fc_data$fc_value), 3), 4.692)
      expect_equal(round(min(function_output$fc_models[[43]]$fc_bottom_up$fc_data$fc_value), 3), 4.036)
      expect_equal(round(max(function_output$fc_models[[43]]$fc_bottom_up$fc_data$fc_value), 3), 5.247)
      expect_equal(round(median(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200703.5)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200610)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200709)
      expect_equal(round(median(function_output$fc_models[[20]]$fc_bottom_up$fc_data$period), 3), 200704.5)
      expect_equal(round(min(function_output$fc_models[[20]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(round(max(function_output$fc_models[[20]]$fc_bottom_up$fc_data$period), 3), 200710)
      expect_equal(unique(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_date), 200609)
      expect_equal(unique(function_output$fc_models[[33]]$fc_bottom_up$fc_data$fc_date), 200611)
    }
    if (all(m == "consistent")) {
      expect_equal(names(function_output$fc_models[[1]]), c(base_models, "fc_mean_l12m_consistent"))
      expect_equal(names(function_output$fc_models[[33]]), c(base_models, "fc_drift_l6m_consistent"))
      expect_equal(round(mean(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 13.549)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 13.549)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 13.549)
      expect_equal(round(mean(function_output$fc_models[[50]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 2.563)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 2.563)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_mean_l12m_consistent$fc_data$fc_value), 3), 2.563)
      expect_equal(round(median(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$period), 3), 200703.5)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$period), 3), 200610)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$period), 3), 200709)
      expect_equal(round(median(function_output$fc_models[[24]]$fc_drift_l3m_consistent$fc_data$period), 3), 200705.5)
      expect_equal(round(min(function_output$fc_models[[24]]$fc_drift_l3m_consistent$fc_data$period), 3), 200612)
      expect_equal(round(max(function_output$fc_models[[24]]$fc_drift_l3m_consistent$fc_data$period), 3), 200711)
      expect_equal(unique(function_output$fc_models[[1]]$fc_mean_l12m_consistent$fc_data$fc_date), 200609)
      expect_equal(unique(function_output$fc_models[[50]]$fc_mean_l12m_consistent$fc_data$fc_date), 200610)
    }
    if (all(m == "bottom-up")) {
      expect_equal(names(function_output$fc_models[[1]]), c(base_models, "fc_bottom_up"))
      expect_equal(names(function_output$fc_models[[6]]), c(base_models, "fc_bottom_up"))
      expect_equal(round(mean(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 13.549)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 13.549)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 13.549)
      expect_equal(round(mean(function_output$fc_models[[43]]$fc_bottom_up$fc_data$fc_value), 3), 4.692)
      expect_equal(round(min(function_output$fc_models[[43]]$fc_bottom_up$fc_data$fc_value), 3), 4.036)
      expect_equal(round(max(function_output$fc_models[[43]]$fc_bottom_up$fc_data$fc_value), 3), 5.247)
      expect_equal(round(median(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200703.5)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200610)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200709)
      expect_equal(round(median(function_output$fc_models[[20]]$fc_bottom_up$fc_data$period), 3), 200704.5)
      expect_equal(round(min(function_output$fc_models[[20]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(round(max(function_output$fc_models[[20]]$fc_bottom_up$fc_data$period), 3), 200710)
      expect_equal(unique(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_date), 200609)
      expect_equal(unique(function_output$fc_models[[33]]$fc_bottom_up$fc_data$fc_date), 200611)
    }
  }
  invalid_model_types <- c(
    "ponies", "fc_rpart", "fc_mean_l12m", "A lot of ponies",
    "consistent", "bottom-up"
  )
  missing_best_models <- best_model_per_group %>% 
    dplyr::slice(1:8)
  wrong_name_best_models <- best_model_per_group %>% 
    dplyr::mutate(
      fc_model = case_when(
        fc_model == "fc_naive_seasonal" ~ "bobby",
        TRUE ~ fc_model
      )
    )
  invalid_model_per_group <- list(
    c("prophet", "fc_rpart", "amazing_ML_model"), 
    missing_best_models, 
    wrong_name_best_models,
    42, NA, list()
  )
  for (model_type in invalid_model_types) {
    for (model in invalid_model_per_group) {
      expect_error(
        add_fc_values_all_split_dates(
          main_forecasting_table = function_input,
          model_type = model_type,
          best_model_per_group = model
        )
      )
    }
  }
})

test_that("check add_fc_values_all_split_dates for valid multivariate input", {
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(currency == "USD") %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      xreg_cols = c("spotprice", "gemprice"),
      hierarchical_col = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>%
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    ) %>% 
    dplyr::filter(ts_split_date != 200611)
  best_model_per_group <- function_input %>%
    get_forecast_accuracy_overview(metric = "MASE") %>%
    get_best_forecast_methods(filter_incomplete_fc = T)
  model_types <- list(
    c("consistent", "bottom-up"),
    "consistent",
    "bottom-up"
  )
  base_models <- c(
    "fc_linear_trend_xreg", "fc_linear_trend_seasonal_xreg"
  )
  for (m in model_types) {
    function_output <- add_fc_values_all_split_dates(
      main_forecasting_table = function_input,
      model_types = m,
      best_model_per_group = best_model_per_group
    )
    expect_equal(colnames(function_input), colnames(function_output))
    expect_equal(class(function_input), class(function_output))
    expect_equal(nrow(function_input), nrow(function_output))
    expect_equal(function_output$grouping, function_input$grouping)
    expect_equal(function_output$ts_start, function_input$ts_start)
    expect_equal(function_output$ts_split_date, function_input$ts_split_date)
    expect_equal(function_output$ts_end, function_input$ts_end)
    expect_equal(function_output$train_length, function_input$train_length)
    expect_equal(function_output$valid_length, function_input$valid_length)
    expect_equal(function_output$ts_object_train, function_input$ts_object_train)
    expect_equal(function_output$ts_object_valid, function_input$ts_object_valid)
    expect_equal(function_output$hierarchy, function_input$hierarchy)
    expect_equal(function_output$fc_errors, function_input$fc_errors)
    if (all(m == c("consistent", "bottom-up"))) {
      expect_equal(names(function_output$fc_models[[1]]), c(base_models, "fc_linear_trend_xreg_consistent", "fc_bottom_up"))
      expect_equal(names(function_output$fc_models[[50]]), c(base_models, "fc_linear_trend_xreg_consistent", "fc_bottom_up"))
      expect_equal(round(mean(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 29.709)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 27.668)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 31.775)
      expect_equal(round(mean(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 10.464)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 10.464)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 10.464)
      expect_equal(round(median(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200609)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200607)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
      expect_equal(round(median(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
      expect_equal(unique(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_date), 200606)
      expect_equal(unique(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$fc_date), 200610)
      expect_equal(round(mean(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 30.373)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 29.233)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 31.629)
      expect_equal(round(mean(function_output$fc_models[[50]]$fc_bottom_up$fc_data$fc_value), 3), 11.318)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_bottom_up$fc_data$fc_value), 3), 11.318)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_bottom_up$fc_data$fc_value), 3), 11.318)
      expect_equal(round(median(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200609)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200607)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(round(median(function_output$fc_models[[50]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(unique(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_date), 200606)
      expect_equal(unique(function_output$fc_models[[50]]$fc_bottom_up$fc_data$fc_date), 200610)
    }
    if (all(m == "consistent")) {
      expect_equal(names(function_output$fc_models[[1]]), c(base_models, "fc_linear_trend_xreg_consistent"))
      expect_equal(names(function_output$fc_models[[33]]), c(base_models, "fc_linear_trend_xreg_consistent"))
      expect_equal(round(mean(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 29.709)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 27.668)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 31.775)
      expect_equal(round(mean(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 10.464)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 10.464)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$fc_value), 3), 10.464)
      expect_equal(round(median(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200609)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200607)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
      expect_equal(round(median(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$period), 3), 200611)
      expect_equal(unique(function_output$fc_models[[1]]$fc_linear_trend_xreg_consistent$fc_data$fc_date), 200606)
      expect_equal(unique(function_output$fc_models[[50]]$fc_linear_trend_xreg_consistent$fc_data$fc_date), 200610)
    }
    if (all(m == "bottom-up")) {
      expect_equal(names(function_output$fc_models[[1]]), c(base_models, "fc_bottom_up"))
      expect_equal(names(function_output$fc_models[[6]]), c(base_models, "fc_bottom_up"))
      expect_equal(round(mean(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 30.373)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 29.233)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_value), 3), 31.629)
      expect_equal(round(mean(function_output$fc_models[[50]]$fc_bottom_up$fc_data$fc_value), 3), 11.318)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_bottom_up$fc_data$fc_value), 3), 11.318)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_bottom_up$fc_data$fc_value), 3), 11.318)
      expect_equal(round(median(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200609)
      expect_equal(round(min(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200607)
      expect_equal(round(max(function_output$fc_models[[1]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(round(median(function_output$fc_models[[50]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(round(min(function_output$fc_models[[50]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(round(max(function_output$fc_models[[50]]$fc_bottom_up$fc_data$period), 3), 200611)
      expect_equal(unique(function_output$fc_models[[1]]$fc_bottom_up$fc_data$fc_date), 200606)
      expect_equal(unique(function_output$fc_models[[50]]$fc_bottom_up$fc_data$fc_date), 200610)
    }
  }
  invalid_model_types <- c(
    "ponies","fc_rpart", "fc_mean_l12m", "A lot of ponies",
    "consistent", "bottom-up"
  )
  missing_best_models <- best_model_per_group %>% 
    dplyr::slice(1:8)
  wrong_name_best_models <- best_model_per_group %>% 
    dplyr::mutate(
      fc_model = case_when(
        fc_model == "fc_linear_trend_xreg" ~ "bobby",
        TRUE ~ fc_model
      )
    )
  invalid_model_per_group <- list(
    c("prophet", "fc_rpart", "amazing_ML_model"), 
    missing_best_models, 
    wrong_name_best_models,
    42, NA, list()
  )
  for (model_type in invalid_model_types) {
    for (model in invalid_model_per_group) {
      expect_error(
        add_fc_values_all_split_dates(
          main_forecasting_table = function_input,
          model_type = model_type,
          best_model_per_group = model
        )
      )
    }
  }
})

test_that("check add_fc_values_all_splits_dates for invalid inputs", {
  univariate_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyA") %>%
    dplyr::filter(currency == "EUR") %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("location", "oil_company", "currency")
    ) %>%
    dplyr::filter(period >= as.Date("2004-10-30")) %>%
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  multivariate_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyB") %>%
    dplyr::filter(currency == "USD") %>%
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("location", "oil_company", "currency"),
      xreg_cols = c("gemprice", "spotprice")
    ) %>%
    dplyr::filter(period >= as.Date("2004-10-30")) %>%
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  invalid_inputs <- list(
    univariate_input, multivariate_input, 
    dummy_gasprice, dummy_hierarchical_gasprice, 
    NA, 42, "potatoes"
  )
  model_types <- list(
    c("consistent", "bottom-up"),
    "consistent",
    "bottom-up"
  )
  best_model_per_group <- univariate_input %>%
    get_forecast_accuracy_overview(metric = "MAPE") %>%
    get_best_forecast_methods(filter_incomplete_fc = T)
  for (input in invalid_inputs) {
    for (model_type in model_types) {
      expect_error(
        add_fc_values_all_split_dates(
          main_forecasting_table = input,
          model_type = model_type,
          best_model_per_group = best_model_per_group
        )
      ) 
    }
  }
})
