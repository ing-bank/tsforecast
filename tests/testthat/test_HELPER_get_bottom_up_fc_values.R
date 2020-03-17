
context("get_bottom_up_fc_values")

# TODO: add test for hierarchical without group_cols!

test_that("check get_bottom_up_fc_values with valid univariate inputs and non-hierarchical groups", {
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyB") %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("currency", "oil_company"),
      hierarchical_col = "location"
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>% 
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200606) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  valid_best_models <- function_input %>% 
    get_forecast_accuracy_overview(metric = "MAE") %>% 
    get_best_forecast_methods(filter_incomplete_fc = T)
  function_output <- get_bottom_up_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = valid_best_models
  )
  expect_is(function_output, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(function_output), c("fc_date", "period", "fc_value", "grouping", "best_model"))
  expect_equal(nrow(function_output), 216)
  expect_equal(unique(function_output$fc_date), 200606)
  expect_equal(min(function_output$period), 200607)
  expect_equal(max(function_output$period), 200706)
  expect_equal(median(function_output$period), 200656.5)
  expect_equal(length(unique(function_output$grouping)), 18)
  location_order <- function_output %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(location) %>% 
    dplyr::distinct() %>%
    tstools::add_grouping_column(group_cols = "location") %>% 
    dplyr::pull()
  hierarchy_matrix <- function_input$hierarchy[[1]]$matrix
  expect_equal(location_order, rownames(hierarchy_matrix))
  expect_equal(class(function_output$best_model), "character")
  expect_equal(unique(function_output$best_model), "fc_bottom_up")
  expect_equal(class(function_output$fc_value), "numeric")
  expect_equal(round(min(function_output$fc_value), 4), 0.5790)
  expect_equal(round(max(function_output$fc_value), 4), 15.5568)
  expect_equal(round(median(function_output$fc_value), 4), 3.1961)
  expect_equal(round(mean(function_output$fc_value), 4), 4.3279)
  # Make best_model_per_group invalid
  missing_best_models <- valid_best_models %>% 
    dplyr::slice(1:8)
  wrong_name_best_models <- valid_best_models %>% 
    dplyr::mutate(
      fc_model = case_when(
        fc_model == "fc_drift_l3m" ~ "bobby",
        TRUE ~ fc_model
      )
    )
  invalid_best_model_per_group <- list(
    c("prophet", "fc_rpart", "amazing_ML_model"), 
    missing_best_models, 
    wrong_name_best_models,
    42, NA, list()
  )
  for (model_per_group in invalid_best_model_per_group) {
    expect_error(
      get_bottom_up_fc_values(
        main_forecasting_table = function_input,
        best_model_per_group = model_per_group
      )
    )
  }
})

test_that("check get_bottom_up_fc_values with valid univariate inputs and wihout non-hierarchical groups", {
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyB") %>% 
    dplyr::filter(currency == "EUR") %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      hierarchical_col = "location"
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>% 
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200606) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  valid_best_models <- function_input %>% 
    get_forecast_accuracy_overview(metric = "MASE") %>% 
    get_best_forecast_methods(filter_incomplete_fc = T)
  function_output <- get_bottom_up_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = valid_best_models
  )
  expect_is(function_output, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(function_output), c("fc_date", "period", "fc_value", "grouping", "best_model"))
  expect_equal(nrow(function_output), 108)
  expect_equal(unique(function_output$fc_date), 200606)
  expect_equal(min(function_output$period), 200607)
  expect_equal(max(function_output$period), 200706)
  expect_equal(median(function_output$period), 200656.5)
  expect_equal(length(unique(function_output$grouping)), 9)
  location_order <- function_output %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(location) %>% 
    dplyr::distinct() %>%
    tstools::add_grouping_column(group_cols = "location") %>% 
    dplyr::pull()
  hierarchy_matrix <- function_input$hierarchy[[1]]$matrix
  expect_equal(location_order, rownames(hierarchy_matrix))
  expect_equal(class(function_output$best_model), "character")
  expect_equal(unique(function_output$best_model), "fc_bottom_up")
  expect_equal(class(function_output$fc_value), "numeric")
  expect_equal(round(min(function_output$fc_value), 4), 0.5790)
  expect_equal(round(max(function_output$fc_value), 4), 14.1425)
  expect_equal(round(median(function_output$fc_value), 4), 2.9540)
  expect_equal(round(mean(function_output$fc_value), 4), 4.1218)
  # Make best_model_per_group invalid
  missing_best_models <- valid_best_models %>% 
    dplyr::slice(1:8)
  wrong_name_best_models <- valid_best_models %>% 
    dplyr::mutate(
      fc_model = case_when(
        fc_model == "fc_mean_l6m" ~ "bobby",
        TRUE ~ fc_model
      )
    )
  invalid_best_model_per_group <- list(
    c("prophet", "fc_rpart", "amazing_ML_model"), 
    missing_best_models, 
    wrong_name_best_models,
    42, NA, list()
  )
  for (model_per_group in invalid_best_model_per_group) {
    expect_error(
      get_bottom_up_fc_values(
        main_forecasting_table = function_input,
        best_model_per_group = model_per_group
      )
    )
  }
})

test_that("check get_bottom_up_fc_values with valid multivariate inputs", {
  function_input <- dummy_hierarchical_gasprice %>%
    dplyr::filter(currency == "USD") %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      xreg_cols = c("gemprice", "spotprice"),
      hierarchical_col = c("location", "oil_company")
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>% 
    create_main_forecasting_table() %>%
    dplyr::filter(ts_split_date == 200606) %>% 
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  valid_best_models <- function_input %>% 
    get_forecast_accuracy_overview(metric = "MAPE") %>% 
    get_best_forecast_methods(filter_incomplete_fc = T)
  function_output <- get_bottom_up_fc_values(
    main_forecasting_table = function_input,
    best_model_per_group = valid_best_models
  )
  expect_is(function_output, c("tbl_df", "tbl", "data.frame"))
  expect_equal(colnames(function_output), c("fc_date", "period", "fc_value", "grouping", "best_model"))
  expect_equal(nrow(function_output), 135)
  expect_equal(unique(function_output$fc_date), 200606)
  expect_equal(min(function_output$period), 200607)
  expect_equal(max(function_output$period), 200611)
  expect_equal(median(function_output$period), 200609)
  expect_equal(length(unique(function_output$grouping)), 27)
  location_order <- function_output %>% 
    tstools::split_grouping_column() %>% 
    dplyr::select(location, oil_company) %>% 
    dplyr::distinct() %>%
    tstools::add_grouping_column(group_cols = c("location", "oil_company")) %>% 
    dplyr::pull()
  hierarchy_matrix <- function_input$hierarchy[[1]]$matrix
  expect_equal(location_order, rownames(hierarchy_matrix))
  expect_equal(class(function_output$best_model), "character")
  expect_equal(unique(function_output$best_model), "fc_bottom_up")
  expect_equal(class(function_output$fc_value), "numeric")
  expect_equal(round(min(function_output$fc_value), 4), 2.6935)
  expect_equal(round(max(function_output$fc_value), 4), 31.6295)
  expect_equal(round(median(function_output$fc_value), 4), 6.0162)
  expect_equal(round(mean(function_output$fc_value), 4), 7.6343)
  # Make best_model_per_group invalid
  missing_best_models <- valid_best_models %>% 
    dplyr::slice(1:8)
  wrong_name_best_models <- valid_best_models %>% 
    dplyr::mutate(
      fc_model = case_when(
        fc_model == "fc_linear_trend_xreg" ~ "bobby",
        TRUE ~ fc_model
      )
    )
  invalid_best_model_per_group <- list(
    c("prophet", "fc_rpart", "amazing_ML_model"), 
    missing_best_models, 
    wrong_name_best_models,
    42, NA, list()
  )
  for (model_per_group in invalid_best_model_per_group) {
    expect_error(
      get_bottom_up_fc_values(
        main_forecasting_table = function_input,
        best_model_per_group = model_per_group
      )
    )
  }
})

test_that("check get_bottom_up_fc_values with invalid inputs", {
  full_table <- dummy_hierarchical_gasprice %>%
    dplyr::filter(oil_company == "CompanyB") %>% 
    tstools::initialize_ts_forecast_data(
      date_col = "year_month",
      col_of_interest = "gasprice",
      group_cols = c("currency", "oil_company"),
      hierarchical_cols = c("location")
    ) %>%
    dplyr::filter(period >= as.Date("2004-06-30")) %>% 
    create_main_forecasting_table() %>%
    add_fc_models_to_main_forecasting_table(
      fc_methods = c("basic", "linear")
    )
  many_split_dates <- full_table %>% 
    dplyr::filter(!grepl("oil_company = CompanyB", grouping))
  many_groups <- full_table %>% 
    dplyr::filter(ts_split_date == 200606)
  invalid_mft <- list(
    many_split_dates, many_groups, full_table, 
    c("I", "Wish", "I was a ", "mft"), 42
  )
  for (input in invalid_mft) {
    expect_error(
      get_bottom_up_fc_values(
        main_forecasting_table = input,
        best_model_per_group = valid_best_models
      )
    )
  }
})
