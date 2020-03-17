
# TO REFRESH THE UNIVARIATE_EXAMPLE_DATA
# 1: Open tsforecast Project file
# 2: Source this script

devtools::load_all(".")
library(tidyverse)

create_univariate_example_data <- function() {
  # Set seed to ensure reproducability
  set.seed(42)
  #### Prepare AirPassengers dataset ####
  # The classic Box & Jenkins airline data. Monthly totals of international airline passengers, 1949 to 1960
  dataset_1_1 <- datasets::AirPassengers %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      passengers = x,
      dataset = "AirPassengers",
      type = "original"
    )
  dataset_1_2 <- datasets::AirPassengers %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      passengers = rev(x),
      dataset = "AirPassengers",
      type = "reverse"
    )
  dataset_1_3 <- datasets::AirPassengers %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      passengers = x + (runif(length(x), -0.5, 0.5) * x),
      dataset = "AirPassengers",
      type = "noisy"
    )
  # Combine datasets 
  dataset_1 <- dplyr::bind_rows(
    dataset_1_1,
    dataset_1_2,
    dataset_1_3
  )
  # Use dataset to initialize data
  data_1 <- tstools::initialize_ts_forecast_data(
    data = dataset_1,
    date_col = "period",
    col_of_interest = "passengers",
    group_cols = c("dataset", "type")
  )
  # Update main_forecasting_table
  univariate_example_data_1 <- update_main_forecasting_table(
    file_path = file.path(dirname(getwd()), "univariate_example_data_1.rda"),
    data = data_1,
    seasonal_periods = c(3, 12),
    min_train_periods = 5 * 12, # 5 years
    periods_ahead = 3 * 12, # 3 years
    #fc_methods = c("basic", "linear"),
    verbose = T
  )
  # Remove the fc_models object to reduce the size of the object in the package (by about 60%)
  univariate_example_data_1 <- univariate_example_data_1 %>% 
    dplyr::select(-fc_models)
  # Store the data in the /data folder of the package
  usethis::use_data(univariate_example_data_1, overwrite = T)
  
  #### Prepare nottem dataset ####
  # A time series object containing average air temperatures at Nottingham Castle in degrees Fahrenheit for 20 years
  dataset_2_1 <- datasets::nottem %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      temperature = x,
      dataset = "nottem",
      type = "original"
    )
  dataset_2_2 <- datasets::nottem %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      temperature = rev(x),
      dataset = "nottem",
      type = "reverse"
    )
  dataset_2_3 <- datasets::nottem %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      temperature = x + (runif(length(x), -0.5, 0.5) * x),
      dataset = "nottem",
      type = "noisy"
    )
  # Combine datasets
  dataset_2 <- dplyr::bind_rows(
    dataset_2_1,
    dataset_2_2,
    dataset_2_3
  )
  # Use dataset to initialize data
  data_2 <- tstools::initialize_ts_forecast_data(
    data = dataset_2,
    date_col = "period",
    col_of_interest = "temperature",
    group_cols = c("dataset", "type")
  )
  # Update main_forecasting_table
  univariate_example_data_2 <- update_main_forecasting_table(
    file_path = file.path(dirname(getwd()), "univariate_example_data_2.rda"),
    data = data_2,
    seasonal_periods = c(3, 12),
    min_train_periods = 5 * 12, # 5 years
    periods_ahead = 3 * 12, # 3 years
    #fc_methods = c("basic", "linear"),
    verbose = T
  )
  # Remove the fc_models object to reduce the size of the object in the package (by about 60%)
  univariate_example_data_2 <- univariate_example_data_2 %>% 
    dplyr::select(-fc_models)
  # Store the data in the /data folder of the package
  usethis::use_data(univariate_example_data_2, overwrite = T)
  
  #### Prepare UKDriverDeaths dataset ####
  # UKDriverDeaths is a time series giving the monthly totals of car drivers in Great Britain killed or seriously injured Jan 1969 to Dec 1984
  dataset_3_1 <- datasets::UKDriverDeaths %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      deaths = x,
      dataset = "UKDriverDeaths",
      type = "original"
    )
  dataset_3_2 <- datasets::UKDriverDeaths %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      deaths = rev(x),
      dataset = "UKDriverDeaths",
      type = "reverse"
    )
  dataset_3_3 <- datasets::UKDriverDeaths %>% 
    ts_object_to_tibble() %>% 
    dplyr::transmute(
      period = period,
      deaths = x + (runif(length(x), -0.5, 0.5) * x),
      dataset = "UKDriverDeaths",
      type = "noisy"
    )
  # Combine datasets
  dataset_3 <- dplyr::bind_rows(
    dataset_3_1,
    dataset_3_2,
    dataset_3_3
  )
  # Use dataset to initialize data
  data_3 <- tstools::initialize_ts_forecast_data(
    data = dataset_3,
    date_col = "period",
    col_of_interest = "deaths",
    group_cols = c("dataset", "type")
  )
  # Update main_forecasting_table
  univariate_example_data_3 <- update_main_forecasting_table(
    file_path = file.path(dirname(getwd()), "univariate_example_data_3.rda"),
    data = data_3,
    seasonal_periods = c(3, 12),
    min_train_periods = 5 * 12, # 5 years
    periods_ahead = 3 * 12, # 3 years
    #fc_methods = c("basic", "linear"),
    verbose = T
  )
  # Remove the fc_models object to reduce the size of the object in the package (by about 60%)
  univariate_example_data_3 <- univariate_example_data_3 %>% 
    dplyr::select(-fc_models)
  # Store the data in the /data folder of the package
  usethis::use_data(univariate_example_data_3, overwrite = T)
  
  #### Store datasets for FFORMA training ####
  if (FALSE) {
    # Store the AirPassengers dataset
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = univariate_example_data_1,
      name = "AirPassengers",
      overwrite = T
    )
    # Store the nottem dataset
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = univariate_example_data_2,
      name = "nottem",
      overwrite = T
    )
    # Store the UKDriverDeaths dataset
    store_main_forecasting_table_for_fforma(
      main_forecasting_table = univariate_example_data_2,
      name = "UKDriverDeaths",
      overwrite = T
  )
  }
}

create_hierarchical_univariate_example_data <- function() {
  # Set seed to ensure reproducability
  set.seed(42)
  #### Use dummy hierarchical gasprice dataset ####
  dataset_1 <- dummy_hierarchical_gasprice
  # Use dataset to initialize data
  data_1 <- tstools::initialize_ts_forecast_data(
    data = dataset_1,
    date_col = "year_month",
    col_of_interest = "gasprice",
    group_cols = "currency",
    hierarchical_cols = c("location", "oil_company")
  )
  # Update main_forecasting_table
  hierarchical_univariate_example_data_1 <- update_main_forecasting_table(
    file_path = file.path(dirname(getwd()), "hierarchical_univariate_example_data_1.rda"),
    data = data_1,
    seasonal_periods = c(3, 12),
    min_train_periods = 14 * 12, # 14 years
    periods_ahead = 3 * 12, # 3 years
    add_hierarchical_fc = T,
    verbose = T
  )
  # Remove the fc_models object to reduce the size of the object in the package (by about 60%)
  hierarchical_univariate_example_data_1 <- hierarchical_univariate_example_data_1 %>% 
    dplyr::select(-fc_models)
  # Store the data in the /data folder of the package
  usethis::use_data(hierarchical_univariate_example_data_1, overwrite = T)
}

create_univariate_example_data()

create_hierarchical_univariate_example_data()
