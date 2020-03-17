# This file contains helper functions for recursive forecasting with any
# model for time series. It also contains functions to do feature extraction
# and transform the time series.
# 
# The functions can be used to recursively forecast with a machine learning method.
# The main function, forecast_recursively, transforms the data, selects the last input,
# predicts this input, adds the prediction to the data and loops this for a certain
# amount of steps.
# 
# Example:
# library(tsforecast)
# library(tidyverse)
# library(default)
# library(randomForest)
# 
# n <- 360
# h <- 40
# target_col <- "col_of_interest"
# data <- example_series(n)
# test <- data[(n - h):n + 1,]
# train <- mask_future_values(data, test$period)
# transform <- transform
# default(transform) <- list(target_col = target_col,
#                            transformation = pct_diff,
#                            lags = seq(1,36,4))
# train_data <- transform(train)
# model <- randomForest::randomForest(col_of_interest ~ ., train_data)
# pct_forecasts <- get_forecasts(train, model, transform, inv_pct_diff, nrow(test))


# Forecasts recursively given a model and a certain number of steps ahead.
forecast_recursively <- function(model, last, data, transform, 
                                 inverse_transform_target, add_forecast_to_data, steps = 1) {
  fcs <- c()
  for (h in 1:steps) {
    # Gets the input to be predicted
    input <- data %>% 
      transform() %>% 
      dplyr::select(-col_of_interest) %>%
      tail(1)
    # The forecast of the model
    fc <- predict(model, input)
    # The forecast inversely transformed to original scale
    last <- inverse_transform_target(fc[[1]], last[[1]])
    # Adds the forecast to the data
    data <- add_forecast_to_data(data, last)
    # Adds the forecast to all forecast
    fcs[h] <- last
  }
  return(fcs)
}

# Extracts features from the period of the data
feature_extract_ts <- function(data) {
  # Get time index features
  date_features <- data$period %>% 
    timetk::tk_get_timeseries_signature() #%>% 
    #dplyr::select(month, quarter) # Instead, lets use all time features!
  data %>% 
    dplyr::bind_cols(date_features) %>%
    #dplyr::select(-period) %>% 
    return()
}

# Lags certain cols for a certain sequence of lags
feature_extract_lag <- function(data, cols, lags) {
  # Lag columns
  for (col in cols) {
    # Create a quosure for each lag
    create_lag_quosures <- setNames(
      object = purrr::map(
        .x = lags,
        .f = ~dplyr::quo(dplyr::lag(!! dplyr::sym(col), n = !! .x))
      ),
      nm = paste0(col, "_lag_", lags)
    )
    # Add each lag
    data <- data %>% 
      dplyr::mutate(!!! create_lag_quosures)
  }
  return(data)
}

# Lags certain cols for a certain sequence of drifts
feature_extract_drift <- function(data, cols, drifts) {
  # Drift columns
  for (col in cols) {
    # Create a quosure for each temp lag
    create_temp_lag_quosures <- setNames(
      object = purrr::map(
        .x = drifts,
        .f = ~dplyr::quo(dplyr::lag(!! dplyr::sym(col), n = !! .x))
      ),
      nm = paste0(col, "_temp_lag_", drifts)
    )
    # Create a quosure for each drift
    create_drift_quosures <- setNames(
      object = purrr::map(
        .x = drifts,
        .f = ~dplyr::quo(!! dplyr::sym(col) - !! dplyr::sym(paste0(col, "_temp_lag_", .x)))
      ),
      nm = paste0(col, "_drift_", drifts)
    )
    # Add each drift
    data <- data %>% 
      dplyr::mutate(!!! create_temp_lag_quosures) %>% 
      dplyr::mutate(!!! create_drift_quosures) %>% 
      dplyr::select(-dplyr::contains("_temp_lag_"))
  }
  return(data)
}

# An example implementation of how to transform the data for the time series
# regression.

transform <- function(data, target_col, transformation, lags = 1, drifts = 1, xreg_cols = NULL){
  data %>%
    # Transform target
    transform_cols(target_col, transformation) %>% 
    # Transform external regressors
    transform_cols(xreg_cols, transformation) %>% 
    # Create lags
    feature_extract_lag(target_col, lags) %>% 
    feature_extract_lag(xreg_cols, lags) %>% 
    # Create drifts
    feature_extract_drift(target_col, drifts) %>% 
    feature_extract_drift(xreg_cols, drifts) %>% 
    # Get time series features
    feature_extract_ts() %>% 
    # Cleanup
    tidyr::drop_na() %>% 
    # Remove stationary columns
    filter_stationary_columns() %>% 
    # Return results
    return()
}

# Adds the forecast to the data
add_forecast_to_data <- function(data, last){
  # Get next date
  next_date <- data %>% 
    tidyr::drop_na(col_of_interest) %>% 
    tail(1) %>% 
    pull(period) %>% 
    tstools::date_to_period() %>% 
    tstools::period_delta(delta = 1) %>% 
    tstools::period_to_last_day()
  # Overwrite next date with the forecast
  r <- which(data$period == next_date)
  data[r,"col_of_interest"] <- last 
  # Return the data
  return(data)
}

# Percentage difference
pct_diff <- function(x){
  return(x/dplyr::lag(x) - 1)
}

inv_pct_diff <- function(pct_fc, last){
  return(last * (1 + pct_fc))
}

# Difference
diff <- function(x){
  return(x - dplyr::lag(x))
}

inv_diff <- function(diff_fc, last){
  return(last + diff_fc)
}

# Logarithmic returns
log_returns <- function(x){
  return(log(x/dplyr::lag(x)))
}

inv_log_returns <- function(log_ret, last){
  return(exp(log_ret)*last)
}


# Transforms a list of columns with a given transformation, e.g. percentage difference
transform_cols <- function(data, columns, transformation){
  for (col in columns) {
    data[[col]] <- transformation(data[[col]])
  }
  return(data)
}

# Masks future values of the dataframe for given periods.
mask_future_values <- function(data, periods){
  data %>%
    dplyr::mutate(
      col_of_interest = dplyr::if_else(
        period %in% periods,
        NaN,
        col_of_interest
      )
    ) %>% 
    return()
}

# Returns the recursive forecasts
get_forecasts <- function(data, model, transform, inverse_transform_target, steps){
  last <- data %>% 
    tidyr::drop_na() %>% 
    dplyr::select(col_of_interest) %>% 
    tail(1)
  forecasts <- forecast_recursively(
    model, 
    last, 
    data, 
    transform, 
    inverse_transform_target,
    add_forecast_to_data, 
    steps
  )
  data %>% 
    dplyr::filter(is.na(col_of_interest)) %>% 
    dplyr::select(period) %>% 
    head(steps) %>% 
    dplyr::mutate(col_of_interest = forecasts) %>% 
    return()
}

# Creates an example time series based on a yearly and 3 years seasonality.
example_series <- function(n){
  step <- 1
  x <- seq(0, n, step)
  
  base <- 100
  yearly_trend <- 5*sin(x*(pi/12))
  business_trend <- 7*sin(x*(pi/36))
  noise <- rnorm(length(x))
  trend <- x/10
  
  target_col <- "col_of_interest"
  data <- tibble::tibble(
      col_of_interest = base + trend + business_trend + yearly_trend + noise,
      period = seq(as.Date("2000/1/1"), by = "month", length.out = length(x))
    ) %>% 
    tstools::initialize_ts_forecast_data(
      col_of_interest = target_col ,
      date_col = "period"
    ) %>% 
    dplyr::select(-grouping)
  
  return(data)
}
