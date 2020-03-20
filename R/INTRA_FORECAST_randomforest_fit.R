#' Determine model fit of random forest
#'
#' \code{randomforest_fit} A function to gauge the fit of a model run of a
#' random forest, given parameters. This is a function that is used to fine-tune
#' the random forest when forecasting
#'
#' @param ML_data Dataset that has been prepared to run through randomForest. If
#'   originally a time series object, then it has gone through the
#'   \code{decompose_ts_object_for_ML} function and the first difference of the
#'   column of interest has been taken
#' @param mtry randomForest parameter. The number of variables that are randomly
#'   sampled as candidates at each split. Default values are different for
#'   classification (sqrt(p)) and regression (p/3) where p is number of
#'   variables
#' @param nodesize randomForest parameter. It is the minimum size of terminal
#'   nodes. Setting this numebr larger causes smaller trees to be grown and thus
#'   takes less time. Default values are different for classification (1) and
#'   regression (5)
#' @param ntree randomForest parameter. It is the number of trees to grow. It
#'   should not be too small in order to eliminate any possible over-fitting and
#'   have each observation used at least a couple of times (randomForest default
#'   = 500)
#'
#' @return The mean absolute prediction error (MAPE), in percentage terms, of
#'   the model run
#'
#' @importFrom magrittr '%>%'
#' @importFrom randomForest randomForest
#' @import dplyr
#' @importFrom tstools transform_data_to_ts_object
#'
#' @examples
#' ML_data <- tstools::initialize_ts_forecast_data(
#'    data = dummy_gasprice, 
#'       date_col = "year_month", 
#'       col_of_interest = "gasprice", 
#'       group_cols = c("state", "oil_company"), 
#'       xreg_cols = c("spotprice", "gemprice")
#'    ) %>% 
#'    dplyr::filter(grouping == "state = New York   &   oil_company = CompanyA") %>% 
#'    tstools::transform_data_to_ts_object() %>% 
#'    decompose_ts_object_for_ML() %>% 
#'    dplyr::mutate(col_of_interest = col_of_interest - dplyr::lag(col_of_interest)) %>% 
#'    dplyr::filter(!is.na(col_of_interest))
#' randomforest_fit(
#'    ML_data = ML_data, 
#'    mtry = 8, 
#'    nodesize = 5, 
#'    ntree = 1000
#' )
randomforest_fit <- function(ML_data, mtry, nodesize, ntree) {
  # Run randomForest regression
  randomforest_init <- randomForest::randomForest(
    formula = col_of_interest ~ .,
    data = ML_data,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize,
    importance = T
  )
  # Exclude col_of_interest for fitting data
  fit_data <- ML_data %>% 
    dplyr::select(-col_of_interest)
  # Get fitted values
  randomforest_fitted <- predict(randomforest_init, fit_data)
  # Calculate MAPE and return
  num <- mean(abs(ML_data$col_of_interest - randomforest_fitted))
  denom <- mean(abs(ML_data$col_of_interest))
  randomforest_mape <- ifelse(num == 0,
                              0,
                              100*(num/denom))
  # Return mape only
  return(randomforest_mape)
}