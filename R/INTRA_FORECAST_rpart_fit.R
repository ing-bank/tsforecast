#' Determine model fit of RPART tree
#'
#' \code{rpart_fit} A function to gauge the fit of a model run of an RPART tree,
#' given parameters. This is a function that is used to fine-tune the RPART tree
#' when forecasting
#'
#' @param ML_data Dataset that has been prepared to run through RPART. If
#'   originally a time series object, then it has gone through the
#'   \code{decompose_ts_object_for_ML} function and the first difference of the
#'   column of interest has been taken
#' @param minsplit RPART parameter. The minimum number of observations that must
#'   exist in a node in order for a split to be attempted (default from RPART =
#'   20)
#' @param maxdepth RPART parameter. The maximum depth of any node in the tree
#'   (default from RPART = 30)
#' @param cp RPART parameter. Determines the minimum amount of increase in
#'   R-squared that is needed for a node to split (default from RPART = 0.1)
#' @param xval RPART parameter. Number of cross validations run. This is
#'   important as it reduces the tendency to over-fit (default from RPART = 10)
#'
#' @return The mean absolute prediction error (MAPE), in percentage terms, of
#'   the model run
#'
#' @importFrom magrittr '%>%'
#' @import rpart
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
#' rpart_fit(
#'    ML_data = ML_data, 
#'    minsplit = 20, 
#'    maxdepth = 30, 
#'    cp = 0.01,
#'    xval = 10
#' )
rpart_fit <- function(ML_data, minsplit, maxdepth, cp, xval) {
  # Run RPART tree regression
  rpart_init <- rpart::rpart(
    formula = col_of_interest ~.,
    data = ML_data, 
    control = rpart::rpart.control(
      minsplit = minsplit,
      maxdepth = maxdepth,
      cp = cp,
      xval = xval
    )
  )
  # Exclude col_of_interest for fitting data
  fit_data <- ML_data %>% 
    dplyr::select(-col_of_interest)
  # Get fitted values
  rpart_fitted <- predict(rpart_init, fit_data)
  # Calculate MAPE and return
  num <- mean(abs(ML_data$col_of_interest - rpart_fitted))
  denom <- mean(abs(ML_data$col_of_interest))
  rpart_mape <- ifelse(num == 0,
                       0,
                       100*(num/denom))
  # Return mape only
  return(rpart_mape)
}