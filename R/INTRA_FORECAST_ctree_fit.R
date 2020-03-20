#' Determine model fit of CTREE tree
#'
#' \code{ctree_fit} A function to gauge the fit of a model run of an CTREE tree,
#' given parameters. This is a function that is used to fine-tune the CTREE tree
#' when forecasting
#'
#' @param ML_data Dataset that has been prepared to run through CTREE. If
#'   originally a time series object, then it has gone through the
#'   \code{decompose_ts_object_for_ML} function and the first difference of the
#'   column of interest has been taken
#' @param minsplit CTREE parameter. The minimum number of observations that must
#'   exist in a node in order for a split to be attempted (default from CTREE =
#'   20)
#' @param mincriterion CTREE parameter. The value of the test-statistic
#'   (\code{testtype} == "Teststatistic") or 1 - p-value that must be exceeded
#'   in order to implement a split (default from CTREE = 0.95)
#' @param minbucket CTREE parameter. Minimum sum of weights in a terminal node
#'   (default from CTREE = 7)
#' @param testtype CTREE parameter. Which distribution to use. Options are
#'   "Bonferroni", "MonteCarlo", "Univariate" and "Teststatistic"
#' @param teststat CTREE parameter. Specifies which test statistic to use when
#'   doing hypothesis testing. Options are "quad" and "max"
#' @param nresample CTREE parameter. Amount of resampling to do when MonteCarlo
#'   is selected as test type (default from CTREE = 9999)
#'
#' @return The mean absolute prediction error (MAPE), in percentage terms, of
#'   the model run
#'
#' @importFrom magrittr '%>%'
#' @import party
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
#'    decompose_ts_object_for_ML(filter_date_features = T) %>% 
#'    dplyr::mutate(col_of_interest = col_of_interest - dplyr::lag(col_of_interest)) %>% 
#'    dplyr::filter(!is.na(col_of_interest))
#' ctree_fit(
#'    ML_data = ML_data, 
#'    minsplit = 20, 
#'    mincriterion = 0.975, 
#'    minbucket = 5, 
#'    testtype = "Univariate", 
#'    teststat = "quad", 
#'    nresample = 9999
#' )
ctree_fit <- function(ML_data, minsplit, mincriterion, minbucket, testtype = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"), teststat = c("quad", "max"), nresample = 9999) {
  # Check input
  testtype <- match.arg(testtype)
  teststat <- match.arg(teststat)
  # Run CTREE tree regression
  ctree_init <- party::ctree(
    formula = col_of_interest ~ .,
    data = ML_data,
    controls = party::ctree_control(
      minsplit = minsplit,
      mincriterion = mincriterion, 
      minbucket = minbucket,
      testtype = testtype,
      teststat = teststat,
      nresample = nresample
    )
  )
  # Exclude col_of_interest for fitting data
  fit_data <- ML_data %>% 
    dplyr::select(-col_of_interest)
  # Get fitted values
  ctree_fitted <- predict(ctree_init, fit_data)
  # Calculate MAPE and return
  num <- mean(abs(ML_data$col_of_interest - ctree_fitted))
  denom <- mean(abs(ML_data$col_of_interest))
  ctree_mape <- ifelse(num == 0,
                       0,
                       100*(num/denom))
  # Return mape only
  return(ctree_mape)
}