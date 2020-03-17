#' @name supported_fc_methods
#' @title Supported forecast models
#' 
#' @description 
#' \code{supported_fc_methods_uni_var} returns all available models for univariate forcasting.
#' \code{supported_fc_methods_multi_var} returns all available models for multivariate forcasting.
#'  
#' @rdname supported_fc_methods 
#' @return \item{supported_fc_methods_uni_var()}{c("basic", "linear", "holt_winters", "bats", "ets", "arima", "nn", "prophet", "tree", "forest", "ensemble", "svm", "recursive", "kalman", "fforma)}
#' @export
supported_fc_methods_uni_var <- function() {
  return(
    c(
      "basic", "linear", "holt_winters", "bats", 
      "ets", "arima", "nn", "prophet", "tree", 
      "forest", "ensemble", "svm", "recursive", "kalman",
      "fforma"
    )
  )
} 
#' @rdname supported_fc_methods
#' @return \item{supported_fc_methods_multi_var()}{c("linear", "arima", "nn", "prophet", "tree", "forest", "svm", "recursive")}
#' @export
supported_fc_methods_multi_var <- function() {
  return(
    c(
      "linear", "arima", "nn", "prophet", "tree", 
      "forest", "svm", "recursive"
    )
  )
}