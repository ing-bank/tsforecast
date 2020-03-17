#' Show package documentation
#'
#' \code{show_documentation} is a function to open the documentation (available
#' as html file) in a browser.
#'
#' @param topic A character string indicating which of the available
#'   documentation files to display.
#' @param test_mode Boolean, which is only set to TRUE when testing this
#'   function within the testthat framework for building the tsforecast
#'   package.
#'
#' @return An html document with documentation.
#' @export
#'
#' @examples
#' show_documentation()
show_documentation <- function(topic = c("README", "forecast_models"), test_mode = F) {
  # Check arguments
  topic <- match.arg(topic)
  # Define file path to documentation
  doc_dir <- system.file("documentation", package = "tsforecast")
  if (topic == "README") file_path <- file.path(doc_dir, "README.html")
  if (topic == "forecast_models") file_path <- file.path(doc_dir, "forecast_models.html")
  # Only return file_path when in test mode
  if (test_mode) {
    return(file_path)
  } else {
    # Load required documentation
    browseURL(paste0('file://', file_path))
  }
}