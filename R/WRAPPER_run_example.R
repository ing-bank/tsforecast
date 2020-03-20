#' Run example of package
#'
#' \code{run_example} is a function to run an example of the application of the
#' tsforecast package on a public (time series) dataset. Calling the function
#' will start an interactive Shiny dashboard that enables exploration of the
#' individual forecasts versus actuals per split date, as well as an analysis of
#' the overall forecast accuracy of the different forecast models.
#'
#' @param dataset A character string indicating which of the available public
#'   (time series) datasets to show in the example.
#' @param test_mode Boolean, which is only set to TRUE when testing this
#'   function within the testthat framework for building the tsforecast
#'   package.
#'
#' @return An interactive Shiny dashboard.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @import shiny
#' @import shinydashboard
#' @importFrom plotly renderPlotly plotlyOutput
#'
#' @examples
#' run_example(dataset = "AirPassengers")
#' run_example(dataset = "nottem")
#' run_example(dataset = "UKDriverDeaths")
#' run_example(dataset = "hierarchical")
run_example <- function(dataset = c("AirPassengers", "nottem", "UKDriverDeaths", "hierarchical"), test_mode = F) {
  # Check arguments
  dataset <- match.arg(dataset)
  # Load required dataset
  if (dataset == "AirPassengers") main_forecasting_table <- tsforecast::univariate_example_data_1
  if (dataset == "nottem") main_forecasting_table <- tsforecast::univariate_example_data_2
  if (dataset == "UKDriverDeaths") main_forecasting_table <- tsforecast::univariate_example_data_3
  if (dataset == "hierarchical") main_forecasting_table <- tsforecast::hierarchical_univariate_example_data_1
  # Load example data and start dashboard
  start_forecast_analysis_dashboard(
    main_forecasting_table = main_forecasting_table,
    test_mode = test_mode
  )
}