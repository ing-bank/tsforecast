#' Start a local forecast analysis dashboard
#'
#' \code{start_forecast_analysis_dashboard} is a wrapper function around several
#' visualization functions to start a local Shiny dashboard to analyse the
#' forecast data available in the main_forecasting_table.
#'
#' @param main_forecasting_table A tibble containing several columns of data
#'   required for time series forecasting, which has been created using the
#'   \code{create_main_forecasting_table} function and which has been extended
#'   with the fc_models and fc_errors columns using the
#'   \code{add_fc_models_to_main_forecasting_table} function.
#' @param test_mode Boolean, which is only set to TRUE when testing this
#'   function within the testthat framework for building the tsforecast
#'   package.
#'
#' @return Starts a local Shiny dashboard to analyse the forecasts.
#' @export
#'
#' @importFrom magrittr '%>%'
#' @importFrom purrr map_lgl
#' @import dplyr
#' @importFrom shinyjs useShinyjs enable disable
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom visNetwork visNetworkProxy visOptions visInteraction
#'   visSelectNodes visNetworkOutput visEvents
#' @importFrom shinycssloaders withSpinner
#' @importFrom openxlsx write.xlsx
#' @importFrom tstools check_data_format split_grouping_column
#'   unlist_if_required get_browser_to_use_for_shiny
#'
#' @examples
#' tstools::initialize_ts_forecast_data(
#'       data = dummy_gasprice,
#'       date_col = "year_month",
#'       col_of_interest = "gasprice",
#'       group_cols = c("state", "oil_company")
#'    ) %>%
#'    create_main_forecasting_table(
#'       min_train_periods = 180
#'    ) %>%
#'    add_fc_models_to_main_forecasting_table(
#'       periods_ahead = 12,
#'       fc_methods = c("basic", "linear")
#'    ) %>%
#'    start_forecast_analysis_dashboard()
start_forecast_analysis_dashboard <- function(main_forecasting_table, test_mode = F) {
  # Check main_forecasting_table
  tstools::check_data_format(
    data = main_forecasting_table,
    func_name = "start_forecast_analysis_dashboard",
    req_cols = c(
      "grouping", "ts_start", "ts_split_date", "ts_end", "train_length", "valid_length", 
      "ts_object_train", "ts_object_valid", "fc_errors"
    )
  )
  # Determine location of shiny dashboard
  appDir <- system.file("forecast_analysis_dashboard", package = "tsforecast")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `tsforecast`.")
  }
  
  # If applicable, get hierarchy information
  is_hierarchical <- "hierarchy" %in% colnames(main_forecasting_table)
  if (is_hierarchical) {
    hierarchy <- main_forecasting_table$hierarchy[[1]]
    hierarchical_cols <- comment(hierarchy$matrix)
  } else {
    hierarchy <- NA
    hierarchical_cols <- ""
  }
  
  # Determine available groupings
  groupings <- main_forecasting_table %>% 
    dplyr::select(grouping) %>% 
    dplyr::distinct() %>% 
    tstools::split_grouping_column()
  # Determine available fc_models
  fc_models <- main_forecasting_table$fc_errors %>% 
    dplyr::bind_rows() %>% 
    dplyr::pull(fc_model) %>% 
    unique()
  # Extract time series object
  ts_object <- main_forecasting_table$ts_object_train[1] %>% 
    tstools::unlist_if_required()
  # Determine if univariate or multivariate
  is_multivariate <-  length(attr(ts_object, "xreg_cols")) >= 1
  # Determine if original actuals are available
  has_original_actuals <- "original_col_of_interest" %in% colnames(ts_object)
  
  # Define app data
  app_data <<- list(
    # Data
    main_forecasting_table = main_forecasting_table,
    # Variables
    groupings = groupings,
    fc_models = fc_models,
    # Hierarchy information
    is_hierarchical = is_hierarchical,
    hierarchy = hierarchy,
    hierarchical_cols = hierarchical_cols,
    # Time series information
    is_multivariate = is_multivariate,
    has_original_actuals = has_original_actuals,
    # Other information
    latest_split_date = max(main_forecasting_table$ts_split_date),
    # Documentation
    documentation = list()
  )
  
  # Only return appDir when in test mode
  if (test_mode) {
    return(appDir)
  } else {
    # Determine best browser to use
    browser_to_use <- tstools::get_browser_to_use_for_shiny()
    # Function to launch the best browser to use
    launch.browser = function(appUrl, browser.path = browser_to_use) {
      if (!is.null(browser.path)) message('Browser path: ', browser.path)
      browseURL(url = appUrl, browser = browser.path, encodeIfNeeded = TRUE)
    }
    # Run the funds entrusted app
    shiny::runApp(
      appDir = appDir, 
      launch.browser = launch.browser
    )
  }
}