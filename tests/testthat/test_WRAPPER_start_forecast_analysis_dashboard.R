
context("start_forecast_analysis_dashboard")

test_that("check start_forecast_analysis_dashboard using run_example with univariate data", {
  # Start shiny app in the background
  lines_to_run <- c(
    paste0("setwd('",getwd(),"')"),
    "devtools::load_all('.')",
    "options(shiny.port=9518)",
    "run_example()"
  )
  invisible(
    system(
      command = paste0('R -e "', paste0(lines_to_run, collapse = ";"), '"'),
      ignore.stdout = T,
      ignore.stderr = T,
      show.output.on.console = F,
      wait = F
    )
  )
  # Wait a few seconds and check if app is running
  shiny_url <- "http://127.0.0.1:9518/"
  start <- Sys.time()
  scraped <- FALSE
  # Get shiny_html
  while (difftime(Sys.time(), start, units = "sec") <= 60 & !scraped) {
    shiny_html <- try(
      expr = xml2::read_html(shiny_url),
      silent = T
    )
    if (any(class(shiny_html) != 'try-error')) {
      scraped <- TRUE
    } else {
      Sys.sleep(5)
    }
  }
  # Get html
  expect_is(shiny_html, "xml_document")
  # Temp function to parse html
  parse_html <- function(html, css) {
    shiny_html %>% 
      rvest::html_nodes(css) %>% 
      rvest::html_children() %>% 
      rvest::html_text() %>% 
      gsub("\n", "", .) %>% 
      gsub("\t", "", .) %>% 
      trimws() %>% 
      stringr::str_squish() %>% 
      magrittr::extract(. != "")
  }
  
  # Check header
  header <- parse_html(
    html = shiny_html,
    css = ".navbar-static-top"
  )
  expect_equal(header, "Toggle navigation")
  # Check sidebar
  sidebar <- parse_html(
    html = shiny_html,
    css = ".sidebar-menu"
  )
  expect_equal(sidebar, c(
    "Latest available data:", 
    "Hide sensitive data", 
    "Forecast Views", 
    "Forecast Deepdive", 
    "Adjust selection", 
    "Select mode: Compare models for one groupCompare groups for one model{} Select groups: All None Select model: {} Select group: Select models: {} Top 3 None 1 2 3 5 7 10 Download Forecast Data"
  ))
  # Check body
  body <- parse_html(
      html = shiny_html,
      css = ".content"
    ) %>% 
    gsub('[0-9]+', '', .)
  expect_equal(body, "Forecast date to use: Select Hierarchical Groups: {} Select Hierarchical Forecast: consistentbottom_up{} Aggregate performance by: Mean Absolute ErrorMean Absolute Percentage ErrorMean Absolute Scaled Error{} Select External Regressors: {} Select External Regressor: {} Select Delta of External Regressor: Forecasts VS Actuals Hierarchical Forecasts Forecast Performance External Regressors External Regressor Drivers Loading... Loading... Loading... Loading... Loading...")
})

test_that("check start_forecast_analysis_dashboard using run_example with hierarchical univariate data", {
  # Start shiny app in the background
  lines_to_run <- c(
    paste0("setwd('",getwd(),"')"),
    "devtools::load_all('.')",
    "options(shiny.port=9519)",
    "run_example(dataset='hierarchical')"
  )
  invisible(
    system(
      command = paste0('R -e "', paste0(lines_to_run, collapse = ";"), '"'),
      ignore.stdout = T,
      ignore.stderr = T,
      show.output.on.console = F,
      wait = F
    )
  )
  # Wait a few seconds and check if app is running
  shiny_url <- "http://127.0.0.1:9519/"
  start <- Sys.time()
  scraped <- FALSE
  # Get shiny_html
  while (difftime(Sys.time(), start, units = "sec") <= 60 & !scraped) {
    shiny_html <- try(
      expr = xml2::read_html(shiny_url),
      silent = T
    )
    if (any(class(shiny_html) != 'try-error')) {
      scraped <- TRUE
    } else {
      Sys.sleep(5)
    }
  }
  # Get html
  expect_is(shiny_html, "xml_document")
  # Temp function to parse html
  parse_html <- function(html, css) {
    shiny_html %>% 
      rvest::html_nodes(css) %>% 
      rvest::html_children() %>% 
      rvest::html_text() %>% 
      gsub("\n", "", .) %>% 
      gsub("\t", "", .) %>% 
      trimws() %>% 
      stringr::str_squish() %>% 
      magrittr::extract(. != "")
  }
  
  # Check header
  header <- parse_html(
    html = shiny_html,
    css = ".navbar-static-top"
  )
  expect_equal(header, "Toggle navigation")
  # Check sidebar
  sidebar <- parse_html(
    html = shiny_html,
    css = ".sidebar-menu"
  )
  expect_equal(sidebar, c(
    "Latest available data:", 
    "Hide sensitive data", 
    "Forecast Views", 
    "View Hierarchy", 
    "Forecast Deepdive", 
    "Adjust selection", 
    "Select mode: Compare models for one groupCompare groups for one model{} Select groups: All None Select model: {} Select group: Select models: {} Top 3 None 1 2 3 5 7 10 Download Forecast Data"
  ))
  # Check body
  body <- parse_html(
    html = shiny_html,
    css = ".content"
  ) %>% 
    gsub('[0-9]+', '', .)
  expect_equal(body, "Forecast date to use: Select Hierarchical Groups: {} Select Hierarchical Forecast: consistentbottom_up{} Aggregate performance by: Mean Absolute ErrorMean Absolute Percentage ErrorMean Absolute Scaled Error{} Select External Regressors: {} Select External Regressor: {} Select Delta of External Regressor: Forecasts VS Actuals Hierarchical Forecasts Forecast Performance External Regressors External Regressor Drivers Loading... Loading... Loading... Loading... Loading...")
})

test_that("check start_forecast_analysis_dashboard with invalid input", {
  expect_error(
    start_forecast_analysis_dashboard(
      main_forecasting_table = "potato"
    )
  )
  expect_error(
    start_forecast_analysis_dashboard(
      main_forecasting_table = univariate_example_data_1 %>% 
        dplyr::filter(FALSE)
    )
  )
  expect_error(
    start_forecast_analysis_dashboard(
      main_forecasting_table = dummy_gasprice
    )
  )
  expect_error(
    start_forecast_analysis_dashboard(
      main_forecasting_table = dummy_hierarchical_gasprice
    )
  )
  expect_error(
    start_forecast_analysis_dashboard(
      main_forecasting_table = univariate_example_data_1 %>% 
        dplyr::select(-fc_errors)
    )
  )
})
