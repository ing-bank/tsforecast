
fluidPage(
  shinydashboard::box(
    width = "100%",
    # If tab is 'forecasts_vs_actuals'
    conditionalPanel(
      condition = "input.MAIN_menu == 'FD' & input.forecast_deepdive_panels == 'forecasts_vs_actuals'",
      column(
        width = 10,
        h5("Forecast date to use:", align = "center"),
        shinyWidgets::sliderTextInput(
          inputId = "FC_vs_ACT_ts_split_date", 
          label = NULL,
          choices = format.Date(as.Date("1000-01-01"), "%Y - %B"), 
          width = "100%"
        ),
        offset = 1
      )
    ),
    # If tab is 'hierarchical_forecast'
    conditionalPanel(
      condition = "input.MAIN_menu == 'FD' & input.forecast_deepdive_panels == 'hierarchical_forecast'",
      column(
        width = 4,
        h5("Select Hierarchical Groups:", align = "center"),
        selectInput(
          inputId = "HY_FC_plot_hierarchical_group",
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        ),
        offset = 1
      ),
      column(
        width = 4,
        h5("Select Hierarchical Forecast:", align = "center"),
        selectInput(
          inputId = "HY_FC_plot_hierarchical_fc_model", 
          label = NULL,
          choices = c("consistent", "bottom_up"),
          width = "100%"
        ),
        offset = 1
      )
    ),
    # If tab is 'forecast_performance'
    conditionalPanel(
      condition = "input.MAIN_menu == 'FD' & input.forecast_deepdive_panels == 'forecast_performance'",
      column(
        width = 4,
        h5("Aggregate performance by:", align = "center"),
        selectInput(
          inputId = "FC_PERF_metric", 
          label = NULL, 
          choices = list(
            "Mean Absolute Error" = "MAE", 
            "Mean Absolute Percentage Error" = "MAPE",
            "Mean Absolute Scaled Error" = "MASE"
          ),
          selected = 'MASE', 
          width = "100%"
        ),
        offset = 4
      )
    ),
    # If tab is 'external_regressors'
    conditionalPanel(
      condition = "input.MAIN_menu == 'FD' & input.forecast_deepdive_panels == 'external_regressors'",
      column(
        width = 10,
        h5("Select External Regressors:", align = "center"),
        selectInput(
          inputId = "FC_vs_ACT_select_xreg",
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        ),
        offset = 1
      )
    ),
    # If tab is 'external_regressor_drivers'
    conditionalPanel(
      condition = "input.MAIN_menu == 'FD' & input.forecast_deepdive_panels == 'external_regressor_drivers'",
      column(
        width = 4,
        h5("Select External Regressor:", align = "center"),
        selectInput(
          inputId = "FC_vs_ACT_select_driver_xreg",
          label = NULL,
          choices = NULL,
          multiple = FALSE,
          width = "100%"
        ),
        offset = 1
      ),
      column(
        width = 5,
        h5("Select Delta of External Regressor:", align = "center"),
        shinyWidgets::sliderTextInput(
          inputId = "FC_vs_ACT_select_driver_xreg_delta", 
          label = NULL,
          choices = seq(from = 0, to = 12),
          width = "100%"
        ),
        offset = 1
      )
    )
  ),
  shinydashboard::box(
    width = "100%",
    # Put in the tabs here
    shiny::mainPanel(
      width = "100%",
      shiny::tabsetPanel(
        id = "forecast_deepdive_panels",
        type = "tabs",
        shiny::tabPanel(
          title = "Forecasts VS Actuals",
          value = "forecasts_vs_actuals",
          # Source forecasts vs actuals tab
          source('FD_forecasts_vs_actuals_tab_ui.R', local = T)[1]
        ),
        shiny::tabPanel(
          title = "Hierarchical Forecasts",
          value = "hierarchical_forecast",
          # Source hierarchical forecasts tab
          source('FD_hierarchical_forecasts_tab_ui.R', local = T)[1]
        ),
        shiny::tabPanel(
          title = "Forecast Performance",
          value = "forecast_performance",
          # Source forecast performance tab
          source('FD_forecast_performance_tab_ui.R', local = T)[1]
        ),
        shiny::tabPanel(
          title = "External Regressors",
          value = "external_regressors",
          # Source external regressors tab
          source('FD_external_regressors_tab_ui.R', local = T)[1]
        ),
        shiny::tabPanel(
          title = "External Regressor Drivers",
          value = "external_regressor_drivers",
          # Source external regressor drivers tab
          source('FD_external_regressor_drivers_tab_ui.R', local = T)[1]
        )
      )
    )
  )
)
