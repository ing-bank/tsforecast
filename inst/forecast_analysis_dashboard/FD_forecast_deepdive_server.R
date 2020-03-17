
#### DATA UPDATES ####

# Update fc_actuals data
FD_update_main_forecasting_table <- reactive({
  # Only start when inputs are available
  req(input$FC_vs_ACT_ts_split_date)
  # Get main_forecasting_table
  main_forecasting_table <- MAIN_update_main_forecasting_table()
  # Update global variables
  main_forecasting_table %>% 
    dplyr::mutate(
      ts_split_date_filter = ts_split_date %>% 
        tstools::period_to_last_day() %>%
        format.Date("%Y - %B")
    ) %>% 
    dplyr::filter(
      ts_split_date_filter == input$FC_vs_ACT_ts_split_date
    ) %>% 
    dplyr::select(-ts_split_date_filter) %>% 
    return()
})

# Update accuracy_overview
FD_update_accuracy_overview <- reactive({
  # Update main_forecasting_table
  main_forecasting_table <- MAIN_update_main_forecasting_table()
  # Requirements
  req(nrow(main_forecasting_table) > 0)
  req(input$FC_PERF_metric)
  # Calculate forecasting_accuracy
  get_forecast_accuracy_overview(
    main_forecasting_table = main_forecasting_table,
    metric = input$FC_PERF_metric
  )
})


#### SELECTIZE UPDATES ####

# Update available ts_split_dates
observe(
  x = {
    # Update main_forecasting_table
    main_forecasting_table <- MAIN_update_main_forecasting_table()
    # Requirements
    req(nrow(main_forecasting_table) > 0)
    # Get available ts_split_dates
    ts_split_dates <- main_forecasting_table %>% 
      dplyr::pull(ts_split_date) %>% 
      unique() %>% 
      tstools::period_to_last_day()
    # Create vector of choices
    ts_split_dates <- seq(
        from = min(ts_split_dates) + 1, 
        to = max(ts_split_dates) + 1, 
        by = "month"
      ) - lubridate::days(1)
    ts_split_dates <- format.Date(ts_split_dates, "%Y - %B")
    # Update choices for FC_vs_ACT_ts_split_date
    shinyWidgets::updateSliderTextInput(
      session = session,
      inputId = "FC_vs_ACT_ts_split_date",
      choices = ts_split_dates
    )
  }
)

# Update available delta range for plotting xreg drivers
observe(
  x = {
    # Update main_forecasting_table
    main_forecasting_table <- MAIN_update_main_forecasting_table()
    # Requirements
    req(nrow(main_forecasting_table) > 0)
    # Get available maximum seasonality
    max_seasonality <- max(attr(main_forecasting_table$ts_object_train[[1]], "seasonality"))
    # Get available xreg names
    xreg_names <- attr(main_forecasting_table$ts_object_train[[1]],"xreg_cols")
    # Make sure it is a multivariate analysis
    req(length(xreg_names) > 0)
    # Update choices for FC_vs_ACT_select_driver_xreg_delta
    shinyWidgets::updateSliderTextInput(
      session = session,
      inputId = "FC_vs_ACT_select_driver_xreg_delta",
      choices = seq(from = 0, to = max_seasonality)
    )
    # Update choices for FC_vs_ACT_select_driver_xreg
    updateSelectInput(
      session = session,
      inputId = "FC_vs_ACT_select_driver_xreg",
      choices = xreg_names,
      selected = input$FC_vs_ACT_select_driver_xreg
    )
    # Update choices for FC_vs_ACT_select_xreg
    updateSelectInput(
      session = session,
      inputId = "FC_vs_ACT_select_xreg",
      choices = xreg_names,
      selected = input$FC_vs_ACT_select_xreg
    )
  }
)

# Update available hierarchical columns for plotting hierarchical forecast
observe({
  x = {
    # Update main forecasting table
    main_forecasting_table <- FD_update_main_forecasting_table()
    # Requirements
    req(nrow(main_forecasting_table) > 0)
    req(app_data$is_hierarchical == TRUE)
    # Update choices for HY_FC_plot_hierarchical_group
    updateSelectInput(
      session = session,
      inputId = "HY_FC_plot_hierarchical_group",
      choices = app_data$hierarchical_cols,
      selected = input$HY_FC_plot_hierarchical_group
    ) 
  }
})

#### NOTIFICATIONS ####

# Show notifications if there is no data available
observe(
  x = {
    # Update main_forecasting_table
    main_forecasting_table <- FD_update_main_forecasting_table()
    # Requirement
    req(input$FC_vs_ACT_ts_split_date != format.Date(as.Date("1000-01-01"), "%Y - %B"))
    # If current tab is selected and no data is available
    if (input$MAIN_menu == "FD" & input$forecast_deepdive_panels != 'forecast_performance' & nrow(main_forecasting_table) == 0) {
      # Show notification
      showNotification(
        id = "no_FC_vs_ACT_data_warning",
        ui = "The current combination of grouping(s), model(s) and forecast date does not have any data! :(",
        duration = NULL,
        closeButton = FALSE,
        type = "error"
      )
    } else {
      # Remove notification
      removeNotification(id = "no_FC_vs_ACT_data_warning")
    }
  },
  priority = -1
)

# Show notifications if there is no data available
observe(
  x = {
    # Update main_forecasting_table
    main_forecasting_table <- MAIN_update_main_forecasting_table()
    # If current tab is selected and no data is available
    if (input$MAIN_menu == "FD" & input$forecast_deepdive_panels == 'forecast_performance' & nrow(main_forecasting_table) == 0) {
      # Show notification
      showNotification(
        id = "no_FC_PERF_data_warning",
        ui = "The current combination of grouping(s) and model(s) does not have any data! :(",
        duration = NULL,
        closeButton = FALSE,
        type = "error"
      )
    } else {
      # Remove notification
      removeNotification(id = "no_FC_PERF_data_warning")
    }
  },
  priority = -1
)


#### TAB UPDATES ####

# Hide tabs in case of non-hierarchical data
observe({
  if (!app_data$is_hierarchical) {
    shinyjs::hide(selector = "#forecast_deepdive_panels li a[data-value = hierarchical_forecast]")
  }
})

# Hide tabs in case of univariate data
observe({
  if (!app_data$is_multivariate) {
    shinyjs::hide(selector = "#forecast_deepdive_panels li a[data-value = external_regressors]")
    shinyjs::hide(selector = "#forecast_deepdive_panels li a[data-value = external_regressor_drivers]")
  }
})


#### OUTPUT UPDATES ####

# Create plot to compare fc models
output$FC_vs_ACT_plot <- plotly::renderPlotly({
  # Update main_forecasting_table
  main_forecasting_table <- FD_update_main_forecasting_table()
  # Requirements
  req(nrow(main_forecasting_table) > 0)
  req(input$MAIN_menu == "FD")
  req(input$forecast_deepdive_panels == 'forecasts_vs_actuals')
  # If mode is to compare_groupings
  if (input$MAIN_select_mode == "compare_groupings") {
    # Extra requirements
    req(input$MAIN_select_single_fc_model)
    req(input$MAIN_select_single_fc_model %in% main_forecasting_table$fc_errors[[1]]$fc_model)
    # Create plot
    plot <- tsforecast::compare_forecast_per_group(
      main_forecasting_table = main_forecasting_table,
      fc_model = input$MAIN_select_single_fc_model,
      demo_mode = input$MAIN_hide_sensitive_data
    )
  }
  # If mode is to compare_fc_models
  if (input$MAIN_select_mode == "compare_fc_models") {
    # Extra requirements
    req(input$MAIN_select_fc_models)
    req(all(input$MAIN_select_fc_models %in% main_forecasting_table$fc_errors[[1]]$fc_model))
    # Create plot
    plot <- tsforecast::compare_forecasts_with_actuals(
      main_forecasting_table = main_forecasting_table,
      fc_models = input$MAIN_select_fc_models,
      show_original = (app_data$has_original_actuals && input$MAIN_show_original),
      demo_mode = input$MAIN_hide_sensitive_data
    )
  }
  # Return plot
  return(plot)
})

# Create plot to look at hierarchical forecasts
output$HY_FC_plot <- plotly::renderPlotly({
  # Update main_forecasting_table
  main_forecasting_table <- FD_update_main_forecasting_table() 
  # Requirements
  req(nrow(main_forecasting_table) > 0)
  req(input$MAIN_menu == "FD")
  req(input$forecast_deepdive_panels == 'hierarchical_forecast')
  req(input$HY_FC_plot_hierarchical_fc_model)
  req(input$HY_FC_plot_hierarchical_group)
  req(app_data$is_hierarchical == TRUE)
  # Get selected grouping and split date
  selected_grouping <- main_forecasting_table %>% 
    dplyr::pull(grouping) %>% 
    unique()
  selected_split_date <- main_forecasting_table %>% 
    dplyr::pull(ts_split_date) %>% 
    unique()
  # Get original main forecasting table and filter
  main_forecasting_table <- app_data$main_forecasting_table %>% 
    dplyr::filter(ts_split_date == selected_split_date)
  # Create plot
  plot <- tsforecast::plot_hierarchical_forecasts(
    main_forecasting_table = main_forecasting_table,
    fc_model = input$HY_FC_plot_hierarchical_fc_model,
    hierarchical_cols = input$HY_FC_plot_hierarchical_group,
    grouping = selected_grouping,
    demo_mode = input$MAIN_hide_sensitive_data
  )
  # Return plot
  return(plot)
})

# Create plot to compare accuracy of fc models
output$FC_PERF_plot <- plotly::renderPlotly({
  # Update accuracy_overview
  accuracy_overview <- FD_update_accuracy_overview()
  # Requirements
  req(nrow(accuracy_overview) > 0)
  req(input$MAIN_menu == "FD")
  req(input$forecast_deepdive_panels == 'forecast_performance')
  # If mode is to compare_groupings
  if (input$MAIN_select_mode == "compare_groupings") {
    # Extra requirements
    req(input$MAIN_select_single_fc_model)
    req(input$MAIN_select_single_fc_model %in% accuracy_overview$fc_model)
    # Create plot
    plot <- tsforecast::compare_forecast_performance_per_group(
      accuracy_overview = accuracy_overview,
      fc_model = input$MAIN_select_single_fc_model,
      demo_mode = input$MAIN_hide_sensitive_data
    )
  }
  # If mode is to compare_fc_models
  if (input$MAIN_select_mode == "compare_fc_models") {
    # Extra requirements
    req(input$MAIN_select_fc_models)
    req(all(input$MAIN_select_fc_models %in% accuracy_overview$fc_model))
    # Create plot
    plot <- tsforecast::compare_forecasts_performance(
      accuracy_overview = accuracy_overview,
      fc_models = input$MAIN_select_fc_models,
      demo_mode = input$MAIN_hide_sensitive_data
    )
  }
  # Return plot
  return(plot)
})

# Create plot to compare external regressors and variable of interest
output$FC_vs_ACT_xreg_plot <- plotly::renderPlotly({
  # Update main_forecasting_table
  main_forecasting_table <- FD_update_main_forecasting_table()
  # Requirements
  req(nrow(main_forecasting_table) > 0)
  req(input$MAIN_menu == "FD")
  req(input$forecast_deepdive_panels == 'external_regressors')
  req(length(attr(main_forecasting_table$ts_object_train[[1]], "xreg_cols")) > 0)
  req(input$FC_vs_ACT_select_xreg)
  # Create plot
  plot <- tsforecast::plot_xreg(
    main_forecasting_table = main_forecasting_table,
    xreg = input$FC_vs_ACT_select_xreg
  )
  # Return plot
  return(plot)
})

# Create plot to compare external regressor driver across different fc_models
output$FC_vs_ACT_xreg_plot_drivers <- plotly::renderPlotly({
  # Update main_forecasting_table
  main_forecasting_table <- FD_update_main_forecasting_table()
  # Requirements
  req(nrow(main_forecasting_table) > 0)
  req(input$MAIN_menu == "FD")
  req(input$forecast_deepdive_panels == 'external_regressor_drivers')
  req(input$FC_vs_ACT_select_driver_xreg)
  req(input$MAIN_select_fc_models)
  req(length(attr(main_forecasting_table$ts_object_train[[1]], "xreg_cols")) > 0)
  # Update main_forecasting_table's fc_models
  main_forecasting_table$fc_models[[1]] <- main_forecasting_table$fc_models[[1]][input$MAIN_select_fc_models]
  # Create plot
  plot <- tsforecast::plot_xreg_drivers(
    main_forecasting_table = main_forecasting_table,
    xreg = input$FC_vs_ACT_select_driver_xreg,
    delta = input$FC_vs_ACT_select_driver_xreg_delta,
    granularity = 100
  )
  # Return plot
  return(plot)
})


#### DOWNLOAD DATA ####

# Download file handler for anomaly data
output$MAIN_download_forecast_data <- downloadHandler(
  filename = function() {
    'forecast_data.xlsx'
  },
  content = function(file) {
    # Update main_forecasting_table
    main_forecasting_table <- FD_update_main_forecasting_table()
    # Requirements
    req(nrow(main_forecasting_table) > 0)
    req(input$MAIN_menu == "FD")
    # If mode is to compare groupings
    if (input$MAIN_select_mode == "compare_groupings") {
      # Extra requirement
      req(input$MAIN_select_single_fc_model)
      fc_models <- input$MAIN_select_single_fc_model 
    }
    # If mode is to compare_fc_models
    if (input$MAIN_select_mode == "compare_fc_models") {
      # Extra requirement
      req(input$MAIN_select_fc_models)
      fc_models <- input$MAIN_select_fc_models
    }
    
    # Extract actuals from ts_objects
    actuals <- get_actuals_from_main_forecasting_table(
      main_forecasting_table = main_forecasting_table,
      for_plot = F
    )
    # Extract forecasts from fc_error data  
    forecasts <- main_forecasting_table %>% 
      dplyr::pull(fc_errors) %>% 
      dplyr::bind_rows() %>% 
      dplyr::filter(fc_model %in% fc_models) %>% 
      dplyr::transmute(
        grouping = grouping,
        period = period,
        fc_periods_ahead = fc_periods_ahead,
        fc_model = fc_model,
        value = fc_value
      ) %>% 
      # Makes into wide format, so each model has a column
      tidyr::spread(fc_model, value)
    
    # Combine actuals and forecasts into save data
    save_data <- dplyr::full_join(
        x = actuals,
        y = forecasts,
        by = c("period", "grouping")
      ) %>% 
      dplyr::filter(period <= max(forecasts$period)) %>% 
      dplyr::mutate(period = tstools::period_to_last_day(period)) %>% 
      dplyr::arrange(grouping, period, fc_periods_ahead)
    
    # Save the Excel file
    openxlsx::write.xlsx(
      x = save_data, 
      file = file
    )
  }
)
