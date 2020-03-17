
#### DATA UPDATES ####

# Reactive dataset for previous grouping
MAIN_previous_values <- reactiveValues(
  groupings = c()
)

# Update all interaction inputs
MAIN_update_interaction_inputs <- reactive({
  all_inputs <- reactiveValuesToList(input)
  interaction_inputs <- grepl("MAIN_|FC_vs_ACT_|FC_PERF_", names(all_inputs))
  return(all_inputs[interaction_inputs])
})

# Update grouping inputs
MAIN_update_groupings <- reactive({
  # Copy groupings to filter
  groupings <- app_data$groupings
  # Get group columns
  groups <- groupings %>% 
    dplyr::select(-grouping) %>% 
    colnames()
  # If mode is to compare_groupings
  if (input$MAIN_select_mode == "compare_groupings") selectinput_prefix <- "MAIN_select_groupings_"
  if (input$MAIN_select_mode == "compare_fc_models") selectinput_prefix <- "MAIN_select_single_grouping_"
  # Loop over each grouping input
  for (group in groups) {
    # Define selectize name
    selectize_name <- paste0(selectinput_prefix, group)
    # Only start when the input is available
    req(input[[selectize_name]])
    # Extract column and value(s)
    column <- unique(gsub(" = .*", "", input[[selectize_name]]))
    values <- gsub(".* = ", "", input[[selectize_name]])
    # Filter groupings
    groupings <- groupings %>% 
      dplyr::filter(!! dplyr::sym(column) %in% c(!!! values))
  }
  # Return only the grouping column
  groupings %>% 
    dplyr::pull(grouping) %>% 
    return()
})

# Update main_forecasting_table
MAIN_update_main_forecasting_table <- reactive({
  # Get groupings
  groupings <- MAIN_update_groupings()
  # Return filtered main_forecasting_table
  app_data$main_forecasting_table %>% 
    dplyr::filter(grouping %in% groupings) %>% 
    return()
})

# Update accuracy_overview
MAIN_update_accuracy_overview <- reactive({
  # Update main_forecasting_table
  main_forecasting_table <- MAIN_update_main_forecasting_table()
  # Requirements
  req(nrow(main_forecasting_table) > 0)
  # Calculate forecasting_accuracy
  main_forecasting_table %>% 
    get_forecast_accuracy_overview()
})

# Update top x models fc_models
MAIN_update_top_x_fc_models <- reactive({
  # Update accuracy_overview
  accuracy_overview <- MAIN_update_accuracy_overview()
  # Requirements
  req(nrow(accuracy_overview) > 0)
  # Update number of models to select
  n_models <- input$MAIN_top_x_to_select
  # Get top x fc_models
  accuracy_overview %>% 
    dplyr::filter(!grepl("RFC_misob_", fc_model)) %>% 
    get_best_forecast_methods(n = as.numeric(n_models)) %>% 
    dplyr::pull(fc_model) %>% 
    return()
})


#### INITIALIZE SELECTIZES ####

# Update available selectInputs for groupings
output$MAIN_uiOutput_select_single_grouping <- renderUI({
  # Create tagList to put selectInputs in
  TagList_for_groupings <- tagList()
  # Split grouping column
  groups <- app_data$groupings %>% 
    dplyr::select(-grouping)
  # Add selectInput for every group
  for (group in colnames(groups)) {
    # Create clean choices
    clean_choices <- groups %>% 
      dplyr::pull(group) %>% 
      unique() %>%
      paste0(group, " = ", .)
    # Check if hierarchical dataset
    if (app_data$is_hierarchical) {
      # Get hierarchy list
      hierarchy <- app_data$main_forecasting_table$hierarchy[[1]]
      # Loop over each group in the matrix
      if (group %in% comment(hierarchy$matrix)) {
        # Define level column
        level_col <- paste0("level_", group)
        # Extract level information
        level_data <- hierarchy$data %>% 
          dplyr::select(group, level_col) %>% 
          dplyr::distinct()
        # Add dashes to the front of the choices
        hierarchical_choices <- groups %>% 
          dplyr::select(group) %>% 
          dplyr::distinct() %>% 
          # Re-arrange
          dplyr::arrange(match(!! dplyr::sym(group), rownames(hierarchy$matrix))) %>% 
          # Put in level information
          dplyr::left_join(
            x = .,
            y = level_data,
            by = group
          ) %>% 
          dplyr::rowwise() %>% 
          # Get dashes
          dplyr::mutate(dashes := paste0(rep('-', !! dplyr::sym(level_col)), collapse = '')) %>% 
          # Put dashes and hierarchy groups together
          dplyr::mutate(!! dplyr::sym(group) := paste0(dashes, !! dplyr::sym(group))) %>% 
          # Pull and organize
          dplyr::pull(group) %>% 
          paste0(group, " = ", .)
        # Use setNames for the selectInput
        clean_choices <- setNames(
          object = clean_choices,
          nm = hierarchical_choices
        )
      }
    }
    # Define new selectInput
    new_selectInput <- selectInput(
      inputId = paste0("MAIN_select_single_grouping_", group),
      label = NULL,
      choices = clean_choices,
      width = "100%"
    )
    # Append new selectInput to list of existing inputs
    TagList_for_groupings <- tagAppendChild(
      TagList_for_groupings,
      new_selectInput
    )
  }
  # Return updated list of inputs
  TagList_for_groupings
})

# Update available selectInputs for groupings
output$MAIN_uiOutput_select_groupings <- renderUI({
  # Create tagList to put selectInputs in
  TagList_for_groupings <- tagList()
  # Split grouping column
  groups <- app_data$groupings %>% 
    dplyr::select(-grouping)
  # Add selectInput for every group
  for (group in colnames(groups)) {
    # Define new selectInput
    new_selectInput <- selectInput(
      inputId = paste0("MAIN_select_groupings_", group),
      label = NULL,
      choices = groups %>% 
        dplyr::pull(group) %>% 
        unique() %>% 
        paste0(group, " = ", .),
      multiple = T,
      width = "100%"
    )
    # Append new selectInput to list of existing inputs
    TagList_for_groupings <- tagAppendChild(
      TagList_for_groupings,
      new_selectInput
    )
  }
  # Return updated list of inputs
  TagList_for_groupings
})


#### UPDATE SELECTIZES ####

# Update available fc_models when changing grouping
observe(
  x = {
    # Update main_forecasting_table
    main_forecasting_table <- MAIN_update_main_forecasting_table()
    # Requirements
    req(nrow(main_forecasting_table) > 0)
    # Get available fc_models
    fc_models <- main_forecasting_table$fc_errors %>% 
      dplyr::bind_rows() %>% 
      dplyr::pull(fc_model) %>% 
      unique()
    # Update choices for MAIN_select_single_fc_model
    updateSelectInput(
      session = session,
      inputId = "MAIN_select_single_fc_model",
      choices = fc_models,
      selected = input$MAIN_select_single_fc_model
    )
    # Update choices for MAIN_select_fc_models
    updateSelectInput(
      session = session,
      inputId = "MAIN_select_fc_models",
      choices = fc_models,
      selected = MAIN_update_top_x_fc_models()
    )
  },
  priority = -1
)

# Update selected fc_models when changing groups
observe(
  x = {
    # Get grouping inputs
    groupings <- MAIN_update_groupings()
    prev_groupings <- MAIN_previous_values$groupings
    # Requirements
    req(length(groupings) > 0)
    if (length(prev_groupings) > 0) {
      # Check if the grouping has changed
      unequal_length <- (length(unlist(groupings)) != length(unlist(prev_groupings)))
      if (unequal_length || !all(unlist(groupings) == unlist(prev_groupings))) {
        # Click button
        shinyjs::click("MAIN_select_top_x_fc_models")
      }
    }
    # Store the grouping for the next iteration
    MAIN_previous_values$groupings <- groupings
  },
  priority = 1
)

# Update label for top x models to select
observeEvent(
  eventExpr = input$MAIN_top_x_to_select,
  handlerExpr = {
    # Update choices for MAIN_select_top_x_fc_models
    updateSelectInput(
      session = session,
      inputId = "MAIN_select_top_x_fc_models",
      label = paste0(" Top ", input$MAIN_top_x_to_select)
    )
    # Click button
    shinyjs::click("MAIN_select_top_x_fc_models")
  }
)


#### FREEZE SELECTIZES ####

# Observe event of changing inputs
observeEvent(
  eventExpr = MAIN_update_interaction_inputs(),
  handlerExpr = {
    # Determine all selectInputs to freeze
    inputs_to_freeze <- MAIN_update_interaction_inputs()
    # Freeze selectizes until plot is ready
    for (input_to_freeze in names(inputs_to_freeze)) {
      shinyjs::disable(input_to_freeze)
    }
  },
  priority = 3
)
# Observe event of changing inputs
observeEvent(
  eventExpr = MAIN_update_interaction_inputs(),
  handlerExpr = {
    # Determine all selectInputs to unfreeze
    inputs_to_unfreeze <- MAIN_update_interaction_inputs()
    # Unfreeze selectizes after is ready
    for (input_to_unfreeze in names(inputs_to_unfreeze)) {
      shinyjs::enable(input_to_unfreeze)
    }
  },
  priority = -3
)


#### NOTIFICATIONS ####

# Show notifications if inputs are missing
observe(
  x = {
    # If mode is to compare_groupings
    if (input$MAIN_menu == "FD" & input$MAIN_select_mode == "compare_groupings") {
      # Remove notifications for other mode
      removeNotification(id = "no_fc_models_warning")
      # Get groupings inputs
      groupings_inputs <- app_data$groupings %>% 
        dplyr::select(-grouping) %>% 
        colnames() %>% 
        paste0("MAIN_select_groupings_", .)
      # Check which groupings are null
      null_groupings <- purrr::map_lgl(
        .x = groupings_inputs, 
        .f = ~ is.null(input[[.x]])
      )
      # If no groupings are selected
      if (any(null_groupings)) {
        # Show notification
        showNotification(
          id = "no_groupings_warning",
          ui = "Please select one or more groups!",
          duration = NULL,
          closeButton = FALSE,
          type = "warning"
        )
      } else {
        # Remove notification
        removeNotification(id = "no_groupings_warning")
      }
      # If no single fc_model is selected
      if (is.null(input$MAIN_select_single_fc_model) | input$MAIN_select_single_fc_model == "") {
        # Show notification
        showNotification(
          id = "no_single_fc_model_warning",
          ui = "Please select one of the models!",
          duration = NULL,
          closeButton = FALSE,
          type = "warning"
        )
      } else {
        # Remove notification
        removeNotification(id = "no_single_fc_model_warning")
      }
    }
    
    # If mode is to compare_fc_models
    if (input$MAIN_menu == "FD" & input$MAIN_select_mode == "compare_fc_models") {
      # Remove notifications for other mode
      removeNotification(id = "no_groupings_warning")
      removeNotification(id = "no_single_fc_model_warning")
      # If no fc_models are selected
      if (is.null(input$MAIN_select_fc_models)) {
        # Show notification
        showNotification(
          id = "no_fc_models_warning",
          ui = "Please select one or more models!",
          duration = NULL,
          closeButton = FALSE,
          type = "warning"
        )
      } else {
        # Remove notification
        removeNotification(id = "no_fc_models_warning")
      }
    }
    
    # If a different tab is selected
    if (input$MAIN_menu != "FD") {
      # Remove notifications
      removeNotification(id = "no_groupings_warning")
      removeNotification(id = "no_single_fc_model_warning")
      removeNotification(id = "no_fc_models_warning")
    }
  },
  priority = -1
)


#### BUTTON EVENTS ####

# Observe event of selecting all groupings
observeEvent(
  eventExpr = input$MAIN_select_all_groupings,
  {
    # Split grouping column
    groups <- app_data$groupings %>% 
      dplyr::select(-grouping)
    # Update selected for each groupings_input
    for (group in colnames(groups)) {
      updateSelectInput(
        session = session,
        inputId = paste0("MAIN_select_groupings_", group),
        selected = groups %>% 
          dplyr::pull(group) %>% 
          unique() %>% 
          paste0(group, " = ", .)
      )
    }
  }
)

# Observe event of selecting no groupings
observeEvent(
  eventExpr = input$MAIN_select_no_groupings,
  {
    # Split grouping column
    groups <- app_data$groupings %>% 
      dplyr::select(-grouping)
    # Update selected for each groupings_input
    for (group in colnames(groups)) {
      updateSelectInput(
        session = session,
        inputId = paste0("MAIN_select_groupings_", group),
        selected = character(0)
      )
    }
  }
)

# Observe event of selecting top x fc_models
observeEvent(
  eventExpr = input$MAIN_select_top_x_fc_models,
  {
    # Update top_x_fc_models to select
    top_x_fc_models <- MAIN_update_top_x_fc_models()
    # Update selected for MAIN_select_fc_models
    updateSelectInput(
      session = session,
      inputId = "MAIN_select_fc_models",
      selected = top_x_fc_models
    )
  }
)

# Observe event of selecting no fc_models
observeEvent(
  eventExpr = input$MAIN_select_no_fc_models,
  {
    # Update selected for MAIN_select_fc_models
    updateSelectInput(
      session = session,
      inputId = "MAIN_select_fc_models",
      selected = character(0)
    )
  }
)


#### INITIALIZE DASHBOARD ####

# Initialize latest available data message
output$latest_available_data <- renderText({
  # Format latest split date
  app_data$latest_split_date %>% 
    tstools::period_to_last_day() %>% 
    format("%Y - %B")
})

# Initialize with selecting top x models
observe(
  x = {
    # Click button
    shinyjs::click("MAIN_select_top_x_fc_models")
  },
  priority = -1
)
