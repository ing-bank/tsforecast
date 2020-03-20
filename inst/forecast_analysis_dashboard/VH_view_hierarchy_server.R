
#### NODE SELECTION UPDATES ####

# Update selected nodes when grouping is updated
observe({
  # Requirement
  req(input$MAIN_select_mode == "compare_fc_models")
  # Get groups
  groups <- app_data$groupings %>%
    dplyr::select(-grouping) %>%
    colnames()
  # Update selected node based on groups
  for (group in groups) {
    # Define selectize name
    selectize_name <- paste0("MAIN_select_single_grouping_", group)
    # Extract value
    selectize_value <- input[[selectize_name]] %>%
      gsub(group, "", .) %>%
      gsub(" = ", "", .)
    # Define network name
    network_name <- paste0("VH_", group)
    # Change selected node
    network_name %>%
      visNetwork::visNetworkProxy() %>%
      visNetwork::visOptions(
        highlightNearest = list(
          enabled = TRUE,
          degree = list(
            from = 100,
            to = 0
          ),
          algorithm = "hierarchical"
        )
      ) %>% 
      visNetwork::visInteraction(multiselect = FALSE) %>% 
      visNetwork::visSelectNodes(selectize_value)
  }
})

# Update grouping when a node is selected
observe({
  # Requirement
  req(input$MAIN_select_mode == "compare_fc_models")
  # Show each selected node
  for (hierarchical_col in app_data$hierarchical_cols) {
    # Get node value
    node_value <- input[[paste0("VH_",hierarchical_col,"_node")]]
    # Continue if a node is selected
    if (!is.null(node_value)) {
      # Group to update
      group_to_update <- paste0("MAIN_select_single_grouping_", hierarchical_col)
      # Get value to set
      value_to_set <- paste0(
        hierarchical_col, " = ", input[[paste0("VH_",hierarchical_col,"_node")]]
      )
      # Update selectInput
      updateSelectInput(
        session = session,
        inputId = group_to_update,
        selected = value_to_set
      )
    }
  }
})

# Update selected nodes when grouping is updated
observe({
  # Requirement
  req(input$MAIN_select_mode == "compare_groupings")
  # Get groups
  groups <- app_data$groupings %>%
    dplyr::select(-grouping) %>%
    colnames()
  # Update selected node based on groups
  for (group in groups) {
    # Define selectize name
    selectize_name <- paste0("MAIN_select_groupings_", group)
    # Extract value
    selectize_values <- input[[selectize_name]] %>%
      gsub(group, "", .) %>%
      gsub(" = ", "", .)
    # Define network name
    network_name <- paste0("VH_", group)
    # Change selected node
    network_name %>%
      visNetwork::visNetworkProxy() %>%
      visNetwork::visOptions(
        highlightNearest = list(
          enabled = FALSE
        )
      ) %>% 
      visNetwork::visInteraction(multiselect = TRUE) %>% 
      visNetwork::visSelectNodes(selectize_values)
  }
})

# Update grouping when a node is selected
observe({
  # Requirement
  req(input$MAIN_select_mode == "compare_groupings")
  # Show each selected node
  for (hierarchical_col in app_data$hierarchical_cols) {
    # Get node value
    node_values <- input[[paste0("VH_",hierarchical_col,"_node")]]
    # Continue if a node is selected
    if (!is.null(node_values)) {
      # Group to update
      group_to_update <- paste0("MAIN_select_groupings_", hierarchical_col)
      # Get value to set
      values_to_set <- paste0(
        hierarchical_col, " = ", input[[paste0("VH_",hierarchical_col,"_node")]]
      )
      # Update selectInput
      updateSelectInput(
        session = session,
        inputId = group_to_update,
        selected = values_to_set
      )
    }
  }
})


#### OUTPUT UPDATES ####

# Create visnetwork outputs for each hierarchical column
output$VH_uiOutput_hierarchy <- renderUI({
  # Create tagList to put visnetworks in
  TagList_for_hierarchy <- tagList()
  # Loop over hierarchical columns
  for (hierarchical_col in app_data$hierarchical_cols) {
    # Determine network name
    name <- paste0("VH_", hierarchical_col)
    # Define new selectInput
    new_visnetwork <- shinydashboard::box(
      title = hierarchical_col,
      visNetwork::visNetworkOutput(
        outputId = name,
        height = "350px"
      )
    )
    # Append new selectInput to list of existing inputs
    TagList_for_hierarchy <- tagAppendChild(
      TagList_for_hierarchy,
      new_visnetwork
    )
  }
  # Return updated list of inputs
  TagList_for_hierarchy
})

# Create visnetwork to render for each hierarchical column
for (hierarchical_col in app_data$hierarchical_cols) {
  # Use local because of this: https://gist.github.com/wch/5436415/
  local({
    # Copy it first, to make local work
    col <- hierarchical_col
    # Determine network name
    name <- paste0("VH_", col)
    # Overwrite visnetwork output
    output[[name]] <- visNetwork::renderVisNetwork(
      tstools::plot_hierarchy(
          hierarchy = app_data$hierarchy,
          hierarchical_col = col,
          interact = T
        ) %>%
        visNetwork::visEvents(
          select = paste0("
            function(nodes) {
              Shiny.onInputChange('",name,"_node', nodes.nodes);
            ;}
          ")
        )
    )
  })
}
