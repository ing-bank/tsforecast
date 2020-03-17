
#### DEFINE DYNAMIC INPUTS ####

# Define which menu items to show
DYNAMIC_menu_items <- list()
# Add items for viewing hierarchy, if data is hierarchical
if (app_data$is_hierarchical) {
  DYNAMIC_menu_items[[length(DYNAMIC_menu_items) + 1]] <- shinydashboard::menuItem(
    text = "View Hierarchy", 
    tabName = "VH", 
    icon = icon("sitemap")
  )
}
# Add default items
DYNAMIC_menu_items[[length(DYNAMIC_menu_items) + 1]] <- shinydashboard::menuItem(
  text = "Forecast Deepdive", 
  tabName = "FD", 
  icon = icon("search")
)

# Define which checkboxes to show
DYNAMIC_checkboxes <- list(br())
# Add checkbox to show original actuals or not
if (app_data$has_original_actuals) {
  DYNAMIC_checkboxes[[length(DYNAMIC_checkboxes) + 1]] <- checkboxInput(
    inputId = 'MAIN_show_original',
    label = 'Show original actuals (if available)',
    value = TRUE
  )
}


#### CREATE SIDEBAR MENU ####

# Using the dynamic inputs
shinydashboard::sidebarMenu(
  id = "MAIN_menu",
  
  # Add package logo
  fluidRow(
    column(
      width = 12,
      align = "center",
      img(
        src = 'tsforecast_logo.png', 
        width = "125px"
      )
    )
  ),

  # Extra information for dashboard users
  h5("Latest available data:", align = "center"),
  fluidRow(
    column(
      width = 12,
      align = "center",
      # Define latest available data
      tags$div(
        title = app_data$documentation$MAIN_latest_available_data,
        textOutput("latest_available_data")
      ),
      # Button to hide sensitive data in the dashboard
      tags$div(
        title = app_data$documentation$MAIN_hide_sensitive_data,
        checkboxInput(
          inputId = 'MAIN_hide_sensitive_data', 
          label = 'Hide sensitive data',
          value = FALSE
        )
      )
    )
  ),
  
  # Line break
  hr(class = "thick"),
  # Indicitate tabs below
  h4("Forecast Views", align = "center"),
  # Line break
  hr(class = "thick"),
  # Source the ui for each tab dynamically
  DYNAMIC_menu_items,

  # Line break
  hr(class = "thick"),
  # Indicitate tabs below
  h4("Adjust selection", align = "center"),
  # Line break
  hr(class = "thick"),
  # Row with selectInputs
  fluidRow(
    column(
      width = 10,
      align = "center",
      offset = 1,
      # If tabitem is view hierarchy/forecast deepdive
      conditionalPanel(
        condition = "['VH', 'FD'].includes(input.MAIN_menu)",
        # Select mode of comparison
        h5("Select mode:"),
        selectInput(
          inputId = "MAIN_select_mode",
          label = NULL,
          choices = list(
            "Compare models for one group" = "compare_fc_models",
            "Compare groups for one model" = "compare_groupings"
          ),
          width = "100%"
        )
      ),

      # If tabitem is view hierarchy/forecast deepdive AND mode is to compare_fc_groupings
      conditionalPanel(
        condition = "(['VH', 'FD'].includes(input.MAIN_menu) & input.MAIN_select_mode == 'compare_groupings')",
        # Select groupings to view
        h5("Select groups:"),
        uiOutput("MAIN_uiOutput_select_groupings"),
        # Row with buttons
        fluidRow(
          # First button
          column(
            width = 5,
            align = "center",
            offset = 1,
            # Button to select all columns
            actionButton(
              inputId = "MAIN_select_all_groupings",
              label = " All",
              icon = icon(name = "check"),
              width = "100%",
              style = "
                padding: 0px 0px 0px 0px; 
                margin: 0px 0px 0px 0px;
              "
            ),
            style = "
              padding: 0px 0px 0px 0px;
              margin: 0px 0px 0px 20px;
            "
          ),
          # Second button
          column(
            width = 5,
            align = "center",
            offset = 1,
            # Button to deselect all columns
            actionButton(
              inputId = "MAIN_select_no_groupings",
              label = " None",
              icon = icon(name = "remove"),
              width = "100%",
              style = "
                padding: 0px 0px 0px 0px; 
                margin: 0px 0px 0px 0px;
              "
            ),
            style = "
              padding: 0px 0px 0px 0px;
              margin: 0px 0px 0px 10px;
            "
          )
        )
      ),
      # If tabitem is forecast deepdive AND mode is to compare_fc_groupings
      conditionalPanel(
        condition = "(input.MAIN_menu == 'FD' & input.MAIN_select_mode == 'compare_groupings')",
        # Select single fc_model to view
        h5("Select model:"),
        selectInput(
          inputId = "MAIN_select_single_fc_model",
          label = NULL,
          choices = NULL,
          width = "100%"
        )
      ),
      
      # If tabitem is view hierarchy/forecast deepdive AND mode is to compare_fc_models
      conditionalPanel(
        condition = "(['VH', 'FD'].includes(input.MAIN_menu) & input.MAIN_select_mode == 'compare_fc_models')",
        # Select single grouping to view
        h5("Select group:"),
        uiOutput("MAIN_uiOutput_select_single_grouping")
      ),
      
      # If tabitem is forecast deepdive AND mode is to compare_fc_models AND not hierarchical_forecasts
      conditionalPanel(
        condition = "(input.MAIN_menu == 'FD' & input.MAIN_select_mode == 'compare_fc_models' & input.forecast_deepdive_panels != 'hierarchical_forecast')",
        # Select fc_models to view
        h5("Select models:"),
        selectInput(
          inputId = "MAIN_select_fc_models",
          label = NULL,
          choices = NULL,
          multiple = TRUE,
          width = "100%"
        ),
        # Row with buttons
        fluidRow(
          # First button
          column(
            width = 5,
            align = "center",
            offset = 1,
            # Button to select all columns
            actionButton(
              inputId = "MAIN_select_top_x_fc_models",
              label = " Top 3",
              icon = icon(name = "check"),
              width = "100%",
              style = "
                padding: 0px 0px 0px 0px; 
                margin: 0px 0px 0px 0px;
              "
            ),
            style = "
              padding: 0px 0px 0px 0px;
              margin: 0px 0px 0px 20px;
            "
          ),
          # Second button
          column(
            width = 5,
            align = "center",
            # Button to deselect all columns
            actionButton(
              inputId = "MAIN_select_no_fc_models",
              label = " None",
              icon = icon(name = "remove"),
              width = "100%",
              style = "
                padding: 0px 0px 0px 0px; 
                margin: 0px 0px 0px 0px;
              "
            ),
            style = "
              padding: 0px 0px 0px 0px;
              margin: 0px 0px 0px 10px;
            "
          )
        ),
        fluidRow(
          column(
            width = 12,
            allign = "center",
            radioButtons(
              inputId = "MAIN_top_x_to_select",
              label = NULL, 
              choices = c(1, 2, 3, 5, 7, 10), 
              selected = 3,
              inline = T
            )
          )
        )
      ),
      
      # If tabitem is forecast deepdive AND tab is 'forecast_vs_actuals' OR 'forecast_performance'
      conditionalPanel(
        condition = "input.MAIN_menu == 'FD' & ['forecasts_vs_actuals', 'forecast_performance'].includes(input.forecast_deepdive_panels)
        ",
        # Specify required checkboxes dynamically
        DYNAMIC_checkboxes
      ),
      
      # If tabitem is forecast deepdive AND tab is 'forecast_vs_actuals'
      conditionalPanel(
        condition = "input.MAIN_menu == 'FD' & input.forecast_deepdive_panels == 'forecasts_vs_actuals'",
        # Some extra space
        br(),
        # Button to download anomaly data
        downloadButton(
          outputId = "MAIN_download_forecast_data",
          label = "Download Forecast Data",
          style = "display: block; margin: 0 auto; width: auto;color: black;"
        )
      )
    )
  
  )
  
)
