
# Create header
header <- shinydashboard::dashboardHeader(
  title = div(
    img(
      src = "white_space.png",
      height = 35,
      width = 110,
      style = "
        margin-top: -10px; 
        padding: 5px 5px 5px 5px; 
      "
    )
  ),
  titleWidth = 350
)

# Create sidebar
sidebar <- shinydashboard::dashboardSidebar(
  width = 350,
  # Source ui for dashboard sidebar
  source('sidebar_ui.R', local = T)[1]
)

# Create body
body <- shinydashboard::dashboardBody(
  # Use ING style from css file
  tags$head(
    tags$link(
      rel = "stylesheet", type = "text/css", href = "style.css"
    )
  ),
  # Enable shinyjs
  shinyjs::useShinyjs(),
  # Source ui for dashboard body per tab
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "VH",
      source('VH_view_hierarchy_ui.R', local = T)[1]
    ),
    shinydashboard::tabItem(
      tabName = "FD",
      source('FD_forecast_deepdive_ui.R', local = T)[1]
    )
  )
)

# Create dashboard
shiny::shinyUI(
  shinydashboard::dashboardPage(
    header = header, 
    sidebar = sidebar, 
    body = body,
    title = "Forecast Dashboard",
    skin = "blue"
  )
)
