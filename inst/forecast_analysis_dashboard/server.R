
# Function that defines the shiny server
shiny::shinyServer(function(input, output, session) {
  
  # Main server functions
  source('main_server.R',local = TRUE)
  # Server functions for the view hierarchy tab
  source('VH_view_hierarchy_server.R',local = TRUE)
  # Server functions for the forecast deepdive tab
  source('FD_forecast_deepdive_server.R',local = TRUE)
  
  # Stop server upon closing the browser tab
  session$onSessionEnded(stopApp)
  
})
