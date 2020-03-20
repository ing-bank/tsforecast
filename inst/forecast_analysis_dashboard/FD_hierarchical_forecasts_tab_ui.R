
fluidPage(
  fluidRow(
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(
        outputId = 'HY_FC_plot', 
        height = "700px"
      ),
      type = 6,
      color = "#FF6200"
    )
  )
)
