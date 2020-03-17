
fluidPage(
  fluidRow(
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(
        outputId = "FC_vs_ACT_xreg_plot",
        height = "700px"
      ),
      type = 6,
      color = "#FF6200"
    )
  )
)
