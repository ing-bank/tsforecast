
fluidPage(
  fluidRow(
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(
        outputId = "FC_vs_ACT_xreg_plot_drivers",
        height = "700px"
      ),
      type = 6,
      color = "#FF6200"
    )
  )
)
