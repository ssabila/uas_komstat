# ui/visualization_ui.R
fluidPage(
  box(
    title = "Kontrol Visualisasi",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(4, uiOutput("vis_variable_selector")),
      column(4, uiOutput("vis_group_selector")),
      column(4, uiOutput("vis_plot_type_selector"))
    )
  ),
  box(
    title = "Hasil Visualisasi",
    status = "info",
    solidHeader = TRUE,
    width = 12,
    plotlyOutput("main_plot", height = "500px")
  ),
  box(
    title = "Heatmap Korelasi",
    status = "warning",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,
    plotlyOutput("correlation_heatmap")
  )
)