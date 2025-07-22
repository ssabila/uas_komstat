# ui/visualization_ui.R - Versi yang Diperbaiki
fluidPage(
  # Kontrol Visualisasi
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
  
  # Visualisasi Utama dengan Interpretasi
  fluidRow(
    column(8,
           box(
             title = "Hasil Visualisasi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             plotlyOutput("main_plot", height = "500px")
           )
    ),
    column(4,
           box(
             title = "Interpretasi Sebaran Data",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             height = "500px",
             style = "overflow-y: auto;",
             htmlOutput("data_interpretation")
           )
    )
  ),
  
  # Heatmap Korelasi dengan Interpretasi
  fluidRow(
    column(8,
           box(
             title = "Heatmap Korelasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             collapsed = TRUE,
             plotlyOutput("correlation_heatmap", height = "600px")
           )
    ),
    column(4,
           box(
             title = "Analisis Korelasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             collapsed = TRUE,
             height = "600px",
             style = "overflow-y: auto;",
             htmlOutput("correlation_interpretation")
           )
    )
  )
)