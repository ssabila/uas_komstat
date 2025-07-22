# ui/visualization_ui.R
fluidPage(
  # Kontrol Visualisasi
  bs4Card(
    title = "Kontrol Visualisasi",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    
    fluidRow(
      # Kolom 1: Jenis Plot (Lebar 2 dari 12)
      column(2,
             selectInput("vis_plot_type", "Jenis Plot:",
                         choices = c("Histogram", "Boxplot", "Scatter Plot"),
                         selected = "Scatter Plot") # Diubah agar sesuai gambar
      ),
      
      # Kolom 2: Kontrol Dinamis (Lebar 10 dari 12)
      # Semua kontrol yang berubah akan muncul di kolom ini.
      column(10,
             # KONTROL UNTUK HISTOGRAM & BOXPLOT
             conditionalPanel(
               condition = "input.vis_plot_type == 'Histogram' || input.vis_plot_type == 'Boxplot'",
               fluidRow( # Baris di dalam kolom dinamis
                 column(6, uiOutput("vis_variable_selector")),
                 column(6, uiOutput("vis_group_selector"))
               )
             ),
             
             # KONTROL UNTUK SCATTER PLOT
             conditionalPanel(
               condition = "input.vis_plot_type == 'Scatter Plot'",
               fluidRow( # Baris di dalam kolom dinamis
                 column(3, uiOutput("vis_x_variable_selector")),
                 column(3, uiOutput("vis_y_variable_selector")),
                 column(3, uiOutput("vis_color_selector")),
                 column(3,
                        div(style = "margin-top: 32px;", # Disesuaikan sedikit untuk alignment
                            checkboxInput("vis_show_trend", "Garis Tren", value = TRUE)
                        )
                 )
               )
             )
      ) 
    ),
    
    # Info panel untuk scatter plot (tetap di bawah kontrol)
    conditionalPanel(
      condition = "input.vis_plot_type == 'Scatter Plot' && input.vis_x_variable_selector == input.vis_y_variable_selector",
      div(
        class = "alert alert-warning", # Diubah menjadi warning untuk perhatian lebih
        style = "margin-top: 10px; padding: 8px 12px;",
        HTML("<i class='fa fa-exclamation-triangle'></i> <strong>Peringatan:</strong> Variabel untuk X dan Y sebaiknya berbeda untuk analisis yang berarti.")
      )
    )
  ),
  
  # Visualisasi Utama dengan Interpretasi
  fluidRow(
    column(8,
           bs4Card(
             title = "Hasil Visualisasi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             plotlyOutput("main_plot", height = "500px")
           )
    ),
    column(4,
           bs4Card(
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
           bs4Card(
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
           bs4Card(
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