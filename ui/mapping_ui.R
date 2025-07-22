fluidPage(
  # Header box (existing)
  bs4Card(
    title = "Peta Distribusi Spasial",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Pilih variabel untuk melihat distribusinya secara geografis di seluruh kabupaten/kota di Indonesia."),
    p(style = "color: #007bff; font-weight: bold;", 
      icon("info-circle"), " Klik pada kabupaten/kota di peta untuk melihat statistik detail!"),
    uiOutput("map_variable_selector")
  ),
  
  # Layout dengan peta dan panel statistik (existing)
  fluidRow(
    # Kolom peta
    column(
      width = 8,
      bs4Card(
        title = "Peta Interaktif",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        height = "700px",
        leafletOutput("distribution_map", height = "600px")
      )
    ),
    
    # Kolom panel statistik
    column(
      width = 4,
      bs4Card(
        title = "Statistik Deskriptif",
        status = "success",
        solidHeader = TRUE,
        width = NULL,
        height = "700px",
        style = "overflow-y: auto;",
        uiOutput("district_info_panel")
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      bs4Card(
        title = "Interpretasi Visualisasi Peta",
        status = "warning",
        solidHeader = TRUE,
        width = NULL,
        height = "auto",
        style = "margin-top: 15px;",
        
        # Konten interpretasi dinamis
        uiOutput("map_interpretation_content")
      )
    )
  )
)
