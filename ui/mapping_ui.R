library(shinycssloaders)

fluidPage(
  # Header box (tidak berubah)
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
  
  # Layout dengan peta dan panel statistik
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
        
        # PERBAIKAN: Menggunakan withSpinner yang lebih stabil
        shinycssloaders::withSpinner(
          leafletOutput("distribution_map", height = "600px"),
          type = 6, # Pilih tipe spinner dari 1-8
          color = "#0d6efd" # Sesuaikan warna dengan tema Anda
        )
      )
    ),
    
    # Kolom panel statistik (tidak berubah)
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
  
  # Baris Interpretasi (tidak berubah)
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
        uiOutput("map_interpretation_content")
      )
    )
  )
)