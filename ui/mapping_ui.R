fluidPage(
  # Header box
  box(
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
      box(
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
      box(
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
  
  # Box informasi tambahan
  box(
    title = "Interpretasi Peta",
    status = "warning",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,
    div(
      h4("Cara Membaca Peta:"),
      tags$ul(
        tags$li(strong("Warna Merah/Gelap:"), " Nilai tinggi untuk variabel yang dipilih"),
        tags$li(strong("Warna Kuning/Terang:"), " Nilai rendah untuk variabel yang dipilih"),
        tags$li(strong("Area Abu-abu:"), " Data tidak tersedia"),
        tags$li(strong("Klik pada area:"), " Untuk melihat statistik detail kabupaten/kota")
      ),
      h4("Analisis Statistik:"),
      tags$ul(
        tags$li(strong("Persentil:"), " Menunjukkan posisi relatif dibanding daerah lain"),
        tags$li(strong("Status Tinggi (>75%):"), " Daerah berisiko tinggi"),
        tags$li(strong("Status Sedang (50-75%):"), " Daerah berisiko menengah"),
        tags$li(strong("Status Rendah (<50%):"), " Daerah berisiko rendah")
      )
    )
  )
)