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
  )
)