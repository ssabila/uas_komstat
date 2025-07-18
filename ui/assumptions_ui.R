tagList(
  box(
    title = "Uji Asumsi Statistik",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Lakukan uji prasyarat sebelum analisis inferensia, seperti uji normalitas dan homogenitas variansi."),
    div(
      style = "background: #e3f2fd; padding: 10px; border-radius: 5px; border-left: 4px solid #2196f3;",
      icon("info-circle", style = "color: #2196f3;"),
      strong(" Catatan Penting:"),
      tags$ul(
        tags$li("Uji normalitas menggunakan Shapiro-Wilk Test"),
        tags$li("Uji homogenitas menggunakan Levene Test"),
        tags$li("Pastikan data memiliki variabel grup yang valid untuk uji homogenitas")
      )
    )
  ),
  fluidRow(
    # Kolom Uji Normalitas
    column(6,
           box(
             title = "Uji Normalitas (Shapiro-Wilk)",
             status = "info",
             width = 12,
             uiOutput("normality_variable_selector"),
             actionButton("run_normality_test", "Jalankan Uji", class = "btn-success", icon = icon("play")),
             hr(),
             h4("Hasil Uji:"),
             verbatimTextOutput("normality_test_result"),
             h4("Plot Q-Q:"),
             plotOutput("qq_plot", height = "300px"),
             hr(),
             h4("Unduh Hasil"),
             fluidRow(
               column(8,
                      radioButtons("normality_format", "Pilih Format:", 
                                   choices = list("PDF" = "pdf", "Word" = "docx"), 
                                   inline = TRUE)
               ),
               column(4,
                      downloadButton("download_normality_result", "Unduh", class = "btn-primary")
               )
             )
           )
    ),
    
    # Kolom Uji Homogenitas
    column(6,
           box(
             title = "Uji Homogenitas Variansi (Levene)",
             status = "info",
             width = 12,
             uiOutput("homogeneity_variable_selector"),
             uiOutput("homogeneity_group_selector"),
             
             # TAMBAHAN: Status validasi
             div(
               style = "margin: 10px 0; padding: 10px; background: #f8f9fa; border-radius: 5px;",
               h5("Status Validasi:", style = "margin-top: 0;"),
               uiOutput("homogeneity_validation_status")
             ),
             
             actionButton("run_homogeneity_test", "Jalankan Uji", class = "btn-success", icon = icon("play")),
             hr(),
             h4("Hasil Uji:"),
             verbatimTextOutput("homogeneity_test_result"),
             hr(),
             h4("Unduh Hasil"),
             fluidRow(
               column(8,
                      radioButtons("homogeneity_format", "Pilih Format:", 
                                   choices = list("PDF" = "pdf", "Word" = "docx"), 
                                   inline = TRUE)
               ),
               column(4,
                      downloadButton("download_homogeneity_result", "Unduh", class = "btn-primary")
               )
             )
           )
    )
  ),
  
  # Box panduan interpretasi
  box(
    title = "Panduan Interpretasi Hasil",
    status = "warning",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    collapsed = TRUE,
    
    fluidRow(
      column(6,
             h4("Uji Normalitas (Shapiro-Wilk)", style = "color: #2196f3;"),
             tags$ul(
               tags$li(strong("H0:"), " Data berdistribusi normal"),
               tags$li(strong("H1:"), " Data tidak berdistribusi normal"),
               tags$li(strong("Keputusan:"), " Jika p-value > 0.05, data berdistribusi normal"),
               tags$li(strong("Q-Q Plot:"), " Titik-titik mengikuti garis lurus = normal")
             )
      ),
      column(6,
             h4("Uji Homogenitas (Levene)", style = "color: #ff9800;"),
             tags$ul(
               tags$li(strong("H0:"), " Variansi semua grup sama (homogen)"),
               tags$li(strong("H1:"), " Minimal ada satu grup dengan variansi berbeda"),
               tags$li(strong("Keputusan:"), " Jika p-value > 0.05, variansi homogen"),
               tags$li(strong("Syarat:"), " Minimal 2 grup, setiap grup minimal 2 observasi")
             )
      )
    ),
    
    hr(),
    
    div(
      style = "background: #fff3cd; padding: 15px; border-radius: 5px; border-left: 4px solid #ffc107;",
      h5("Tips Penggunaan:", style = "color: #856404; margin-top: 0;"),
      tags$ul(
        style = "color: #856404;",
        tags$li("Pastikan variabel numerik untuk uji normalitas"),
        tags$li("Untuk uji homogenitas, pilih variabel grup yang memiliki 2+ kategori"),
        tags$li("Jika tidak ada variabel grup yang valid, buat kategorisasi di halaman Manajemen Data"),
        tags$li("Kedua uji ini penting sebagai prasyarat untuk analisis parametrik")
      )
    )
  )
)