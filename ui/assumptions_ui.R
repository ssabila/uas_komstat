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
  )
)