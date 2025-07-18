tagList(
  box(
    title = "Uji Variance",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Uji ini digunakan untuk menguji variance (ragam) dari satu populasi atau membandingkan variance dua populasi."),
    div(
      style = "background: #e3f2fd; padding: 10px; border-radius: 5px; border-left: 4px solid #2196f3;",
      icon("info-circle", style = "color: #2196f3;"),
      strong(" Catatan:"),
      tags$ul(
        tags$li("Uji 1 kelompok: Chi-square test untuk variance populasi"),
        tags$li("Uji 2 kelompok: F-test untuk perbandingan variance"),
        tags$li("Data harus berdistribusi normal")
      )
    )
  ),
  
  fluidRow(
    column(4,
           box(
             title = "Pengaturan Uji",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             
             # Pilih jenis uji
             selectInput("variance_test_type", 
                         "Pilih Jenis Uji:",
                         choices = list(
                           "Satu Kelompok (Chi-square)" = "one_sample",
                           "Dua Kelompok (F-test)" = "two_sample"
                         )),
             
             # Pilih variabel numerik
             uiOutput("variance_variable_selector"),
             
             # Input untuk uji satu kelompok
             conditionalPanel(
               condition = "input.variance_test_type == 'one_sample'",
               numericInput("variance_sigma2", 
                            "Variance Hipotesis (σ²₀):", 
                            value = 1, 
                            min = 0.001, 
                            step = 0.1),
               helpText("Nilai variance populasi yang akan diuji")
             ),
             
             # Input untuk uji dua kelompok
             conditionalPanel(
               condition = "input.variance_test_type == 'two_sample'",
               uiOutput("variance_group_selector"),
               helpText("Pilih variabel dengan tepat 2 kategori")
             ),
             
             # Pilih hipotesis alternatif
             selectInput("variance_alternative", 
                         "Hipotesis Alternatif:",
                         choices = list(
                           "Dua Sisi (Two-sided)" = "two.sided",
                           "Lebih Besar (Greater)" = "greater", 
                           "Lebih Kecil (Less)" = "less"
                         )),
             
             # Tingkat signifikansi
             numericInput("variance_alpha", 
                          "Tingkat Signifikansi (α):", 
                          value = 0.05, 
                          min = 0.001, 
                          max = 0.5, 
                          step = 0.01),
             
             br(),
             actionButton("run_variance_test", 
                          "Jalankan Uji Variance", 
                          class = "btn-success", 
                          icon = icon("play"))
           )
    ),
    
    column(8,
           box(
             title = "Hasil Analisis Uji Variance",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             
             tabsetPanel(
               tabPanel("Hasil Uji",
                        h4("Ringkasan Hasil"),
                        verbatimTextOutput("variance_test_result"),
                        
                        hr(),
                        h4("Statistik Deskriptif"),
                        DT::dataTableOutput("variance_descriptive_stats")
               ),
               
               tabPanel("Interpretasi",
                        h4("Interpretasi Hasil"),
                        verbatimTextOutput("variance_interpretation"),
                        
                        hr(),
                        h4("Panduan Interpretasi"),
                        div(
                          style = "background: #fff3cd; padding: 15px; border-radius: 5px;",
                          tags$ul(
                            tags$li(strong("Uji Satu Kelompok:"), " Menguji apakah variance populasi sama dengan nilai tertentu"),
                            tags$li(strong("Uji Dua Kelompok:"), " Menguji apakah variance kedua kelompok sama"),
                            tags$li(strong("P-value < α:"), " Tolak H₀ (ada perbedaan signifikan)"),
                            tags$li(strong("P-value ≥ α:"), " Gagal tolak H₀ (tidak ada perbedaan signifikan)")
                          )
                        )
               ),
               
               tabPanel("Visualisasi",
                        plotOutput("variance_boxplot", height = "400px"),
                        plotOutput("variance_histogram", height = "400px")
               )
             ),
             
             hr(),
             h4("Unduh Hasil"),
             fluidRow(
               column(8,
                      radioButtons("variance_format", 
                                   "Pilih Format:", 
                                   choices = list("PDF" = "pdf", "Word" = "docx"), 
                                   inline = TRUE)
               ),
               column(4,
                      downloadButton("download_variance_result", 
                                     "Unduh Hasil", 
                                     class = "btn-primary")
               )
             )
           )
    )
  )
)