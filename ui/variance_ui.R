# ui/variance_ui.R - Versi yang diperbaiki untuk mengatasi error tabsetPanel

tagList(
  # Header Section
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
  
  # Main Content
  fluidRow(
    # Control Panel
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
    
    # Results Panel
    column(8,
           box(
             title = "Hasil Analisis Uji Variance",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             
             # PERBAIKAN: Tambahkan parameter yang diperlukan untuk tabsetPanel
             tabsetPanel(
               id = "variance_results_tabs",  # PERBAIKAN: Tambahkan id
               type = "tabs",                 # PERBAIKAN: Tambahkan parameter type
               
               # Tab Hasil Uji
               tabPanel(
                 title = "Hasil Uji",        # PERBAIKAN: Gunakan title yang simple
                 value = "results_tab",      # PERBAIKAN: Tambahkan value
                 br(),
                 h4("Ringkasan Hasil"),
                 verbatimTextOutput("variance_test_result"),
                 
                 hr(),
                 h4("Statistik Deskriptif"),
                 DT::dataTableOutput("variance_descriptive_stats")
               ),
               
               # Tab Interpretasi
               tabPanel(
                 title = "Interpretasi",      # PERBAIKAN: Gunakan title yang simple
                 value = "interpretation_tab", # PERBAIKAN: Tambahkan value
                 br(),
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
               
               # Tab Visualisasi
               tabPanel(
                 title = "Visualisasi",       # PERBAIKAN: Gunakan title yang simple
                 value = "visualization_tab", # PERBAIKAN: Tambahkan value
                 br(),
                 h4("📊 Distribusi Data"),
                 div(
                   style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                   p("Boxplot menunjukkan distribusi dan variance antar kelompok. Histogram menunjukkan bentuk distribusi data.")
                 ),
                 
                 # Boxplot
                 div(
                   style = "margin-bottom: 20px;",
                   h5("Boxplot Perbandingan"),
                   plotOutput("variance_boxplot", height = "400px")
                 ),
                 
                 # Histogram
                 div(
                   h5("Histogram Distribusi"),
                   plotOutput("variance_histogram", height = "400px")
                 ),
                 
                 # Visualization Guide
                 div(
                   style = "background: #e8f4fd; padding: 15px; border-radius: 5px; margin-top: 15px;",
                   h5("💡 Interpretasi Visualisasi"),
                   fluidRow(
                     column(6,
                            tags$ul(
                              tags$li(strong("Boxplot lebar:"), " Variance tinggi"),
                              tags$li(strong("Boxplot sempit:"), " Variance rendah"),
                              tags$li(strong("Whiskers panjang:"), " Data tersebar")
                            )
                     ),
                     column(6,
                            tags$ul(
                              tags$li(strong("Histogram melebar:"), " Variance besar"),
                              tags$li(strong("Histogram mengerucut:"), " Variance kecil"),
                              tags$li(strong("Bentuk normal:"), " Asumsi terpenuhi")
                            )
                     )
                   )
                 )
               )
             ),
             
             # Download Section
             hr(),
             div(
               style = "background: #e9ecef; padding: 15px; border-radius: 5px;",
               h4("📥 Unduh Hasil"),
               fluidRow(
                 column(8,
                        radioButtons("variance_format", 
                                     "Pilih Format:", 
                                     choices = list("PDF" = "pdf", "Word" = "docx"), 
                                     inline = TRUE)
                 ),
                 column(4,
                        downloadButton("download_variance_result", 
                                       "📄 Unduh Hasil", 
                                       class = "btn-primary",
                                       style = "width: 100%;")
                 )
               )
             )
           )
    )
  ),
  
  # Information Section
  fluidRow(
    column(12,
           box(
             title = "📚 Informasi Tambahan",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             collapsed = TRUE,
             
             fluidRow(
               column(6,
                      div(
                        style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                        h5("🔍 Uji Chi-square (Satu Kelompok)"),
                        tags$ul(
                          tags$li("Menguji variance populasi terhadap nilai tertentu"),
                          tags$li("H₀: σ² = σ²₀"),
                          tags$li("H₁: σ² ≠ σ²₀ (two-sided)"),
                          tags$li("Statistik: χ² = (n-1)s²/σ²₀"),
                          tags$li("df = n - 1")
                        )
                      )
               ),
               column(6,
                      div(
                        style = "background: #f0f8ff; padding: 15px; border-radius: 5px;",
                        h5("📊 Uji F (Dua Kelompok)"),
                        tags$ul(
                          tags$li("Membandingkan variance dua kelompok"),
                          tags$li("H₀: σ₁² = σ₂²"),
                          tags$li("H₁: σ₁² ≠ σ₂² (two-sided)"),
                          tags$li("Statistik: F = s₁²/s₂²"),
                          tags$li("df₁ = n₁-1, df₂ = n₂-1")
                        )
                      )
               )
             ),
             
             hr(),
             
             div(
               style = "background: #fff3cd; padding: 15px; border-radius: 5px;",
               h5("⚠️ Asumsi dan Catatan Penting"),
               tags$ul(
                 tags$li(strong("Normalitas:"), " Data harus berdistribusi normal (sangat penting untuk uji variance)"),
                 tags$li(strong("Independensi:"), " Observasi harus saling independen"),
                 tags$li(strong("Sensitivitas:"), " Uji variance sangat sensitif terhadap pelanggaran asumsi normalitas"),
                 tags$li(strong("Alternative:"), " Jika data tidak normal, pertimbangkan transformasi atau uji non-parametrik"),
                 tags$li(strong("Interpretasi:"), " Variance yang berbeda menunjukkan variabilitas data yang berbeda antar kelompok")
               )
             )
           )
    )
  )
)