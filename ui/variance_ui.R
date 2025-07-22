# ui/variance_ui.R

tagList(
  # Header Section
  box(
    title = "Uji Varians",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Uji ini digunakan untuk menguji varians (ragam) dari satu populasi atau membandingkan varians dua populasi."),
    div(
      style = "background: #e3f2fd; padding: 10px; border-radius: 5px; border-left: 4px solid #2196f3;",
      icon("info-circle", style = "color: #2196f3;"),
      strong(" Catatan:"),
      tags$ul(
        tags$li("Uji 1 kelompok menggunakan Chi-square test, sedangkan uji 2 kelompok menggunakan F-test."),
        tags$li("Asumsi utama untuk uji ini adalah data harus berdistribusi normal.")
      )
    )
  ),
  
  # Baris untuk Pengaturan dan Hasil Uji Statistik
  fluidRow(
    # Kolom Kiri: Pengaturan Uji
    column(4,
           box(
             title = "Pengaturan Uji",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             selectInput("variance_test_type", "Pilih Jenis Uji:",
                         choices = list("Satu Kelompok (Chi-square)" = "one_sample", "Dua Kelompok (F-test)" = "two_sample")),
             uiOutput("variance_variable_selector"),
             conditionalPanel(
               condition = "input.variance_test_type == 'one_sample'",
               numericInput("variance_sigma2", "Varians Hipotesis (σ²₀):", value = 1, min = 0.001, step = 0.1)
             ),
             conditionalPanel(
               condition = "input.variance_test_type == 'two_sample'",
               uiOutput("variance_group_selector")
             ),
             selectInput("variance_alternative", "Hipotesis Alternatif:",
                         choices = list("Dua Sisi (Two-sided)" = "two.sided", "Lebih Besar (Greater)" = "greater", "Lebih Kecil (Less)" = "less")),
             numericInput("variance_alpha", "Tingkat Signifikansi (α):", value = 0.05, min = 0.001, max = 0.5, step = 0.01),
             br(),
             actionButton("run_variance_test", "Jalankan Uji Variance", class = "btn-success", icon = icon("play"))
           )
    ),
    
    # Kolom Kanan: Hasil Uji Statistik
    column(8,
           box(
             title = "Hasil Analisis Uji Varians",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             # Hanya menampilkan hasil uji dan statistik deskriptif di sini
             h4("Ringkasan Hasil Uji Statistik"),
             verbatimTextOutput("variance_test_result"),
             hr(),
             h4("Statistik Deskriptif"),
             DT::dataTableOutput("variance_descriptive_stats")
           )
    )
  ),
  
  # Baris BARU untuk Interpretasi, Visualisasi, dan Unduhan (Full-Width)
  fluidRow(
    column(12,
           box(
             title = "Interpretasi, Visualisasi, dan Laporan",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             tabsetPanel(
               id = "variance_details_tabs",
               type = "tabs",
               
               # Tab Interpretasi Statistik
               tabPanel(
                 title = "Interpretasi Uji", value = "interpretation_tab", br(),
                 verbatimTextOutput("variance_interpretation")
               ),
               
               # Tab Visualisasi dan Interpretasi Visual
               tabPanel(
                 title = "Visualisasi", value = "visualization_tab", br(),
                 h4("Distribusi Data"),
                 plotOutput("variance_boxplot", height = "350px"),
                 hr(),
                 plotOutput("variance_histogram", height = "350px"),
                 hr(),
                 h4("Interpretasi Visual"),
                 verbatimTextOutput("variance_visualization_interpretation")
               )
             ),
             
             # Bagian Unduh dipindahkan ke sini
             hr(),
             div(
               style = "background: #e9ecef; padding: 15px; border-radius: 5px;",
               h4("📥 Unduh Laporan Lengkap"),
               p("Unduh semua hasil analisis (teks dan gambar) dalam satu dokumen."),
               fluidRow(
                 column(8, radioButtons("variance_format", "Pilih Format:", choices = list("PDF" = "pdf", "Word" = "docx"), inline = TRUE)),
                 column(4, downloadButton("download_variance_result", "📄 Unduh Laporan", class = "btn-primary", style = "width: 100%;"))
               )
             )
           )
    )
  )
)