# ui/ttest_ui.R
tagList(
  #   Kotak Judul Halaman  
  box(
    title = "Uji Beda Rata-Rata (t-Test)",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Gunakan halaman ini untuk membandingkan rata-rata (mean) dari satu atau dua kelompok. Pastikan asumsi normalitas data terpenuhi sebelum melakukan uji ini.")
  ),
  
  #   Baris untuk Pengaturan dan Hasil Uji  
  fluidRow(
    #   Kolom Kiri: Pengaturan Uji  
    column(4,
           box(
             title = "Pengaturan Uji",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             selectInput("ttest_type", "Pilih Jenis Uji:",
                         choices = list("Satu Sampel (One-Sample)" = "one_sample",
                                        "Dua Sampel Independen (Independent)" = "two_sample")),
             uiOutput("ttest_variable_selector"),
             conditionalPanel(
               condition = "input.ttest_type == 'one_sample'",
               numericInput("ttest_mu", "Nilai Hipotesis (μ₀):", value = 0)
             ),
             conditionalPanel(
               condition = "input.ttest_type == 'two_sample'",
               uiOutput("ttest_group_selector")
             ),
             selectInput("ttest_alternative", "Hipotesis Alternatif:",
                         choices = list("Dua Sisi (Two-sided)" = "two.sided",
                                        "Kurang Dari (Less)" = "less",
                                        "Lebih Dari (Greater)" = "greater")),
             actionButton("run_ttest", "Jalankan Uji t-Test", class = "btn-success", icon = icon("play"))
           )
    ),
    
    #   Kolom Kanan: Ringkasan Hasil & Unduh  
    column(8,
           box(
             title = "Hasil Analisis t-Test",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             verbatimTextOutput("ttest_result_summary")
           )
    )
  ),
  
  #   Baris Baru: Kotak Interpretasi (Full-Width)  
  fluidRow(
    column(12,
           box(
             title = "Interpretasi Hasil",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             verbatimTextOutput("ttest_interpretation"),
             hr(),
             h4("Unduh Laporan Lengkap"),
             p("Unduh ringkasan hasil beserta interpretasinya dalam format PDF atau Word."),
             radioButtons("ttest_format", "Pilih Format:", choices = list("PDF" = "pdf", "Word" = "docx"), inline = TRUE),
             downloadButton("download_ttest_result", "Unduh Laporan", icon = icon("download"))
           )
    )
  )
)