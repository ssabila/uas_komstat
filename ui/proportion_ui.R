# ui/proportion_ui.R
tagList(
  box(
    title = "Uji Proporsi",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Uji ini digunakan untuk menganalisis proporsi (persentase) dari sebuah kategori spesifik dalam data Anda. Contoh: menguji apakah proporsi penduduk dengan pendidikan rendah lebih dari 20%.")
  ),
  fluidRow(
    #   Kolom Kiri: Pengaturan Uji  
    column(4,
           box(
             title = "Pengaturan Uji",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             selectInput("prop_test_type", "Pilih Jenis Uji:",
                         choices = list("Satu Proporsi (One-Proportion)" = "one_prop",
                                        "Dua Proporsi (Two-Proportion)" = "two_prop")),
             uiOutput("prop_variable_selector"),
             helpText("Pilih variabel kategorik yang ingin Anda analisis."),
             
             uiOutput("prop_success_selector"),
             helpText("Pilih satu kategori dari variabel di atas yang proporsinya ingin diuji."),
             
             conditionalPanel(
               condition = "input.prop_test_type == 'one_prop'",
               numericInput("prop_p", "Proporsi Hipotesis (p₀):", value = 0.5, min = 0, max = 1, step = 0.01),
               helpText("Nilai proporsi yang akan dijadikan pembanding (contoh: 0.5 untuk 50%).")
             ),
             conditionalPanel(
               condition = "input.prop_test_type == 'two_prop'",
               uiOutput("prop_group_selector"),
               helpText("Pilih variabel dengan dua kategori untuk membandingkan proporsi.")
             ),
             actionButton("run_prop_test", "Jalankan Uji Proporsi", class = "btn-success", icon = icon("play"))
           )
    ),
    #   Kolom Kanan: Ringkasan Hasil Uji  
    column(8,
           box(
             title = "Hasil Analisis Uji Proporsi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             verbatimTextOutput("prop_test_result_summary")
           )
    )
  ),
  #   Baris Baru untuk Interpretasi dan Unduhan  
  fluidRow(
    column(12,
           box(
             title = "Interpretasi dan Laporan",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             # Bagian Interpretasi
             h4("Interpretasi Hasil Analisis"),
             verbatimTextOutput("prop_test_interpretation"),
             hr(),
             # Bagian Unduh
             h4("Unduh Laporan Lengkap"),
             p("Unduh ringkasan hasil beserta interpretasinya dalam format PDF atau Word."),
             radioButtons("proportion_format", "Pilih Format:", choices = list("PDF" = "pdf", "Word" = "docx"), inline = TRUE),
             downloadButton("download_proportion_result", "Unduh Laporan", icon = icon("download"))
           )
    )
  )
)