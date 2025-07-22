# ui/user_guide_ui.R - FINAL FIX untuk mengatasi semua error tabsetPanel

fluidRow(
  # Header utama
  column(12,
         box(
           title = "Panduan Penggunaan Dashboard Kerentanan Sosial Indonesia", 
           status = "primary", 
           solidHeader = TRUE,
           width = 12,
           div(
             style = "text-align: center; padding: 20px;",
             h3("Petunjuk Lengkap Menggunakan Dashboard", style = "color: #3c8dbc;"),
             p("Panduan komprehensif untuk memanfaatkan semua fitur analisis data kerentanan sosial", 
               style = "font-size: 16px; margin-top: 15px;"),
             div(
               style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin-top: 15px;",
               h4("Tujuan Dashboard", style = "color: #155724; margin-top: 0;"),
               p(style = "color: #155724;", "Dashboard ini dirancang untuk membantu peneliti, akademisi, dan praktisi dalam menganalisis data kerentanan sosial Indonesia dengan metodologi statistik yang tepat.")
             )
           )
         )
  ),
  
  # Navigasi cepat
  column(12,
         box(
           title = "Navigasi Cepat", 
           status = "info", 
           solidHeader = TRUE,
           width = 12,
           collapsible = TRUE,
           fluidRow(
             column(2,
                    div(style = "text-align: center; padding: 10px;",
                        actionButton("guide_nav_data", "Data", 
                                     class = "btn-primary btn-sm",
                                     style = "width: 100%; margin-bottom: 5px;"),
                        p("Kelola Data", style = "font-size: 12px; margin: 0;")
                    )
             ),
             column(2,
                    div(style = "text-align: center; padding: 10px;",
                        actionButton("guide_nav_explore", "Eksplorasi", 
                                     class = "btn-success btn-sm",
                                     style = "width: 100%; margin-bottom: 5px;"),
                        p("Jelajahi Data", style = "font-size: 12px; margin: 0;")
                    )
             ),
             column(2,
                    div(style = "text-align: center; padding: 10px;",
                        actionButton("guide_nav_assumptions", "Asumsi", 
                                     class = "btn-warning btn-sm",
                                     style = "width: 100%; margin-bottom: 5px;"),
                        p("Uji Prasyarat", style = "font-size: 12px; margin: 0;")
                    )
             ),
             column(2,
                    div(style = "text-align: center; padding: 10px;",
                        actionButton("guide_nav_inference", "Inferensia", 
                                     class = "btn-info btn-sm",
                                     style = "width: 100%; margin-bottom: 5px;"),
                        p("Uji Hipotesis", style = "font-size: 12px; margin: 0;")
                    )
             ),
             column(2,
                    div(style = "text-align: center; padding: 10px;",
                        actionButton("guide_nav_regression", "Regresi", 
                                     class = "btn-secondary btn-sm",
                                     style = "width: 100%; margin-bottom: 5px;"),
                        p("Model Prediksi", style = "font-size: 12px; margin: 0;")
                    )
             ),
             column(2,
                    div(style = "text-align: center; padding: 10px;",
                        actionButton("guide_nav_download", "Download", 
                                     class = "btn-success btn-sm",
                                     style = "width: 100%; margin-bottom: 5px;"),
                        p("Ekspor Hasil", style = "font-size: 12px; margin: 0;")
                    )
             )
           )
         )
  ),
  
  # Konten panduan utama
  column(12,
         box(
           title = "Panduan Penggunaan Dashboard", 
           status = "primary", 
           solidHeader = TRUE,
           width = 12,
           
           # MAIN TABSETPANEL - FIXED
           tabsetPanel(
             id = "guide_main_tabs",
             type = "tabs",
             
             # Tab 1: Persiapan
             tabPanel(
               title = "Persiapan",
               value = "getting_started",
               br(),
               div(
                 h3("Langkah Awal Menggunakan Dashboard"),
                 
                 div(
                   style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 15px 0;",
                   h4("1. Persiapan Data"),
                   tags$ul(
                     tags$li("Pastikan data SOVI sudah dimuat dengan benar"),
                     tags$li("Periksa kelengkapan variabel yang diperlukan"),
                     tags$li("Identifikasi missing values dan outliers")
                   )
                 ),
                 
                 div(
                   style = "background: #e3f2fd; padding: 20px; border-radius: 8px; margin: 15px 0;",
                   h4("2. Eksplorasi Awal"),
                   tags$ul(
                     tags$li("Mulai dengan statistik deskriptif"),
                     tags$li("Buat visualisasi untuk memahami distribusi data"),
                     tags$li("Identifikasi pola geografis melalui peta")
                   )
                 ),
                 
                 div(
                   style = "background: #fff3cd; padding: 20px; border-radius: 8px; margin: 15px 0;",
                   h4("3. Analisis Lanjutan"),
                   tags$ul(
                     tags$li("Pilih uji statistik yang sesuai dengan tujuan penelitian"),
                     tags$li("Interpretasikan hasil dengan bantuan panduan yang disediakan"),
                     tags$li("Unduh laporan untuk dokumentasi")
                   )
                 )
               )
             ),
             
             # Tab 2: Manajemen Data
             tabPanel(
               title = "Manajemen Data",
               value = "data_management",
               br(),
               div(
                 h3("Kelola dan Transformasi Data"),
                 
                 div(
                   style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #28a745;",
                   h4("Transformasi Data"),
                   tags$ul(
                     tags$li(strong("Logaritma (log):"), " Mengurangi skewness, cocok untuk data dengan distribusi miring kanan"),
                     tags$li(strong("Akar kuadrat (sqrt):"), " Mengurangi heteroskedastisitas, untuk data dengan variansi tidak konstan"),
                     tags$li(strong("Standardisasi (z-score):"), " Mengubah data ke mean=0, sd=1, untuk perbandingan skala"),
                     tags$li(strong("Normalisasi (0-1):"), " Mengubah range data ke 0-1, untuk algoritma sensitif skala")
                   )
                 )
               )
             ),
             
             # Tab 3: Uji Asumsi
             tabPanel(
               title = "Uji Asumsi",
               value = "assumptions",
               br(),
               div(
                 h3("Verifikasi Prasyarat Analisis Statistik"),
                 
                 div(
                   style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #dc3545;",
                   h4("Mengapa Uji Asumsi Penting?"),
                   p("Setiap uji statistik memiliki asumsi yang harus dipenuhi. Melanggar asumsi dapat menghasilkan kesimpulan yang salah dan menyesatkan."),
                   
                   tags$ul(
                     tags$li(strong("Normalitas:"), " Data mengikuti distribusi normal"),
                     tags$li(strong("Homoskedastisitas:"), " Variansi konstan"),
                     tags$li(strong("Independensi:"), " Observasi saling independen"),
                     tags$li(strong("Linearitas:"), " Hubungan linear antar variabel")
                   )
                 ),
                 
                 div(
                   style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                   h4("Checklist Uji Asumsi"),
                   tags$ol(
                     tags$li("Periksa normalitas untuk setiap variabel numerik"),
                     tags$li("Uji homogenitas jika akan membandingkan grup"),
                     tags$li("Lihat Q-Q plot untuk validasi visual normalitas"),
                     tags$li("Dokumentasikan hasil uji asumsi"),
                     tags$li("Pilih metode analisis berdasarkan hasil uji asumsi")
                   )
                 )
               )
             ),
             
             # Tab 4: Statistik Inferensia
             tabPanel(
               title = "Statistik Inferensia",
               value = "inference",
               br(),
               div(
                 h3("Uji Hipotesis dan Analisis Inferensia"),
                 
                 div(
                   style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin: 15px 0;",
                   h4("Memilih Uji yang Tepat"),
                   p("Pemilihan uji statistik bergantung pada tujuan penelitian, jenis data, dan jumlah grup yang dibandingkan."),
                   
                   fluidRow(
                     column(6,
                            strong("Berdasarkan Tujuan:"),
                            tags$ul(
                              tags$li("Bandingkan rata-rata → t-test/ANOVA"),
                              tags$li("Bandingkan proporsi → Uji Proporsi"),
                              tags$li("Bandingkan variansi → Uji Variance"),
                              tags$li("Prediksi variabel → Regresi")
                            )
                     ),
                     column(6,
                            strong("Berdasarkan Jumlah Grup:"),
                            tags$ul(
                              tags$li("1 grup → One-sample tests"),
                              tags$li("2 grup → Two-sample tests"),
                              tags$li("3+ grup → ANOVA"),
                              tags$li("Multiple variables → Regresi")
                            )
                     )
                   )
                 ),
                 
                 # FIXED: Remove problematic nested tabsetPanel
                 # Instead, use simple divs with headers
                 div(
                   style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 15px 0;",
                   h4("t-Test"),
                   tags$ul(
                     tags$li(strong("One-sample t-test:"), " Membandingkan rata-rata sampel dengan nilai tertentu"),
                     tags$li(strong("Two-sample t-test:"), " Membandingkan rata-rata dua grup independen"),
                     tags$li(strong("Asumsi:"), " Normalitas data, untuk dua sampel perlu homogenitas variansi")
                   ),
                   strong("Contoh penggunaan:"),
                   tags$ul(
                     tags$li("Apakah rata-rata kemiskinan berbeda dari 15%?"),
                     tags$li("Apakah kemiskinan di Jawa berbeda dengan Sumatera?")
                   )
                 ),
                 
                 div(
                   style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                   h4("ANOVA"),
                   tags$ul(
                     tags$li(strong("One-way ANOVA:"), " Membandingkan rata-rata 3+ grup dengan 1 faktor"),
                     tags$li(strong("Two-way ANOVA:"), " Membandingkan rata-rata dengan 2 faktor"),
                     tags$li(strong("Asumsi:"), " Normalitas, homogenitas variansi, independensi")
                   ),
                   strong("Contoh penggunaan:"),
                   tags$ul(
                     tags$li("Apakah ada perbedaan kemiskinan antar pulau (Jawa, Sumatera, Kalimantan)?"),
                     tags$li("Pengaruh wilayah dan tipe daerah terhadap kerentanan sosial?")
                   )
                 )
               )
             )
           )
         )
  )
)