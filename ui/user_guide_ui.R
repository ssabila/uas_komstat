# ui/user_guide_ui.R

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
           
           # MAIN TABSETPANEL
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
                   h4("2. Pemilihan Metode Analisis"),
                   
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
                 
                 div(
                   style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 15px 0;",
                   h4("3. Flow Analisis yang Disarankan"),
                   tags$ol(
                     tags$li("Eksplorasi data (Statistik Deskriptif & Visualisasi)"),
                     tags$li("Uji asumsi data (Normalitas, Homogenitas, dll)"),
                     tags$li("Pilih dan jalankan uji statistik yang sesuai"),
                     tags$li("Interpretasi hasil dan buat laporan")
                   )
                 )
               )
             ),
             
             # Tab 2: Uji Statistik
             tabPanel(
               title = "Uji Statistik",
               value = "statistical_tests",
               br(),
               div(
                 h3("Panduan Pemilihan dan Penggunaan Uji Statistik"),
                 
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
             ),
             
             # Tab 3: PANDUAN REGRESI
             tabPanel(
               title = "Analisis Regresi",
               value = "regression_guide",
               br(),
               div(
                 h3("Panduan Lengkap Analisis Regresi Linear"),
                 
                 # Pengenalan Regresi
                 div(
                   style = "background: #d4edda; padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #28a745;",
                   h4("Apa itu Analisis Regresi?", style = "color: #155724; margin-top: 0;"),
                   p(style = "color: #155724;", "Analisis regresi adalah metode statistik untuk memahami hubungan antara variabel dependen (Y) dengan satu atau lebih variabel independen (X). Metode ini memungkinkan kita untuk:"),
                   tags$ul(
                     style = "color: #155724;",
                     tags$li(strong("Prediksi:"), " Memperkirakan nilai Y berdasarkan nilai X"),
                     tags$li(strong("Hubungan:"), " Mengukur kekuatan dan arah hubungan antar variabel"),
                     tags$li(strong("Pengaruh:"), " Menentukan variabel mana yang berpengaruh signifikan")
                   )
                 ),
                 
                 # Jenis-jenis Regresi
                 div(
                   style = "background: #fff3cd; padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #ffc107;",
                   h4(" Jenis-jenis Regresi", style = "color: #856404; margin-top: 0;"),
                   fluidRow(
                     column(6,
                            div(
                              style = "background: #fff; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                              h5("Regresi Linear Sederhana", style = "color: #856404;"),
                              p("1 variabel independen → 1 variabel dependen"),
                              p(strong("Formula:"), " Y = a + bX + e"),
                              p(strong("Contoh:"), " Kemiskinan = f(Pendidikan)")
                            )
                     ),
                     column(6,
                            div(
                              style = "background: #fff; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                              h5("Regresi Linear Berganda", style = "color: #856404;"),
                              p("2+ variabel independen → 1 variabel dependen"),
                              p(strong("Formula:"), " Y = a + b₁X₁ + b₂X₂ + ... + e"),
                              p(strong("Contoh:"), " Kemiskinan = f(Pendidikan, Kesehatan, Ekonomi)")
                            )
                     )
                   )
                 ),
                 
                 # Langkah-langkah Analisis
                 div(
                   style = "background: #e2e3e5; padding: 20px; border-radius: 8px; margin: 15px 0;",
                   h4("Langkah-langkah Analisis Regresi"),
                   div(
                     style = "background: #fff; padding: 15px; border-radius: 5px; margin: 10px 0;",
                     h5("1. Persiapan Data"),
                     tags$ul(
                       tags$li("Pastikan semua variabel numerik"),
                       tags$li("Periksa missing values dan outliers"),
                       tags$li("Lakukan eksplorasi data awal (scatter plot, korelasi)")
                     )
                   ),
                   div(
                     style = "background: #fff; padding: 15px; border-radius: 5px; margin: 10px 0;",
                     h5("2. Uji Asumsi Regresi"),
                     tags$ul(
                       tags$li(strong("Linearitas:"), " Hubungan linear antara X dan Y"),
                       tags$li(strong("Normalitas residual:"), " Residual berdistribusi normal"),
                       tags$li(strong("Homoskedastisitas:"), " Variansi residual konstan"),
                       tags$li(strong("Independensi:"), " Tidak ada autokorelasi residual"),
                       tags$li(strong("Multikolinearitas:"), " Variabel X tidak saling berkorelasi tinggi")
                     )
                   ),
                   div(
                     style = "background: #fff; padding: 15px; border-radius: 5px; margin: 10px 0;",
                     h5("3. Membangun Model"),
                     tags$ul(
                       tags$li("Pilih variabel dependen (Y)"),
                       tags$li("Pilih variabel independen (X₁, X₂, ...)"),
                       tags$li("Jalankan analisis regresi"),
                       tags$li("Evaluasi kualitas model (R², F-test)")
                     )
                   ),
                   div(
                     style = "background: #fff; padding: 15px; border-radius: 5px; margin: 10px 0;",
                     h5("4. Interpretasi Hasil"),
                     tags$ul(
                       tags$li("Analisis koefisien regresi dan signifikansinya"),
                       tags$li("Evaluasi kualitas model secara keseluruhan"),
                       tags$li("Uji asumsi residual"),
                       tags$li("Buat prediksi jika diperlukan")
                     )
                   )
                 ),
                 
                 # Cara Membaca Output
                 div(
                   style = "background: #d1ecf1; padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #bee5eb;",
                   h4("Cara Membaca Output Regresi", style = "color: #0c5460; margin-top: 0;"),
                   
                   fluidRow(
                     column(6,
                            div(
                              style = "background: #fff; padding: 15px; border-radius: 5px;",
                              h5("Koefisien (Estimates)", style = "color: #0c5460;"),
                              tags$ul(
                                tags$li(strong("Intercept (a):"), " Nilai Y ketika semua X = 0"),
                                tags$li(strong("Slope (b):"), " Perubahan Y per unit perubahan X"),
                                tags$li(strong("P-value < 0.05:"), " Koefisien signifikan")
                              )
                            )
                     ),
                     column(6,
                            div(
                              style = "background: #fff; padding: 15px; border-radius: 5px;",
                              h5("Kualitas Model", style = "color: #0c5460;"),
                              tags$ul(
                                tags$li(strong("R-squared:"), " % variasi Y yang dijelaskan model"),
                                tags$li(strong("Adjusted R²:"), " R² yang disesuaikan jumlah variabel"),
                                tags$li(strong("F-statistic:"), " Signifikansi model secara keseluruhan")
                              )
                            )
                     )
                   )
                 ),
                 
                 # Contoh Praktis
                 div(
                   style = "background: #f8d7da; padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #dc3545;",
                   h4("Contoh Praktis: Analisis Kerentanan Sosial", style = "color: #721c24; margin-top: 0;"),
                   div(
                     style = "background: #fff; padding: 15px; border-radius: 5px; margin: 10px 0;",
                     h5("Penelitian Question:", style = "color: #721c24;"),
                     p("Faktor-faktor apa yang mempengaruhi tingkat kerentanan sosial di Indonesia?")
                   ),
                   div(
                     style = "background: #fff; padding: 15px; border-radius: 5px; margin: 10px 0;",
                     h5("Model yang Diusulkan:", style = "color: #721c24;"),
                     p(strong("Y (Dependen):"), " Indeks Kerentanan Sosial"),
                     p(strong("X (Independen):"), " Tingkat Kemiskinan, Tingkat Pendidikan, Akses Kesehatan, Infrastruktur")
                   ),
                   div(
                     style = "background: #fff; padding: 15px; border-radius: 5px; margin: 10px 0;",
                     h5("Interpretasi Hasil:", style = "color: #721c24;"),
                     tags$ul(
                       tags$li("Jika koefisien Kemiskinan = 0.8 (p < 0.001): Setiap kenaikan 1% kemiskinan meningkatkan kerentanan sosial sebesar 0.8 poin"),
                       tags$li("Jika R² = 0.75: Model menjelaskan 75% variasi kerentanan sosial"),
                       tags$li("Jika F-test signifikan: Model secara keseluruhan bermakna")
                     )
                   )
                 ),
                 
                 # Tips dan Peringatan
                 div(
                   style = "background: #e7f3ff; padding: 20px; border-radius: 8px; margin: 15px 0;",
                   h4("⚠️ Tips dan Peringatan Penting"),
                   fluidRow(
                     column(6,
                            div(
                              style = "background: #fff; padding: 15px; border-radius: 5px; border-left: 3px solid #28a745;",
                              h5("✅ Best Practices", style = "color: #28a745;"),
                              tags$ul(
                                tags$li("Selalu periksa asumsi sebelum interpretasi"),
                                tags$li("Gunakan scatter plot untuk eksplorasi awal"),
                                tags$li("Pertimbangkan transformasi data jika asumsi dilanggar"),
                                tags$li("Validasi model dengan data terpisah jika memungkinkan")
                              )
                            )
                     ),
                     column(6,
                            div(
                              style = "background: #fff; padding: 15px; border-radius: 5px; border-left: 3px solid #dc3545;",
                              h5("❌ Common Mistakes", style = "color: #dc3545;"),
                              tags$ul(
                                tags$li("Mengabaikan uji asumsi"),
                                tags$li("Menginterpretasi korelasi sebagai kausalitas"),
                                tags$li("Menggunakan terlalu banyak variabel (overfitting)"),
                                tags$li("Tidak mempertimbangkan outliers")
                              )
                            )
                     )
                   )
                 ),
                 
                 # Panduan Pelaporan
                 div(
                   style = "background: #e8f4fd; padding: 20px; border-radius: 8px; margin: 15px 0;",
                   h4("Panduan Pelaporan Hasil Regresi"),
                   tags$ol(
                     tags$li(strong("Deskripsi Model:"), " Jelaskan variabel yang digunakan dan tujuan analisis"),
                     tags$li(strong("Uji Asumsi:"), " Laporkan hasil pemeriksaan asumsi regresi"),
                     tags$li(strong("Kualitas Model:"), " Sebutkan R², Adjusted R², dan F-test"),
                     tags$li(strong("Koefisien Signifikan:"), " Interpretasikan variabel yang berpengaruh signifikan"),
                     tags$li(strong("Kesimpulan:"), " Rangkum temuan utama dan implikasi praktis"),
                     tags$li(strong("Keterbatasan:"), " Diskusikan batasan model dan saran penelitian lanjutan")
                   )
                 )
               )
             )
           )
         )
  )
)