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
                                     class = "btn-purple btn-sm",
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
           
           tabsetPanel(
             id = "guide_tabs",
             
             # Tab 1: Memulai
             tabPanel("Persiapan",
                      value = "getting_started",
                      br(),
                      div(
                        h3("Langkah Awal Menggunakan Dashboard"),
                        
                        div(
                          style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin: 15px 0;",
                          h4("2. Pahami Data Anda"),
                          tags$ul(
                            tags$li("Kunjungi tab ", strong("Beranda"), " untuk melihat ringkasan dataset"),
                            tags$li("Data berisi 511 kabupaten/kota di Indonesia dengan 16 indikator kerentanan sosial"),
                            tags$li("Setiap variabel sudah dikategorisasi secara geografis (provinsi, pulau, wilayah)")
                          )
                        ),
                        
                        div(
                          style = "background: #e3f2fd; padding: 20px; border-radius: 8px; margin: 15px 0;",
                          h4("2. Mulai dari Eksplorasi Data"),
                          tags$ul(
                            tags$li("Gunakan ", strong("Statistik Deskriptif"), " untuk memahami karakteristik dasar data"),
                            tags$li("Buat ", strong("Visualisasi"), " untuk melihat pola distribusi"),
                            tags$li("Eksplorasi ", strong("Peta Distribusi"), " untuk analisis geografis")
                          )
                        ),
                        
                        div(
                          style = "background: #fff3cd; padding: 20px; border-radius: 8px; margin: 15px 0;",
                          h4("4. Persiapan Analisis"),
                          tags$ul(
                            tags$li("Lakukan ", strong("Uji Asumsi"), " sebelum analisis inferensia"),
                            tags$li("Gunakan ", strong("Manajemen Data"), " jika perlu transformasi"),
                            tags$li("Pastikan data memenuhi syarat untuk analisis yang dipilih")
                          )
                        ),
                        
                        div(
                          style = "background: #d4edda; padding: 20px; border-radius: 8px; margin: 15px 0;",
                          h4("5. Analisis Lanjutan"),
                          tags$ul(
                            tags$li("Pilih uji statistik yang sesuai dengan tujuan penelitian"),
                            tags$li("Interpretasikan hasil dengan bantuan panduan yang disediakan"),
                            tags$li("Unduh laporan untuk dokumentasi")
                          )
                        )
                      )
             ),
             
             # Tab 2: Manajemen Data
             tabPanel("Manajemen Data",
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
                          ),
                          div(
                            style = "background: #fff; padding: 10px; border-radius: 4px; margin-top: 10px;",
                            strong("Tips:"), " Gunakan transformasi log untuk data kemiskinan atau populasi yang cenderung skew"
                          )
                        ),
                        
                        div(
                          style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #2196f3;",
                          h4("Kategorisasi"),
                          tags$ul(
                            tags$li(strong("Kuantil:"), " Membagi data berdasarkan persentil (distribusi seimbang)"),
                            tags$li(strong("Interval sama:"), " Membagi data berdasarkan range yang sama"),
                            tags$li(strong("Custom:"), " Tentukan breakpoint sendiri berdasarkan domain knowledge")
                          ),
                          div(
                            style = "background: #fff; padding: 10px; border-radius: 4px; margin-top: 10px;",
                            strong("Tips:"), " Untuk analisis kebijakan, gunakan kategorisasi berdasarkan standar nasional"
                          )
                        ),
                        
                        div(
                          style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #ffc107;",
                          h4("Deteksi Outlier"),
                          tags$ul(
                            tags$li(strong("IQR Method:"), " Menggunakan Interquartile Range (standar: 1.5 × IQR)"),
                            tags$li(strong("Z-Score:"), " Berdasarkan standar deviasi (standar: |z| > 3)"),
                            tags$li(strong("Modified Z-Score:"), " Robust terhadap outlier ekstrem")
                          ),
                          div(
                            style = "background: #fff; padding: 10px; border-radius: 4px; margin-top: 10px;",
                            strong("Perhatian:"), " Jangan langsung hapus outlier. Investigasi dulu apakah data valid"
                          )
                        ),
                        
                        div(
                          style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #dc3545;",
                          h4("Best Practices"),
                          tags$ul(
                            tags$li("Selalu simpan backup data asli sebelum transformasi"),
                            tags$li("Dokumentasikan setiap langkah transformasi"),
                            tags$li("Validasi hasil transformasi dengan domain expert"),
                            tags$li("Uji asumsi setelah transformasi")
                          )
                        )
                      )
             ),
             
             # Tab 3: Eksplorasi Data
             tabPanel("Eksplorasi Data",
                      value = "data_exploration",
                      br(),
                      div(
                        h3("Jelajahi dan Visualisasikan Data"),
                        
                        fluidRow(
                          column(6,
                                 div(
                                   style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin: 15px 0;",
                                   h4("Statistik Deskriptif"),
                                   tags$ul(
                                     tags$li(strong("Mean vs Median:"), " Bandingkan untuk deteksi skewness"),
                                     tags$li(strong("Standar Deviasi:"), " Ukur variabilitas data"),
                                     tags$li(strong("Min/Max:"), " Identifikasi range dan outlier potensial"),
                                     tags$li(strong("Quartiles:"), " Pahami distribusi data")
                                   ),
                                   div(
                                     style = "background: #fff; padding: 10px; border-radius: 4px; margin-top: 10px;",
                                     strong("Interpretasi:"), br(),
                                     "• Mean > Median: Distribusi miring kanan", br(),
                                     "• Mean < Median: Distribusi miring kiri", br(),
                                     "• Mean ≈ Median: Distribusi simetris"
                                   )
                                 )
                          ),
                          column(6,
                                 div(
                                   style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                                   h4("Visualisasi"),
                                   tags$ul(
                                     tags$li(strong("Histogram:"), " Lihat bentuk distribusi"),
                                     tags$li(strong("Boxplot:"), " Identifikasi outlier dan quartiles"),
                                     tags$li(strong("Scatter Plot:"), " Eksplorasi hubungan antar variabel"),
                                     tags$li(strong("Correlation Heatmap:"), " Peta korelasi lengkap")
                                   ),
                                   div(
                                     style = "background: #fff; padding: 10px; border-radius: 4px; margin-top: 10px;",
                                     strong("Tips Visualisasi:"), br(),
                                     "• Gunakan warna yang kontras", br(),
                                     "• Beri judul dan label yang jelas", br(),
                                     "• Sesuaikan bins histogram dengan data"
                                   )
                                 )
                          )
                        ),
                        
                        div(
                          style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          h4("Peta Distribusi Geografis"),
                          tags$ul(
                            tags$li(strong("Peta Interaktif:"), " Klik pada kabupaten/kota untuk detail statistik"),
                            tags$li(strong("Color Coding:"), " Merah = Tinggi, Kuning = Rendah"),
                            tags$li(strong("Persentil Analysis:"), " Posisi relatif terhadap daerah lain"),
                            tags$li(strong("Regional Comparison:"), " Bandingkan antar pulau/wilayah")
                          ),
                          div(
                            style = "background: #fff; padding: 10px; border-radius: 4px; margin-top: 10px;",
                            strong("Manfaat:"), " Identifikasi hotspot kerentanan, pola geografis, dan target intervensi"
                          )
                        )
                      )
             ),
             
             # Tab 4: Uji Asumsi
             tabPanel("Uji Asumsi",
                      value = "assumptions",
                      br(),
                      div(
                        h3("Verifikasi Prasyarat Analisis Statistik"),
                        
                        div(
                          style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #dc3545;",
                          h4("Mengapa Uji Asumsi Penting?"),
                          p("Setiap uji statistik memiliki asumsi yang harus dipenuhi. Melanggar asumsi dapat menghasilkan kesimpulan yang salah dan menyesatkan."),
                          tags$ul(
                            tags$li("Validitas hasil analisis bergantung pada terpenuhinya asumsi"),
                            tags$li("Pelanggaran asumsi dapat menyebabkan bias dan kesalahan inferensi"),
                            tags$li("Alternatif non-parametrik tersedia jika asumsi tidak terpenuhi")
                          )
                        ),
                        
                        fluidRow(
                          column(6,
                                 div(
                                   style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin: 15px 0;",
                                   h4("Uji Normalitas (Shapiro-Wilk)"),
                                   strong("Kapan digunakan:"),
                                   tags$ul(
                                     tags$li("Sebelum t-test"),
                                     tags$li("Sebelum ANOVA"),
                                     tags$li("Untuk regresi linear")
                                   ),
                                   strong("Interpretasi:"),
                                   tags$ul(
                                     tags$li("H₀: Data berdistribusi normal"),
                                     tags$li("p > 0.05: Data normal"),
                                     tags$li("p ≤ 0.05: Data tidak normal")
                                   ),
                                   div(
                                     style = "background: #fff; padding: 10px; border-radius: 4px; margin-top: 10px;",
                                     strong("Solusi jika tidak normal:"), br(),
                                     "• Transformasi data (log, sqrt)", br(),
                                     "• Gunakan uji non-parametrik", br(),
                                     "• Increase sample size (CLT)"
                                   )
                                 )
                          ),
                          column(6,
                                 div(
                                   style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                                   h4("Uji Homogenitas (Levene)"),
                                   strong("Kapan digunakan:"),
                                   tags$ul(
                                     tags$li("Sebelum independent t-test"),
                                     tags$li("Sebelum ANOVA"),
                                     tags$li("Membandingkan variansi grup")
                                   ),
                                   strong("Interpretasi:"),
                                   tags$ul(
                                     tags$li("H₀: Variansi semua grup sama"),
                                     tags$li("p > 0.05: Variansi homogen"),
                                     tags$li("p ≤ 0.05: Variansi tidak homogen")
                                   ),
                                   div(
                                     style = "background: #fff; padding: 10px; border-radius: 4px; margin-top: 10px;",
                                     strong("Solusi jika tidak homogen:"), br(),
                                     "• Transformasi data", br(),
                                     "• Welch's t-test (unequal variance)", br(),
                                     "• Non-parametrik tests"
                                   )
                                 )
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
             
             # Tab 5: Statistik Inferensia
             tabPanel("Statistik Inferensia",
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
                        
                        tabsetPanel(
                          tabPanel("t-Test",
                                   br(),
                                   div(
                                     style = "background: #fff; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                                     h5("Uji t-Test"),
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
                                   )
                          ),
                          tabPanel("Uji Proporsi",
                                   br(),
                                   div(
                                     style = "background: #fff; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                                     h5("Uji Proporsi"),
                                     tags$ul(
                                       tags$li(strong("One-proportion:"), " Menguji proporsi terhadap nilai hipotesis"),
                                       tags$li(strong("Two-proportion:"), " Membandingkan proporsi dua grup"),
                                       tags$li(strong("Untuk data:"), " Variabel kategori/binary")
                                     ),
                                     strong("Contoh penggunaan:"),
                                     tags$ul(
                                       tags$li("Apakah proporsi daerah dengan kemiskinan tinggi > 50%?"),
                                       tags$li("Apakah proporsi daerah rawan bencana berbeda antar pulau?")
                                     )
                                   )
                          ),
                          tabPanel("ANOVA",
                                   br(),
                                   div(
                                     style = "background: #fff; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                                     h5("Analysis of Variance (ANOVA)"),
                                     tags$ul(
                                       tags$li(strong("One-way ANOVA:"), " Membandingkan rata-rata 3+ grup"),
                                       tags$li(strong("Two-way ANOVA:"), " Menguji efek 2 faktor dan interaksinya"),
                                       tags$li(strong("Asumsi:"), " Normalitas, homogenitas variansi, independensi")
                                     ),
                                     strong("Contoh penggunaan:"),
                                     tags$ul(
                                       tags$li("Apakah rata-rata kemiskinan berbeda antar pulau?"),
                                       tags$li("Apakah ada interaksi antara pulau dan ukuran populasi?")
                                     )
                                   )
                          ),
                          tabPanel("Uji Variance",
                                   br(),
                                   div(
                                     style = "background: #fff; padding: 15px; border-radius: 8px; border: 1px solid #dee2e6;",
                                     h5("Uji Variance"),
                                     tags$ul(
                                       tags$li(strong("Chi-square test:"), " Menguji variance satu populasi"),
                                       tags$li(strong("F-test:"), " Membandingkan variance dua populasi"),
                                       tags$li(strong("Asumsi:"), " Normalitas data")
                                     ),
                                     strong("Contoh penggunaan:"),
                                     tags$ul(
                                       tags$li("Apakah variabilitas kemiskinan sama dengan standar nasional?"),
                                       tags$li("Apakah variabilitas kemiskinan sama antara daerah urban dan rural?")
                                     )
                                   )
                          )
                        ),
                        
                        div(
                          style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          h4("Interpretasi Hasil"),
                          tags$ul(
                            tags$li(strong("p-value < 0.05:"), " Tolak H₀, ada perbedaan yang signifikan"),
                            tags$li(strong("p-value ≥ 0.05:"), " Gagal tolak H₀, tidak ada perbedaan yang signifikan"),
                            tags$li(strong("Confidence Interval:"), " Range estimasi parameter populasi"),
                            tags$li(strong("Effect Size:"), " Ukuran praktis dari perbedaan yang ditemukan")
                          )
                        )
                      )
             ),
             
             # Tab 6: Regresi Linear
             tabPanel("Regresi Linear",
                      value = "regression",
                      br(),
                      div(
                        h3("Analisis Regresi Linear Berganda"),
                        
                        div(
                          style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          h4("Tujuan Analisis Regresi"),
                          tags$ul(
                            tags$li("Memprediksi nilai variabel dependen (Y) berdasarkan variabel independen (X)"),
                            tags$li("Mengidentifikasi faktor-faktor yang berpengaruh signifikan"),
                            tags$li("Mengukur kekuatan hubungan antar variabel"),
                            tags$li("Membuat model prediksi untuk perencanaan kebijakan")
                          )
                        ),
                        
                        fluidRow(
                          column(6,
                                 div(
                                   style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                                   h4("Interpretasi Output"),
                                   strong("Coefficients (Estimate):"),
                                   tags$ul(
                                     tags$li("Perubahan Y untuk setiap kenaikan 1 unit X"),
                                     tags$li("Tanda menunjukkan arah hubungan (+/-)"),
                                     tags$li("Nilai mutlak menunjukkan kekuatan pengaruh")
                                   ),
                                   strong("P-value (Pr>|t|):"),
                                   tags$ul(
                                     tags$li("< 0.05: Variabel berpengaruh signifikan"),
                                     tags$li("≥ 0.05: Variabel tidak berpengaruh signifikan")
                                   ),
                                   strong("R-squared:"),
                                   tags$ul(
                                     tags$li("Persentase variasi Y yang dijelaskan model"),
                                     tags$li("Adjusted R² lebih akurat untuk multiple regression")
                                   )
                                 )
                          ),
                          column(6,
                                 div(
                                   style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                                   h4("Uji Asumsi Regresi"),
                                   strong("1. Linearitas:"),
                                   p("Hubungan linear antara X dan Y"),
                                   strong("2. Normalitas Residual:"),
                                   p("Q-Q plot residual mendekati garis lurus"),
                                   strong("3. Homoskedastisitas:"),
                                   p("Variance residual konstan (plot residual vs fitted)"),
                                   strong("4. Independensi:"),
                                   p("Observasi saling independen"),
                                   strong("5. Multikolinearitas:"),
                                   p("VIF < 10 (ideally < 5)")
                                 )
                          )
                        ),
                        
                        div(
                          style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          h4("Peringatan Penting"),
                          tags$ul(
                            tags$li(strong("Korelasi ≠ Kausalitas:"), " Hubungan statistik tidak selalu berarti sebab-akibat"),
                            tags$li(strong("Outliers:"), " Dapat sangat mempengaruhi hasil regresi"),
                            tags$li(strong("Overfitting:"), " Model terlalu kompleks bisa kurang generalisable"),
                            tags$li(strong("Missing Data:"), " Dapat menyebabkan bias jika not random")
                          )
                        ),
                        
                        div(
                          style = "background: #d4edda; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          h4("Tips Analisis Regresi"),
                          tags$ol(
                            tags$li("Mulai dengan analisis korelasi sederhana"),
                            tags$li("Pilih variabel berdasarkan teori, bukan hanya signifikansi statistik"),
                            tags$li("Periksa asumsi sebelum interpretasi"),
                            tags$li("Validasi model dengan data terpisah jika memungkinkan"),
                            tags$li("Interpretasikan dalam konteks domain knowledge")
                          )
                        )
                      )
             ),
             
             # Tab 7: Tips dan Best Practices
             tabPanel("Tips & Best Practices",
                      value = "tips",
                      br(),
                      div(
                        h3("Tips dan Praktik Terbaik"),
                        
                        fluidRow(
                          column(6,
                                 div(
                                   style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin: 15px 0;",
                                   h4("Workflow Analisis Data"),
                                   tags$ol(
                                     tags$li(strong("Eksplorasi:"), " Pahami data dengan statistik deskriptif"),
                                     tags$li(strong("Cleaning:"), " Tangani missing values dan outliers"),
                                     tags$li(strong("Transformasi:"), " Sesuaikan distribusi data jika perlu"),
                                     tags$li(strong("Asumsi:"), " Verifikasi prasyarat analisis"),
                                     tags$li(strong("Analisis:"), " Pilih dan jalankan uji yang tepat"),
                                     tags$li(strong("Interpretasi:"), " Kontekstualisasi dengan domain knowledge"),
                                     tags$li(strong("Validasi:"), " Cross-check dengan literature"),
                                     tags$li(strong("Dokumentasi:"), " Catat semua langkah dan keputusan")
                                   )
                                 )
                          ),
                          column(6,
                                 div(
                                   style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                                   h4("Quality Control"),
                                   tags$ul(
                                     tags$li(strong("Sanity Check:"), " Hasil masuk akal?"),
                                     tags$li(strong("Reproducibility:"), " Bisa diulang dengan hasil sama?"),
                                     tags$li(strong("Peer Review:"), " Minta orang lain review analisis"),
                                     tags$li(strong("Literature:"), " Bandingkan dengan penelitian serupa"),
                                     tags$li(strong("Sensitivity:"), " Test robustness terhadap perubahan asumsi")
                                   )
                                 )
                          )
                        ),
                        
                        div(
                          style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          h4("Data Visualization Tips"),
                          fluidRow(
                            column(4,
                                   strong("Choosing Charts:"),
                                   tags$ul(
                                     tags$li("Distribution → Histogram, Boxplot"),
                                     tags$li("Comparison → Bar chart, Boxplot"),
                                     tags$li("Relationship → Scatter plot"),
                                     tags$li("Temporal → Line chart"),
                                     tags$li("Geographic → Maps, Choropleth")
                                   )
                            ),
                            column(4,
                                   strong("Design Principles:"),
                                   tags$ul(
                                     tags$li("Clear, descriptive titles"),
                                     tags$li("Labeled axes with units"),
                                     tags$li("Consistent color schemes"),
                                     tags$li("Avoid 3D effects"),
                                     tags$li("Consider colorblind accessibility")
                                   )
                            ),
                            column(4,
                                   strong("Common Mistakes:"),
                                   tags$ul(
                                     tags$li("Truncated y-axis"),
                                     tags$li("Too many colors"),
                                     tags$li("Unclear legends"),
                                     tags$li("Inappropriate chart types"),
                                     tags$li("Information overload")
                                   )
                            )
                          )
                        ),
                        
                        div(
                          style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          h4("Common Statistical Mistakes"),
                          fluidRow(
                            column(6,
                                   strong("Design Issues:"),
                                   tags$ul(
                                     tags$li("Multiple testing without correction"),
                                     tags$li("Cherry-picking significant results"),
                                     tags$li("Ignoring effect size"),
                                     tags$li("Confusing correlation with causation"),
                                     tags$li("Inappropriate sample size")
                                   )
                            ),
                            column(6,
                                   strong("Interpretation Issues:"),
                                   tags$ul(
                                     tags$li("Over-interpreting non-significant results"),
                                     tags$li("Ignoring confidence intervals"),
                                     tags$li("Assuming normality without testing"),
                                     tags$li("Extrapolating beyond data range"),
                                     tags$li("Ignoring practical significance")
                                   )
                            )
                          )
                        ),
                        
                        div(
                          style = "background: #d4edda; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          h4("Reporting Guidelines"),
                          tags$ul(
                            tags$li(strong("Methods:"), " Jelaskan uji yang digunakan dan alasannya"),
                            tags$li(strong("Assumptions:"), " Laporkan hasil uji asumsi"),
                            tags$li(strong("Results:"), " Sertakan statistik deskriptif, test statistics, p-values, dan confidence intervals"),
                            tags$li(strong("Effect Size:"), " Laporkan magnitude of effect, bukan hanya significance"),
                            tags$li(strong("Limitations:"), " Diskusikan keterbatasan analisis dan data"),
                            tags$li(strong("Conclusions:"), " Tarik kesimpulan yang tepat dan avoid overgeneralization")
                          )
                        )
                      )
             ),
             
             # Tab 8: FAQ
             tabPanel("FAQ",
                      value = "faq",
                      br(),
                      div(
                        h3("Frequently Asked Questions"),
                        
                        div(
                          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin: 15px 0;",
                          
                          h4(style = "color: #007bff;", "Q: Data saya tidak normal, apa yang harus dilakukan?"),
                          p("A: Beberapa opsi:"),
                          tags$ul(
                            tags$li("Transformasi data (log, sqrt, Box-Cox)"),
                            tags$li("Gunakan uji non-parametrik (Mann-Whitney, Kruskal-Wallis)"),
                            tags$li("Bootstrap atau permutation tests"),
                            tags$li("Jika sample size besar (n>30), Central Limit Theorem mungkin berlaku")
                          ),
                          
                          hr(),
                          
                          h4(style = "color: #007bff;", "Q: Kapan menggunakan uji parametrik vs non-parametrik?"),
                          p("A: Gunakan parametrik jika:"),
                          tags$ul(
                            tags$li("Data berdistribusi normal (atau mendekati normal)"),
                            tags$li("Variansi homogen"),
                            tags$li("Data interval/ratio"),
                            tags$li("Sample size memadai")
                          ),
                          p("Gunakan non-parametrik jika asumsi di atas tidak terpenuhi."),
                          
                          hr(),
                          
                          h4(style = "color: #007bff;", "Q: Bagaimana menentukan sample size yang cukup?"),
                          p("A: Pertimbangkan:"),
                          tags$ul(
                            tags$li("Effect size yang ingin dideteksi"),
                            tags$li("Statistical power (biasanya 80%)"),
                            tags$li("Alpha level (biasanya 0.05)"),
                            tags$li("Variabilitas data"),
                            tags$li("Gunakan power analysis untuk estimasi")
                          ),
                          
                          hr(),
                          
                          h4(style = "color: #007bff;", "Q: Hasil tidak signifikan, apakah berarti tidak ada efek?"),
                          p("A: Tidak selalu! Non-significance bisa karena:"),
                          tags$ul(
                            tags$li("Sample size terlalu kecil (underpowered)"),
                            tags$li("Effect size kecil tapi bermakna praktis"),
                            tags$li("Variabilitas data tinggi"),
                            tags$li("Memang tidak ada efek"),
                            tags$li("Lihat confidence intervals dan effect size")
                          ),
                          
                          hr(),
                          
                          h4(style = "color: #007bff;", "Q: Bagaimana menangani outliers?"),
                          p("A: Langkah-langkah:"),
                          tags$ol(
                            tags$li("Identifikasi dengan metode statistik (IQR, Z-score)"),
                            tags$li("Investigasi penyebab (data entry error, unusual cases)"),
                            tags$li("Jika error: koreksi atau hapus"),
                            tags$li("Jika valid: pertimbangkan transformasi atau robust methods"),
                            tags$li("Laporkan sensitivity analysis (dengan dan tanpa outliers)")
                          ),
                          
                          hr(),
                          
                          h4(style = "color: #007bff;", "Q: Berapa banyak variabel yang bisa dimasukkan dalam regresi?"),
                          p("A: Rule of thumb:"),
                          tags$ul(
                            tags$li("Minimal 10-15 observasi per variabel independen"),
                            tags$li("Untuk model prediksi: 20+ observasi per variabel"),
                            tags$li("Gunakan variable selection methods jika variabel banyak"),
                            tags$li("Pertimbangkan regularization (Ridge, Lasso) untuk high-dimensional data")
                          ),
                          
                          hr(),
                          
                          h4(style = "color: #007bff;", "Q: Dashboard error atau tidak responding, apa yang harus dilakukan?"),
                          p("A: Troubleshooting steps:"),
                          tags$ol(
                            tags$li("Refresh browser page"),
                            tags$li("Check browser console untuk error messages"),
                            tags$li("Pastikan data files tersedia"),
                            tags$li("Coba dengan dataset yang lebih kecil"),
                            tags$li("Restart R session jika menggunakan local deployment")
                          )
                        )
                      )
             )
           )
         )
  )
)