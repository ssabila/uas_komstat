# ui/anova_ui.R - FINAL FIXED VERSION

tagList(
  # Header Section
  box(
    title = "Analysis of Variance (ANOVA)",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    fluidRow(
      column(8,
             p(strong("ANOVA"), "digunakan untuk membandingkan rata-rata dari tiga atau lebih kelompok."),
             p("Pastikan asumsi ", strong("normalitas"), " dan ", strong("homogenitas variansi"), " terpenuhi sebelum melakukan analisis.")
      ),
      column(4,
             div(
               style = "background: #d4edda; padding: 10px; border-radius: 5px; border-left: 4px solid #28a745;",
               h5("💡 Tips Penggunaan", style = "margin-top: 0;"),
               tags$ul(
                 tags$li("One-Way: 1 faktor, 3+ grup"),
                 tags$li("Two-Way: 2 faktor + interaksi"),
                 tags$li("Cek asumsi di tab 'Uji Asumsi'")
               )
             )
      )
    )
  ),
  
  # Main Analysis Section
  fluidRow(
    # Control Panel
    column(4,
           box(
             title = "Pengaturan ANOVA",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             
             # ANOVA Type Selection
             div(
               style = "margin-bottom: 15px;",
               selectInput("anova_type", 
                           label = div(icon("cogs"), "Pilih Jenis ANOVA:"),
                           choices = list(
                             "Satu Arah (One-Way)" = "one_way",
                             "Dua Arah (Two-Way)" = "two_way"
                           ),
                           selected = "one_way")
             ),
             
             # Variable Selectors
             div(
               style = "margin-bottom: 15px;",
               uiOutput("anova_dependent_selector")
             ),
             
             div(
               style = "margin-bottom: 15px;",
               uiOutput("anova_independent1_selector")
             ),
             
             # Two-way specific options
             conditionalPanel(
               condition = "input.anova_type == 'two_way'",
               div(
                 style = "margin-bottom: 15px;",
                 uiOutput("anova_independent2_selector")
               ),
               div(
                 style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                 checkboxInput("anova_interaction", 
                               label = div(icon("link"), "Sertakan Efek Interaksi"), 
                               value = TRUE),
                 helpText("Efek interaksi menguji apakah pengaruh satu faktor bergantung pada level faktor lain.")
               )
             ),
             
             # Action Button
             div(
               style = "text-align: center; margin-top: 20px;",
               actionButton("run_anova", 
                            label = div(icon("play"), "Jalankan ANOVA"), 
                            class = "btn-success btn-lg",
                            style = "width: 100%; font-weight: bold;")
             ),
             
             # Information Box
             div(
               style = "background: #e7f3ff; padding: 10px; border-radius: 5px; margin-top: 15px;",
               h5("📊 Asumsi ANOVA", style = "margin-top: 0;"),
               tags$ul(
                 tags$li(strong("Normalitas:"), " Data berdistribusi normal"),
                 tags$li(strong("Homogenitas:"), " Variansi antar grup sama"),
                 tags$li(strong("Independensi:"), " Observasi saling independen")
               ),
               p(em("Gunakan tab 'Uji Asumsi' untuk memverifikasi asumsi ini."))
             )
           )
    ),
    
    # Results Panel
    column(8,
           box(
             title = "Hasil Analisis ANOVA",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             
             tabsetPanel(
               id = "anova_results_tabs",
               
               # ANOVA Table Tab
               tabPanel(
                 title = div(icon("table"), "Tabel ANOVA"),
                 value = "anova_table",
                 br(),
                 div(
                   style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                   verbatimTextOutput("anova_summary_table")
                 ),
                 
                 hr(),
                 
                 # Download Section
                 div(
                   style = "background: #e9ecef; padding: 15px; border-radius: 5px;",
                   h4(icon("download"), "Unduh Hasil ANOVA", style = "margin-top: 0;"),
                   fluidRow(
                     column(6,
                            radioButtons("anova_format", 
                                         "Pilih Format:", 
                                         choices = list(
                                           "PDF" = "pdf", 
                                           "Word (.docx)" = "docx"
                                         ), 
                                         inline = TRUE)
                     ),
                     column(6,
                            br(),
                            downloadButton("download_anova_result", 
                                           label = div(icon("file-download"), "Unduh Laporan"),
                                           class = "btn-primary",
                                           style = "width: 100%;")
                     )
                   )
                 )
               ),
               
               # Interpretation Tab
               tabPanel(
                 title = div(icon("lightbulb"), "Interpretasi"),
                 value = "interpretation",
                 br(),
                 div(
                   style = "background: #f8f9fa; padding: 20px; border-radius: 5px;",
                   div(
                     style = "background: #d1ecf1; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #bee5eb;",
                     h4(icon("info-circle"), "Interpretasi Hasil", style = "margin-top: 0; color: #0c5460;"),
                     p("Interpretasi statistik berdasarkan hasil ANOVA yang telah dijalankan:", 
                       style = "margin-bottom: 0; color: #0c5460;")
                   ),
                   verbatimTextOutput("anova_interpretation", placeholder = TRUE)
                 ),
                 
                 # Post Hoc Notification (conditionally shown)
                 conditionalPanel(
                   condition = "output.show_posthoc == true",
                   div(
                     style = "background: #d4edda; padding: 15px; border-radius: 5px; margin-top: 15px; border-left: 4px solid #c3e6cb;",
                     h5(icon("exclamation-triangle"), "Uji Post Hoc Diperlukan!", style = "margin-top: 0; color: #155724;"),
                     p("Hasil ANOVA menunjukkan perbedaan signifikan antar grup. ", 
                       strong("Silakan gunakan tab 'Uji Post Hoc'"), 
                       " untuk mengidentifikasi grup mana yang berbeda secara spesifik.",
                       style = "margin-bottom: 0; color: #155724;")
                   )
                 ),
                 
                 # Guidance Box
                 div(
                   style = "background: #d4edda; padding: 15px; border-radius: 5px; margin-top: 15px;",
                   h5(icon("question-circle"), "Panduan Interpretasi", style = "margin-top: 0;"),
                   tags$ul(
                     tags$li(strong("p < 0.05:"), " Ada perbedaan signifikan antar grup"),
                     tags$li(strong("p ≥ 0.05:"), " Tidak ada perbedaan signifikan"),
                     tags$li(strong("F-statistik:"), " Semakin besar, semakin kuat bukti perbedaan"),
                     tags$li(strong("Post-hoc:"), " Jika signifikan, lakukan uji lanjutan untuk identifikasi grup yang berbeda")
                   )
                 )
               ),
               
               # Visualization Tab
               tabPanel(
                 title = div(icon("chart-bar"), "Visualisasi"),
                 value = "visualization",
                 br(),
                 div(
                   style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                   h4(icon("chart-line"), "Visualisasi Distribusi Data", style = "margin-top: 0;"),
                   p("Boxplot menunjukkan distribusi data antar grup. Titik merah menunjukkan rata-rata grup.")
                 ),
                 
                 # Plot container with loading message
                 div(
                   id = "plot_container",
                   plotlyOutput("anova_plot", height = "500px"),
                   conditionalPanel(
                     condition = "!output.anova_plot",
                     div(
                       style = "text-align: center; padding: 50px; color: #6c757d;",
                       h4(icon("chart-bar"), "Grafik Belum Tersedia"),
                       p("Klik 'Jalankan ANOVA' untuk menampilkan visualisasi.")
                     )
                   )
                 ),
                 
                 # Visualization Guide
                 div(
                   style = "background: #fff3cd; padding: 15px; border-radius: 5px; margin-top: 15px;",
                   h5(icon("eye"), "Cara Membaca Visualisasi", style = "margin-top: 0;"),
                   fluidRow(
                     column(6,
                            tags$ul(
                              tags$li(strong("Box:"), " Q1, median, Q3"),
                              tags$li(strong("Whiskers:"), " Rentang data normal"),
                              tags$li(strong("Titik:"), " Data individual")
                            )
                     ),
                     column(6,
                            tags$ul(
                              tags$li(strong("Diamond merah:"), " Rata-rata grup"),
                              tags$li(strong("Outliers:"), " Titik di luar whiskers"),
                              tags$li(strong("Overlap box:"), " Kemungkinan tidak berbeda")
                            )
                     )
                   )
                 )
               )
             )
           )
    )
  ),
  
  # Post Hoc Section (Conditional)
  conditionalPanel(
    condition = "output.show_posthoc == true",
    fluidRow(
      column(12,
             box(
               title = div(icon("search-plus"), "Uji Post Hoc"),
               status = "warning",
               solidHeader = TRUE,
               width = 12,
               
               div(
                 style = "background: #d1ecf1; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #bee5eb;",
                 h4(icon("info-circle"), "Uji Post Hoc Tersedia", style = "margin-top: 0; color: #0c5460;"),
                 p("Hasil ANOVA menunjukkan perbedaan signifikan antar grup. Uji post hoc diperlukan untuk mengidentifikasi grup mana yang berbeda.", 
                   style = "margin-bottom: 0; color: #0c5460;")
               ),
               
               # Post Hoc Method Selection
               fluidRow(
                 column(4,
                        div(
                          style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                          h5("Pilih Metode Post Hoc:", style = "margin-top: 0;"),
                          selectInput("posthoc_method", "Metode:",
                                      choices = list(
                                        "Tukey HSD" = "tukey",
                                        "Bonferroni" = "bonferroni",
                                        "Holm" = "holm",
                                        "Scheffe" = "scheffe"
                                      ),
                                      selected = "tukey"
                          ),
                          actionButton("run_posthoc", 
                                       label = div(icon("play"), "Jalankan Post Hoc"),
                                       class = "btn-primary btn-block")
                        )
                 ),
                 
                 column(8,
                        tabsetPanel(
                          tabPanel(
                            title = "Hasil",
                            br(),
                            div(
                              style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                              verbatimTextOutput("posthoc_results")
                            )
                          ),
                          tabPanel(
                            title = "Interpretasi",
                            br(),
                            div(
                              style = "background: #d4edda; padding: 15px; border-radius: 5px;",
                              verbatimTextOutput("posthoc_interpretation")
                            )
                          ),
                          tabPanel(
                            title = "Visualisasi",
                            br(),
                            plotlyOutput("posthoc_plot", height = "400px")
                          )
                        )
                 )
               ),
               
               # Method Guide
               div(
                 style = "background: #fff3cd; padding: 15px; border-radius: 5px; margin-top: 15px;",
                 h5(icon("question-circle"), "Panduan Metode Post Hoc", style = "margin-top: 0;"),
                 fluidRow(
                   column(6,
                          tags$ul(
                            tags$li(strong("Tukey HSD:"), " Paling umum, kontrol family-wise error rate"),
                            tags$li(strong("Bonferroni:"), " Konservatif, cocok untuk sedikit perbandingan")
                          )
                   ),
                   column(6,
                          tags$ul(
                            tags$li(strong("Holm:"), " Lebih powerful dari Bonferroni"),
                            tags$li(strong("Scheffe:"), " Paling konservatif, untuk banyak perbandingan")
                          )
                   )
                 )
               )
             )
      )
    )
  ),
  
  # Information Footer
  div(
    style = "margin-top: 20px;",
    box(
      title = "Informasi Penting",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      collapsible = TRUE,
      collapsed = TRUE,
      
      fluidRow(
        column(4,
               h5("🎯 Kapan Menggunakan ANOVA"),
               tags$ul(
                 tags$li("Membandingkan rata-rata 3+ grup"),
                 tags$li("Data numerik (variabel dependen)"),
                 tags$li("Grup independen (variabel independen)"),
                 tags$li("Asumsi terpenuhi")
               )
        ),
        column(4,
               h5("⚠️ Hal yang Perlu Diperhatikan"),
               tags$ul(
                 tags$li("Cek normalitas data terlebih dahulu"),
                 tags$li("Pastikan homogenitas variansi"),
                 tags$li("Jika signifikan, lakukan uji post-hoc"),
                 tags$li("Perhatikan ukuran sampel per grup")
               )
        ),
        column(4,
               h5("📚 Alternatif Jika Asumsi Dilanggar"),
               tags$ul(
                 tags$li("Kruskal-Wallis (non-parametrik)"),
                 tags$li("Welch ANOVA (variansi tidak homogen)"),
                 tags$li("Transformasi data"),
                 tags$li("Bootstrap methods")
               )
        )
      )
    )
  )
)