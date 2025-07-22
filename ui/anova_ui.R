# ui/anova_ui.R - Kembalikan versi original lengkap dengan post hoc + tambahan download

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
               type = "tabs",
               
               # ANOVA Table Tab
               tabPanel(
                 title = "Tabel ANOVA",
                 value = "anova_table",
                 br(),
                 div(
                   style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                   verbatimTextOutput("anova_summary_table")
                 ),
                 
                 hr(),
                 
                 # Download Section - TAMBAHAN BARU
                 # Smart Download Section untuk UI ANOVA - ganti bagian download di ui/anova_ui.R
                 
                 # Smart Download Section - Auto ANOVA + Tukey
                 div(
                   class = "download-section",
                   style = "background: #e9ecef; padding: 20px; border-radius: 8px; border-top: 3px solid #28a745; margin-top: 20px;",
                   h4("🚀 Smart Download - Laporan Lengkap", style = "margin-top: 0; color: #495057;"),
                   p(strong("Sekali klik untuk laporan komprehensif!"), " Sistem akan otomatis menjalankan Tukey HSD jika ANOVA signifikan."),
                   
                   fluidRow(
                     column(6,
                            radioButtons("anova_download_format", 
                                         "Pilih Format Download:", 
                                         choices = list(
                                           "📄 PDF" = "pdf", 
                                           "📝 Word (.docx)" = "docx"
                                         ), 
                                         inline = FALSE,
                                         selected = "pdf")
                     ),
                     column(6,
                            div(
                              style = "margin-top: 25px;",
                              downloadButton("download_anova_result", 
                                             "🚀 Download Laporan Lengkap", 
                                             class = "btn-success btn-lg",
                                             style = "width: 100%; padding: 12px; font-weight: bold;")
                            )
                     )
                   ),
                   
                   # Smart process explanation
                   div(
                     style = "background: #d4edda; padding: 15px; border-radius: 5px; margin-top: 15px; border-left: 4px solid #28a745;",
                     h6("🤖 Proses Otomatis", style = "margin-top: 0; color: #155724;"),
                     div(
                       style = "display: flex; align-items: center; margin-bottom: 10px;",
                       div(style = "background: #28a745; color: white; border-radius: 50%; width: 25px; height: 25px; display: flex; align-items: center; justify-content: center; margin-right: 10px; font-weight: bold; font-size: 12px;", "1"),
                       span("Sistem menganalisis hasil ANOVA Anda", style = "color: #155724;")
                     ),
                     div(
                       style = "display: flex; align-items: center; margin-bottom: 10px;",
                       div(style = "background: #28a745; color: white; border-radius: 50%; width: 25px; height: 25px; display: flex; align-items: center; justify-content: center; margin-right: 10px; font-weight: bold; font-size: 12px;", "2"),
                       span("Jika signifikan → Tukey HSD otomatis dijalankan", style = "color: #155724;")
                     ),
                     div(
                       style = "display: flex; align-items: center;",
                       div(style = "background: #28a745; color: white; border-radius: 50%; width: 25px; height: 25px; display: flex; align-items: center; justify-content: center; margin-right: 10px; font-weight: bold; font-size: 12px;", "3"),
                       span("Laporan lengkap dengan interpretasi siap diunduh", style = "color: #155724;")
                     )
                   ),
                   
                   # Content overview
                   div(
                     style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px; border-left: 4px solid #6c757d;",
                     h6("📋 Isi Laporan Lengkap", style = "margin-top: 0; color: #495057;"),
                     
                     # Always included
                     div(
                       style = "margin-bottom: 10px;",
                       h6("✅ Selalu Disertakan:", style = "margin: 0 0 5px 0; color: #155724;"),
                       tags$ul(
                         style = "margin: 0 0 0 20px; font-size: 0.9em; color: #495057;",
                         tags$li("Tabel ANOVA lengkap + interpretasi"),
                         tags$li("Formula analisis & ringkasan model"),
                         tags$li("Informasi dataset & metadata")
                       )
                     ),
                     
                     # Conditional content
                     div(
                       h6("🔍 Tukey HSD (Otomatis):", style = "margin: 0 0 5px 0; color: #0c5460;"),
                       tags$ul(
                         style = "margin: 0 0 0 20px; font-size: 0.9em; color: #495057;",
                         tags$li("✓ Dijalankan otomatis jika ANOVA signifikan"),
                         tags$li("✓ Perbandingan berpasangan semua kelompok"),
                         tags$li("✓ Interpretasi kelompok mana yang berbeda"),
                         tags$li("ℹ️ Dilewati jika ANOVA tidak signifikan")
                       )
                     )
                   ),
                   
                   # Benefits highlight
                   div(
                     style = "background: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px; border-left: 3px solid #ffc107;",
                     h6("⚡ Keunggulan Smart Download:", style = "margin-top: 0; color: #856404;"),
                     div(
                       style = "font-size: 0.9em; color: #856404;",
                       "• Tidak perlu klik berkali-kali • Hasil selalu lengkap • Workflow yang efisien • Laporan profesional siap pakai"
                     )
                   )
                 )
               ),
               
               # Interpretation Tab
               tabPanel(
                 title = "Interpretasi",
                 value = "anova_interpretation",
                 br(),
                 div(
                   style = "background: #f8f9fa; padding: 20px; border-radius: 5px;",
                   h4("📝 Interpretasi Hasil ANOVA"),
                   hr(),
                   div(
                     style = "background: white; padding: 15px; border-radius: 5px; border-left: 4px solid #007bff;",
                     verbatimTextOutput("anova_interpretation")
                   )
                 )
               ),
               
               # Visualisasi Tab
               tabPanel(
                 title = "Visualisasi",
                 value = "anova_visualization",
                 br(),
                 
                 # Info Box
                 div(
                   style = "background: #e7f3ff; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                   h4("📈 Visualisasi Data ANOVA", style = "margin-top: 0;"),
                   p("Visualisasi ini membantu memahami distribusi data dan perbedaan antar grup secara visual.")
                 ),
                 
                 # Plot container with loading message
                 div(
                   id = "plot_container",
                   plotlyOutput("anova_plot", height = "500px"),
                   conditionalPanel(
                     condition = "!output.anova_plot",
                     div(
                       style = "text-align: center; padding: 50px; color: #6c757d;",
                       h4("📊 Grafik Belum Tersedia"),
                       p("Klik 'Jalankan ANOVA' untuk menampilkan visualisasi.")
                     )
                   )
                 ),
                 
                 # Visualization Guide
                 div(
                   style = "background: #fff3cd; padding: 15px; border-radius: 5px; margin-top: 15px;",
                   h5("👁️ Cara Membaca Visualisasi", style = "margin-top: 0;"),
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
  
  # Post Hoc Section (Conditional) - KEMBALIKAN SEPERTI SEMULA
  conditionalPanel(
    condition = "output.show_posthoc == true",
    fluidRow(
      column(12,
             box(
               title = "🔍 Uji Post Hoc Tukey HSD",
               status = "warning",
               solidHeader = TRUE,
               width = 12,
               
               div(
                 style = "background: #d1ecf1; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #bee5eb;",
                 h4("ℹ️ Uji Post Hoc Tukey HSD", style = "margin-top: 0; color: #0c5460;"),
                 p("Hasil ANOVA menunjukkan perbedaan signifikan antar grup. ",
                   "Uji post hoc Tukey HSD akan mengidentifikasi grup mana yang berbeda secara spesifik.", 
                   style = "margin-bottom: 0; color: #0c5460;")
               ),
               
               # Post Hoc Control and Results
               fluidRow(
                 column(4,
                        div(
                          style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                          h5("Uji Post Hoc Tukey HSD", style = "margin-top: 0;"),
                          div(
                            style = "background: #e9ecef; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                            h6("✅ Metode: Tukey HSD", style = "margin: 0; color: #495057;"),
                            p("Metode paling umum dan andal untuk perbandingan multiple", 
                              style = "margin: 5px 0 0 0; font-size: 0.9em; color: #6c757d;")
                          ),
                          actionButton("run_posthoc", 
                                       label = "▶️ Jalankan Tukey HSD",
                                       class = "btn-primary btn-block",
                                       style = "font-weight: bold;")
                        )
                 ),
                 
                 column(8,
                        tabsetPanel(
                          id = "posthoc_tabs",
                          type = "tabs",
                          
                          tabPanel(
                            title = "Hasil",
                            value = "posthoc_results_tab",
                            br(),
                            div(
                              style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                              verbatimTextOutput("posthoc_results")
                            )
                          ),
                          tabPanel(
                            title = "Interpretasi",
                            value = "posthoc_interpretation_tab",
                            br(),
                            div(
                              style = "background: #d4edda; padding: 15px; border-radius: 5px;",
                              verbatimTextOutput("posthoc_interpretation")
                            )
                          ),
                          tabPanel(
                            title = "Visualisasi",
                            value = "posthoc_visualization_tab",
                            br(),
                            div(
                              style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                              plotlyOutput("posthoc_plot", height = "400px")
                            )
                          )
                        )
                 )
               )
             )
      )
    )
  )
)