# ui/regression_ui.R - VERSI DIPERBAIKI DENGAN INTERPRETASI UJI ASUMSI LENGKAP

tagList(
  # Kotak Header Utama
  box(
    title = "Regresi Linear Berganda",
    status = "primary",
    solidHeader = TRUE,
    width = 12, # Lebar penuh
    p("Bangun model regresi untuk memprediksi sebuah variabel dependen (Y) berdasarkan beberapa variabel independen (X)."),
    div(
      style = "background: #e3f2fd; padding: 10px; border-radius: 5px; border-left: 4px solid #2196f3;",
      icon("info-circle", style = "color: #2196f3;"),
      strong(" Tips: "),
      "Pastikan hubungan linear antar variabel dan periksa asumsi regresi pada tab 'Uji Asumsi'."
    )
  ),
  
  # Kotak Pengaturan Model (Bagian Atas)
  box(
    title = "Pengaturan Model Regresi",
    status = "info",
    solidHeader = TRUE,
    width = 12, # Lebar penuh
    collapsible = TRUE, # Bisa dilipat agar tidak memakan tempat
    
    fluidRow(
      column(4,
             uiOutput("reg_dependent_selector")
      ),
      column(6,
             uiOutput("reg_independent_selector")
      ),
      column(2,
             br(),
             actionButton("run_regression", "Bangun Model", 
                          class = "btn-success btn-block", 
                          icon = icon("cogs"))
      )
    ),
    
    # Status dan validasi
    div(
      id = "regression_status",
      style = "margin-top: 15px; padding: 10px; background: #f8f9fa; border-radius: 4px; border-left: 4px solid #28a745;",
      icon("check-circle", style = "color: #28a745;"),
      span(" Status: Siap untuk membangun model regresi", style = "color: #155724;")
    )
  ),
  
  # Kotak Hasil Model (Bagian Bawah)
  box(
    title = "Hasil Model Regresi",
    status = "success",
    width = 12, # Lebar penuh
    tabsetPanel(
      id = "regression_tabs",
      type = "tabs",
      selected = "summary_tab", # Tab default
      
      tabPanel("Ringkasan Model", 
               value = "summary_tab",
               icon = icon("list-alt"),
               br(),
               div(
                 style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
                 icon("lightbulb", style = "color: #856404;"),
                 strong(" Cara Membaca Hasil:"),
                 tags$ul(
                   style = "margin-bottom: 0; color: #856404;",
                   tags$li("R-squared: Persentase variabilitas Y yang dijelaskan model"),
                   tags$li("P-value koefisien < 0.05: Variabel berpengaruh signifikan"),
                   tags$li("Estimate: Perubahan Y per unit perubahan X")
                 )
               ),
               verbatimTextOutput("regression_summary"),
               
               hr(),
               h4("Unduh Ringkasan"),
               fluidRow(
                 column(8,
                        radioButtons("regression_format", "Pilih Format:", 
                                     choices = list("PDF" = "pdf", "Word" = "docx"), 
                                     inline = TRUE)
                 ),
                 column(4,
                        downloadButton("download_regression_summary", "Unduh", 
                                       class = "btn-primary")
                 )
               )
      ),
      
      tabPanel("Uji Asumsi", 
               value = "assumptions_tab",
               icon = icon("check-circle"),
               br(),
               div(
                 style = "background: #d4edda; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                 icon("info-circle", style = "color: #155724; font-size: 18px;"),
                 strong(" Pentingnya Uji Asumsi:", style = "color: #155724; font-size: 16px;"),
                 p("Regresi linear valid hanya jika asumsi terpenuhi. Setiap uji memberikan interpretasi lengkap untuk membantu pengambilan keputusan.", 
                   style = "margin: 8px 0 0 0; color: #155724;")
               ),
               
               # === 1. UJI NORMALITAS RESIDUAL ===
               div(
                 style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #007bff;",
                 h4("1. Uji Normalitas Residual", style = "color: #007bff; margin-top: 0;"),
                 p("Residual harus berdistribusi normal untuk validitas inferensi statistik.", style = "margin-bottom: 15px;"),
                 
                 # Plot normalitas
                 plotOutput("regression_qqplot", height = "400px"),
                 
                 hr(),
                 
                 # Interpretasi normalitas
                 h5("Interpretasi Uji Normalitas:", style = "color: #007bff;"),
                 div(
                   style = "background: #ffffff; padding: 12px; border: 1px solid #dee2e6; border-radius: 4px;",
                   verbatimTextOutput("normality_residual_interpretation")
                 )
               ),
               
               # === 2. UJI HOMOSKEDASTISITAS ===
               div(
                 style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #28a745;",
                 h4("2. Uji Homoskedastisitas", style = "color: #28a745; margin-top: 0;"),
                 p("Varians residual harus konstan di semua level variabel independen.", style = "margin-bottom: 15px;"),
                 
                 # Plot homoskedastisitas
                 plotOutput("regression_residual_plot", height = "400px"),
                 
                 hr(),
                 
                 # Interpretasi homoskedastisitas
                 h5("Interpretasi Uji Homoskedastisitas:", style = "color: #28a745;"),
                 div(
                   style = "background: #ffffff; padding: 12px; border: 1px solid #dee2e6; border-radius: 4px;",
                   verbatimTextOutput("homoskedasticity_interpretation")
                 )
               ),
               
               # === 3. UJI MULTIKOLINEARITAS ===
               div(
                 style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px; border-left: 4px solid #ffc107;",
                 h4("3. Uji Multikolinearitas (VIF)", style = "color: #856404; margin-top: 0;"),
                 p("Variabel independen tidak boleh berkorelasi tinggi satu sama lain.", style = "margin-bottom: 15px;"),
                 
                 # Interpretasi VIF
                 h5("Hasil dan Interpretasi VIF:", style = "color: #856404;"),
                 div(
                   style = "background: #ffffff; padding: 12px; border: 1px solid #dee2e6; border-radius: 4px;",
                   verbatimTextOutput("regression_vif")
                 )
               )
      ),
      
      tabPanel("Interpretasi Model", 
               value = "interpretation_tab",
               icon = icon("comment-dots"),
               br(),
               div(
                 style = "background: #d1ecf1; padding: 15px; border-radius: 8px; border-left: 4px solid #bee5eb;",
                 h4("Panduan Interpretasi Lengkap", style = "color: #0c5460; margin-top: 0;"),
                 p("Gunakan panduan di bawah untuk memahami dan mengkomunikasikan hasil regresi secara komprehensif.", 
                   style = "color: #0c5460; margin-bottom: 0;")
               ),
               
               # Area interpretasi
               div(
                 style = "background: #ffffff; padding: 15px; border: 1px solid #dee2e6; border-radius: 8px; margin-top: 15px;",
                 verbatimTextOutput("regression_interpretation", 
                                    placeholder = FALSE) %>% 
                   tagAppendAttributes(style = "
                    white-space: pre-wrap !important; 
                    word-wrap: break-word !important; 
                    overflow-wrap: break-word !important;
                    max-width: 100% !important;
                    overflow-x: auto !important;
                  ")
               ),
               
               # Panduan tambahan
               div(
                 style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 20px;",
                 h5("Panduan Pelaporan Hasil:", style = "color: #495057;"),
                 tags$ol(
                   tags$li("Mulai dengan kualitas model (R-squared, F-test)"),
                   tags$li("Interpretasikan koefisien yang signifikan"),
                   tags$li("Laporkan hasil uji asumsi"),
                   tags$li("Berikan rekomendasi berdasarkan temuan"),
                   tags$li("Diskusikan keterbatasan model")
                 )
               )
      ),
    )
  )
)