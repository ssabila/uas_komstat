# ui/regression_ui.R
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
      selected = "summary_tab", # PERBAIKAN: Set tab default
      
      tabPanel("Ringkasan Model", 
               value = "summary_tab", # PERBAIKAN: Tambahkan value
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
               value = "assumptions_tab", # PERBAIKAN: Tambahkan value
               icon = icon("check"),
               br(),
               div(
                 style = "background: #d4edda; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
                 icon("info-circle", style = "color: #155724;"),
                 strong(" Pentingnya Uji Asumsi:"),
                 p("Regresi linear valid hanya jika asumsi terpenuhi. Periksa setiap plot dengan seksama.", 
                   style = "margin: 5px 0 0 0; color: #155724;")
               ),
               
               h4("1. Normalitas Residual (Plot Q-Q)"),
               p("Titik-titik harus mengikuti garis diagonal. Jika menyimpang, residual tidak normal."),
               plotOutput("regression_qqplot", height = "350px"),
               
               hr(),
               h4("2. Homoskedastisitas (Residuals vs Fitted)"),
               p("Titik-titik harus tersebar acak tanpa pola. Jika membentuk corong, terjadi heteroskedastisitas."),
               plotOutput("regression_residual_plot", height = "350px"),
               
               hr(),
               h4("3. Multikolinearitas (VIF)"),
               p("VIF > 10 menunjukkan multikolinearitas serius. VIF > 5 perlu diwaspadai."),
               verbatimTextOutput("regression_vif")
      ),
      
      tabPanel("Interpretasi", 
               value = "interpretation_tab", # PERBAIKAN: Tambahkan value
               icon = icon("comment-dots"),
               br(),
               div(
                 style = "background: #d1ecf1; padding: 15px; border-radius: 8px; border-left: 4px solid #bee5eb;",
                 h4("Panduan Interpretasi Lengkap", style = "color: #0c5460; margin-top: 0;"),
                 p("Gunakan panduan di bawah untuk memahami dan mengkomunikasikan hasil regresi.", 
                   style = "color: #0c5460; margin-bottom: 0;")
               ),
               verbatimTextOutput("regression_interpretation")
      )
    )
  )
)