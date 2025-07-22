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
    collapsible = TRUE,
    
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
    solidHeader = TRUE,
    width = 12, # Lebar penuh
    
    tabsetPanel(
      id = "regression_tabs",
      type = "tabs",
      selected = "summary_tab",
      
      tabPanel(
        title = "Ringkasan Model",
        value = "summary_tab",
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
        
        div(
          style = "background: #e9ecef; padding: 15px; border-radius: 5px;",
          h4("📥 Unduh Hasil Regresi"),
          fluidRow(
            column(8,
                   radioButtons("regression_format", 
                                "Pilih Format:", 
                                choices = list("PDF" = "pdf", "Word (.docx)" = "docx"), 
                                inline = TRUE)
            ),
            column(4,
                   # PENYESUAIAN ID: Mengubah ID agar lebih sesuai
                   downloadButton("download_regression_summary", 
                                  "📄 Unduh Laporan", 
                                  class = "btn-primary",
                                  style = "width: 100%;")
            )
          )
        )
      ),
      
      tabPanel(
        title = "Uji Asumsi",
        value = "assumptions_tab",
        br(),
        
        div(
          style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #dc3545;",
          h4("⚠️ Asumsi Regresi Linear", style = "margin-top: 0; color: #721c24;"),
          p("Model regresi linear memiliki asumsi utama yang harus dipenuhi untuk validitas hasil:", 
            style = "margin-bottom: 0; color: #721c24;")
        ),
        
        # Asumsi 1: Normalitas Residual
        div(
          style = "background: #fff; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          h5("1. Normalitas Residual"),
          p("Residual harus berdistribusi normal. Gunakan Q-Q plot dan uji Shapiro-Wilk."),
          plotOutput("regression_qqplot", height = "300px"),
          br(),
          br(),
          verbatimTextOutput("normality_residual_interpretation")
        ),
        
        # Asumsi 2: Homoskedastisitas
        div(
          style = "background: #fff; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          h5("2. Homoskedastisitas (Variansi Konstan)"),
          p("Variansi residual harus konstan. Gunakan plot Residuals vs Fitted dan uji Breusch-Pagan."),
          plotOutput("regression_residual_plot", height = "300px"),
          verbatimTextOutput("homoscedasticity_interpretation")
        ),
        
        # Asumsi 3: Independensi Residual (Autokorelasi)
        div(
          style = "background: #fff; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          h5("3. Autokorelasi"),
          p("Residual tidak boleh berkorelasi satu sama lain. Gunakan uji Durbin-Watson."),
          verbatimTextOutput("independence_interpretation") # Output untuk Durbin-Watson
        ),
        
        div(
          style = "background: #fff; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          h5("4. Multikolinearitas"),
          p("Antar variabel independen tidak boleh saling berkorelasi tinggi. Gunakan Variance Inflation Factor (VIF)."),
          verbatimTextOutput("multikolinearitas_vif") # Output untuk VIF
        ),
        
        # Penilaian Keseluruhan
        div(
          style = "background: #d4edda; padding: 15px; border-radius: 8px; border-left: 4px solid #c3e6cb;",
          h5("✅ Penilaian Keseluruhan Asumsi", style = "margin-top: 0; color: #155724;"),
          verbatimTextOutput("overall_assumptions_assessment"),
          p(strong("Catatan:"), " Jika asumsi dilanggar, pertimbangkan transformasi data atau metode regresi alternatif.",
            style = "margin-bottom: 0; color: #155724;")
        )
      ),
      
      tabPanel(
        title = "Interpretasi",
        value = "interpretation_tab",
        br(),
        div(
          style = "background: #d1ecf1; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #bee5eb;",
          h4("Interpretasi Hasil Regresi", style = "margin-top: 0; color: #0c5460;"),
          p("Analisis mendalam terhadap koefisien, signifikansi, dan kualitas model regresi.", 
            style = "color: #0c5460; margin-bottom: 0;")
        ),
        verbatimTextOutput("regression_interpretation", placeholder = FALSE)
      )
    )
  )
)