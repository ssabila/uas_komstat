# ui/regression_ui.R - VERSI DIPERBAIKI UNTUK MENGATASI ERROR TABSETPANEL

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
    solidHeader = TRUE,
    width = 12, # Lebar penuh
    
    # PERBAIKAN: Tambahkan parameter yang diperlukan untuk tabsetPanel
    tabsetPanel(
      id = "regression_tabs",      # PERBAIKAN: Tambahkan id
      type = "tabs",               # PERBAIKAN: Tambahkan parameter type yang diperlukan
      selected = "summary_tab",    # Tab default
      
      # Tab Ringkasan Model
      tabPanel(
        title = "Ringkasan Model",  # PERBAIKAN: Simplify title, hapus icon() 
        value = "summary_tab",      # PERBAIKAN: Tambahkan value
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
        
        # Download Section
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
                   downloadButton("download_regression_result", 
                                  "📄 Unduh Laporan", 
                                  class = "btn-primary",
                                  style = "width: 100%;")
            )
          )
        )
      ),
      
      # Tab Uji Asumsi
      tabPanel(
        title = "Uji Asumsi",           # PERBAIKAN: Simplify title
        value = "assumptions_tab",      # PERBAIKAN: Tambahkan value
        br(),
        
        div(
          style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #dc3545;",
          h4("⚠️ Asumsi Regresi Linear", style = "margin-top: 0; color: #721c24;"),
          p("Model regresi linear memiliki 4 asumsi utama yang harus dipenuhi:", 
            style = "margin-bottom: 0; color: #721c24;")
        ),
        
        # Asumsi 1: Normalitas Residual
        div(
          style = "background: #fff; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          h5("1. 📊 Normalitas Residual"),
          p("Residual harus berdistribusi normal. Gunakan Q-Q plot dan uji Shapiro-Wilk."),
          
          fluidRow(
            column(8,
                   plotOutput("regression_qqplot", height = "300px")
            ),
            column(4,
                   div(
                     style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                     h6("Interpretasi Q-Q Plot:"),
                     tags$ul(
                       tags$li("Titik mengikuti garis → Normal"),
                       tags$li("Titik menyimpang → Tidak normal"),
                       tags$li("Pola kurva → Distribusi skewed")
                     )
                   )
            )
          ),
          verbatimTextOutput("normality_residual_interpretation")
        ),
        
        # Asumsi 2: Homoskedastisitas
        div(
          style = "background: #fff; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          h5("2. 📈 Homoskedastisitas (Variansi Konstan)"),
          p("Variansi residual harus konstan di semua level variabel independen."),
          
          fluidRow(
            column(8,
                   plotOutput("regression_residual_plot", height = "300px")
            ),
            column(4,
                   div(
                     style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                     h6("Interpretasi Residual Plot:"),
                     tags$ul(
                       tags$li("Titik tersebar acak → Homoskedastis"),
                       tags$li("Pola funnel → Heteroskedastis"),
                       tags$li("Pola kurva → Non-linear")
                     )
                   )
            )
          ),
          verbatimTextOutput("homoscedasticity_interpretation")
        ),
        
        # Asumsi 3: Independensi
        div(
          style = "background: #fff; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          h5("3. 🔗 Independensi Residual"),
          p("Residual tidak berkorelasi (tidak ada autokorelasi). Gunakan uji Durbin-Watson."),
          verbatimTextOutput("independence_interpretation")
        ),
        
        # Asumsi 4: Linearitas
        div(
          style = "background: #fff; padding: 15px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #dee2e6;",
          h5("4. 📏 Linearitas"),
          p("Hubungan antara variabel independen dan dependen harus linear."),
          plotOutput("regression_linearity_plot", height = "300px"),
          verbatimTextOutput("linearity_interpretation")
        ),
        
        # Overall Assessment
        div(
          style = "background: #d4edda; padding: 15px; border-radius: 8px; border-left: 4px solid #c3e6cb;",
          h5("✅ Penilaian Keseluruhan Asumsi", style = "margin-top: 0; color: #155724;"),
          verbatimTextOutput("overall_assumptions_assessment"),
          p(strong("Catatan:"), " Jika asumsi dilanggar, pertimbangkan transformasi data atau metode regresi alternatif.",
            style = "margin-bottom: 0; color: #155724;")
        )
      ),
      
      # Tab Interpretasi
      tabPanel(
        title = "Interpretasi",           # PERBAIKAN: Simplify title
        value = "interpretation_tab",     # PERBAIKAN: Tambahkan value
        br(),
        
        div(
          style = "background: #d1ecf1; padding: 15px; border-radius: 5px; margin-bottom: 15px; border-left: 4px solid #bee5eb;",
          h4("💡 Interpretasi Hasil Regresi", style = "margin-top: 0; color: #0c5460;"),
          p("Analisis mendalam terhadap koefisien, signifikansi, dan kualitas model regresi.", 
            style = "color: #0c5460; margin-bottom: 0;")
        ),
        
        # Area interpretasi
        div(
          style = "background: #ffffff; padding: 15px; border: 1px solid #dee2e6; border-radius: 8px; margin-top: 15px;",
          verbatimTextOutput("regression_interpretation", 
                             placeholder = FALSE)
        ),
        
        # Panduan tambahan
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-top: 20px;",
          h5("📋 Panduan Pelaporan Hasil:", style = "color: #495057;"),
          tags$ol(
            tags$li("Mulai dengan kualitas model (R-squared, F-test)"),
            tags$li("Interpretasikan koefisien yang signifikan"),
            tags$li("Laporkan hasil uji asumsi"),
            tags$li("Berikan rekomendasi berdasarkan temuan"),
            tags$li("Diskusikan keterbatasan model")
          )
        )
      ),
      
      # Tab Prediksi
      tabPanel(
        title = "Prediksi",               # PERBAIKAN: Simplify title
        value = "prediction_tab",         # PERBAIKAN: Tambahkan value
        br(),
        
        div(
          style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          h4("🔮 Prediksi dengan Model Regresi"),
          p("Gunakan model yang telah dibuat untuk memprediksi nilai variabel dependen berdasarkan input variabel independen.")
        ),
        
        # Input untuk prediksi
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          h5("Input Nilai untuk Prediksi:"),
          uiOutput("prediction_inputs"),
          br(),
          actionButton("make_prediction", "🎯 Buat Prediksi", 
                       class = "btn-primary",
                       style = "width: 200px;")
        ),
        
        # Hasil prediksi
        div(
          style = "background: #e8f4fd; padding: 15px; border-radius: 8px;",
          h5("📊 Hasil Prediksi:"),
          verbatimTextOutput("prediction_result"),
          
          div(
            style = "background: #fff; padding: 10px; border-radius: 5px; margin-top: 10px;",
            strong("Catatan:"), " Prediksi hanya akurat dalam rentang data yang digunakan untuk membangun model."
          )
        )
      )
    )
  )
)