# ui/regression_ui.R - COMPATIBLE DENGAN STRUKTUR APP

tagList(
  # Header Box - Sesuai dengan struktur yang ada
  box(
    title = "Analisis Regresi Linear Berganda",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Bangun model regresi untuk memprediksi variabel dependen (Y) berdasarkan satu atau lebih variabel independen (X)."),
    div(
      style = "background: #e3f2fd; padding: 10px; border-radius: 5px; border-left: 4px solid #2196f3;",
      icon("info-circle", style = "color: #2196f3;"),
      strong(" Tips: "),
      "Pastikan data memiliki variabel numerik yang cukup dan periksa asumsi regresi setelah model dibuat."
    )
  ),
  
  # Configuration Box - PERBAIKAN: Struktur yang sederhana seperti modul lain
  box(
    title = "Pengaturan Model Regresi", 
    status = "info",
    solidHeader = TRUE,
    width = 12,
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
    
    # Status validation
    uiOutput("regression_validation_status")
  ),
  
  # Results Box - PERBAIKAN: Menggunakan struktur tabsetPanel yang konsisten
  box(
    title = "Hasil Analisis Regresi",
    status = "success",
    solidHeader = TRUE,
    width = 12,
    
    # PERBAIKAN: TabsetPanel dengan struktur yang sama seperti modul lain
    tabsetPanel(
      id = "regression_results_tabs",
      type = "tabs",
      
      # Tab 1: Model Summary
      tabPanel(
        "Ringkasan Model", 
        icon = icon("list-alt"),
        
        br(),
        # Info box untuk panduan membaca hasil
        div(
          style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          icon("lightbulb", style = "color: #856404;"),
          strong(" Cara Membaca Hasil:", style = "color: #856404;"),
          tags$ul(
            style = "margin-bottom: 0; color: #856404;",
            tags$li(strong("Estimate:"), " Perubahan Y untuk setiap kenaikan 1 unit X"),
            tags$li(strong("Pr(>|t|):"), " Jika < 0.05, variabel berpengaruh signifikan"),
            tags$li(strong("R-squared:"), " Persentase variabilitas Y yang dijelaskan model"),
            tags$li(strong("F-statistic:"), " Signifikansi model secara keseluruhan")
          )
        ),
        
        # Output hasil regresi
        verbatimTextOutput("regression_summary")
      ),
      
      # Tab 2: Model Interpretation
      tabPanel(
        "Interpretasi", 
        icon = icon("comments"),
        
        br(),
        div(
          style = "background: #e8f5e8; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          icon("info-circle", style = "color: #155724;"),
          strong(" Interpretasi Model:", style = "color: #155724;"),
          p("Penjelasan makna praktis dari hasil analisis regresi dalam konteks data Anda.", 
            style = "margin-bottom: 0; color: #155724;")
        ),
        
        verbatimTextOutput("regression_interpretation")
      ),
      
      # Tab 3: Diagnostic Plots
      tabPanel(
        "Uji Asumsi", 
        icon = icon("chart-line"),
        
        br(),
        div(
          style = "background: #f8d7da; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          icon("exclamation-triangle", style = "color: #721c24;"),
          strong(" Penting:", style = "color: #721c24;"),
          p("Hasil regresi hanya valid jika asumsi-asumsi berikut terpenuhi:", 
            style = "color: #721c24; margin-bottom: 5px;"),
          tags$ul(
            style = "margin-bottom: 0; color: #721c24;",
            tags$li("Normalitas residual (Q-Q Plot mendekati garis lurus)"),
            tags$li("Homoskedastisitas (Residual vs Fitted tersebar acak)"),
            tags$li("Tidak ada multikolinearitas (VIF < 10)"),
            tags$li("Linieritas hubungan antar variabel")
          )
        ),
        
        fluidRow(
          column(6,
                 h4("1. Uji Normalitas Residual"),
                 plotOutput("regression_qqplot", height = "400px"),
                 hr(),
                 verbatimTextOutput("normality_residual_interpretation")
          ),
          column(6,
                 h4("2. Uji Homoskedastisitas"),
                 plotOutput("regression_residual_plot", height = "400px"),
                 hr(),
                 verbatimTextOutput("homoskedasticity_interpretation")
          )
        ),
        
        hr(),
        
        fluidRow(
          column(6,
                 h4("3. Uji Multikolinearitas (VIF)"),
                 verbatimTextOutput("regression_vif")
          ),
          column(6,
                 h4("4. Deteksi Outlier"),
                 plotOutput("cooks_distance_plot", height = "300px")
          )
        )
      ),
      
      # Tab 4: Advanced Diagnostics  
      tabPanel(
        "Diagnostik Lanjut",
        icon = icon("search"),
        
        br(),
        p("Analisis lanjutan untuk mendeteksi outlier dan observasi yang berpengaruh terhadap model."),
        
        fluidRow(
          column(6,
                 h4("Cook's Distance"),
                 plotOutput("cooks_distance_plot", height = "350px"),
                 helpText("Mengidentifikasi observasi yang sangat mempengaruhi model (threshold = 1)")
          ),
          column(6,
                 h4("Leverage Values"),
                 plotOutput("leverage_plot", height = "350px"),
                 helpText("Mengidentifikasi observasi dengan nilai X yang ekstrem")
          )
        ),
        
        hr(),
        
        fluidRow(
          column(12,
                 h4("Residuals vs Leverage"),
                 plotOutput("residual_leverage_plot", height = "400px"),
                 helpText("Kombinasi outlier dan leverage untuk identifikasi data berpengaruh")
          )
        )
      )
    )
  )
)