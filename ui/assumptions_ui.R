# ui/assumptions_ui.R - VERSI LENGKAP DAN DIPERBAIKI

tagList(
  # Header Section
  box(
    title = "Uji Asumsi Statistik",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    style = "margin-bottom: 20px;",
    
    p("Lakukan uji prasyarat sebelum analisis inferensia, seperti uji normalitas dan homogenitas variansi."),
    
    div(
      style = "background: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 4px solid #2196f3; margin-top: 15px;",
      icon("info-circle", style = "color: #2196f3; margin-right: 8px;"),
      strong(" Catatan Penting:"),
      tags$ul(
        style = "margin-top: 10px; margin-bottom: 0;",
        tags$li("Uji normalitas menggunakan Shapiro-Wilk Test"),
        tags$li("Uji homogenitas menggunakan Levene Test"),
        tags$li("Pastikan data memiliki variabel grup yang valid untuk uji homogenitas")
      )
    )
  ),
  
  # Main Content - Two Column Layout
  fluidRow(
    style = "margin-top: 20px;",
    
    # Column 1: Normality Test
    column(
      width = 6,
      style = "padding-right: 15px;",
      
      box(
        title = "🔬 Uji Normalitas (Shapiro-Wilk)",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        style = "min-height: 900px;",
        
        # Input Section
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
          h5("📊 Pengaturan Uji", style = "margin-top: 0; color: #495057;"),
          uiOutput("normality_variable_selector"),
          
          div(
            style = "text-align: center; margin-top: 15px;",
            actionButton("run_normality_test", 
                         "🚀 Jalankan Uji Normalitas", 
                         class = "btn-success btn-lg", 
                         icon = icon("play"),
                         style = "padding: 10px 30px; font-weight: bold;")
          )
        ),
        
        # Results Section
        div(
          style = "margin-bottom: 20px;",
          h4("📋 Hasil Uji Statistik", style = "color: #495057; border-bottom: 2px solid #dee2e6; padding-bottom: 10px;"),
          div(
            style = "background: #ffffff; padding: 15px; border: 1px solid #dee2e6; border-radius: 8px; margin-top: 15px;",
            verbatimTextOutput("normality_test_result")
          )
        ),
        
        # Q-Q Plot Section
        div(
          style = "margin-bottom: 20px;",
          h4("📈 Plot Q-Q", style = "color: #495057; border-bottom: 2px solid #dee2e6; padding-bottom: 10px;"),
          div(
            style = "background: #ffffff; padding: 15px; border: 1px solid #dee2e6; border-radius: 8px; margin-top: 15px;",
            plotOutput("qq_plot", height = "350px")
          )
        ),
        
        # Download Section
        div(
          style = "background: #e9ecef; padding: 20px; border-radius: 8px; border-top: 3px solid #17a2b8;",
          h4("💾 Unduh Hasil", style = "margin-top: 0; color: #495057;"),
          fluidRow(
            column(7,
                   radioButtons("normality_format", 
                                "Pilih Format Download:", 
                                choices = list(
                                  "📄 PDF" = "pdf", 
                                  "📝 Word (.docx)" = "docx",
                                  "🌐 HTML" = "html"
                                ), 
                                inline = FALSE,
                                selected = "pdf")
            ),
            column(5,
                   div(
                     style = "margin-top: 25px;",
                     downloadButton("download_normality_result", 
                                    "📥 Download Laporan", 
                                    class = "btn-primary btn-lg",
                                    style = "width: 100%; padding: 12px; font-weight: bold;")
                   )
            )
          )
        )
      )
    ),
    
    # Column 2: Homogeneity Test
    column(
      width = 6,
      style = "padding-left: 15px;",
      
      box(
        title = "⚖️ Uji Homogenitas Variansi (Levene)",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        style = "min-height: 900px;",
        
        # Input Section
        div(
          style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
          h5("📊 Pengaturan Uji", style = "margin-top: 0; color: #856404;"),
          uiOutput("homogeneity_variable_selector"),
          uiOutput("homogeneity_group_selector"),
          
          # Status Validasi
          div(
            style = "margin: 15px 0; padding: 12px; background: #f8f9fa; border-radius: 6px; border-left: 4px solid #ffc107;",
            h6("🔍 Status Validasi:", style = "margin-top: 0; color: #856404;"),
            uiOutput("homogeneity_validation_status")
          ),
          
          div(
            style = "text-align: center; margin-top: 15px;",
            actionButton("run_homogeneity_test", 
                         "🚀 Jalankan Uji Homogenitas", 
                         class = "btn-success btn-lg", 
                         icon = icon("play"),
                         style = "padding: 10px 30px; font-weight: bold;")
          )
        ),
        
        # Results Section
        div(
          style = "margin-bottom: 20px;",
          h4("📋 Hasil Uji Statistik", style = "color: #495057; border-bottom: 2px solid #dee2e6; padding-bottom: 10px;"),
          div(
            style = "background: #ffffff; padding: 15px; border: 1px solid #dee2e6; border-radius: 8px; margin-top: 15px; min-height: 300px;",
            verbatimTextOutput("homogeneity_test_result")
          )
        ),
        
        # Download Section
        div(
          style = "background: #e9ecef; padding: 20px; border-radius: 8px; border-top: 3px solid #ffc107; margin-top: auto;",
          h4("💾 Unduh Hasil", style = "margin-top: 0; color: #495057;"),
          fluidRow(
            column(7,
                   radioButtons("homogeneity_format", 
                                "Pilih Format Download:", 
                                choices = list(
                                  "📄 PDF" = "pdf", 
                                  "📝 Word (.docx)" = "docx",
                                  "🌐 HTML" = "html"
                                ), 
                                inline = FALSE,
                                selected = "pdf")
            ),
            column(5,
                   div(
                     style = "margin-top: 25px;",
                     downloadButton("download_homogeneity_result", 
                                    "📥 Download Laporan", 
                                    class = "btn-primary btn-lg",
                                    style = "width: 100%; padding: 12px; font-weight: bold;")
                   )
            )
          )
        )
      )
    )
  ),
  
  # Tips Section
  fluidRow(
    style = "margin-top: 30px;",
    column(12,
           box(
             title = "💡 Tips dan Interpretasi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             collapsed = TRUE,
             
             fluidRow(
               column(6,
                      div(
                        style = "background: #d4edda; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                        h5("📊 Interpretasi Q-Q Plot:", style = "color: #155724; margin-top: 0;"),
                        tags$ul(
                          style = "color: #155724;",
                          tags$li("Titik mengikuti garis diagonal → Data normal"),
                          tags$li("Titik menyimpang dari garis → Data tidak normal"),
                          tags$li("Pola melengkung → Data skewed"),
                          tags$li("Titik tersebar acak → Heteroskedastis")
                        )
                      )
               ),
               column(6,
                      div(
                        style = "background: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                        h5("⚖️ Interpretasi Uji Levene:", style = "color: #856404; margin-top: 0;"),
                        tags$ul(
                          style = "color: #856404;",
                          tags$li("p-value < 0.05 → Variansi tidak homogen"),
                          tags$li("p-value ≥ 0.05 → Variansi homogen"),
                          tags$li("Homogenitas penting untuk ANOVA"),
                          tags$li("Gunakan Welch's t-test jika tidak homogen")
                        )
                      )
               )
             ),
             
             # Additional Tips
             fluidRow(
               column(12,
                      div(
                        style = "background: #d1ecf1; padding: 15px; border-radius: 8px; margin-top: 15px;",
                        h5("🎯 Panduan Interpretasi Lengkap:", style = "color: #0c5460; margin-top: 0;"),
                        
                        div(
                          style = "display: flex; justify-content: space-between; margin-top: 15px;",
                          
                          div(
                            style = "flex: 1; margin-right: 15px;",
                            h6("Shapiro-Wilk Test:", style = "color: #0c5460; margin-bottom: 8px;"),
                            tags$ul(
                              style = "color: #0c5460; font-size: 14px;",
                              tags$li("p > 0.05: Data normal"),
                              tags$li("p ≤ 0.05: Data tidak normal"),
                              tags$li("Efektif untuk n < 5000"),
                              tags$li("Sensitif terhadap outliers")
                            )
                          ),
                          
                          div(
                            style = "flex: 1; margin-left: 15px;",
                            h6("Levene Test:", style = "color: #0c5460; margin-bottom: 8px;"),
                            tags$ul(
                              style = "color: #0c5460; font-size: 14px;",
                              tags$li("p > 0.05: Variansi homogen"),
                              tags$li("p ≤ 0.05: Variansi tidak homogen"),
                              tags$li("Robust terhadap non-normalitas"),
                              tags$li("Prasyarat untuk ANOVA")
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