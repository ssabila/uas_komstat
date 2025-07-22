# ui/download_ui.R
fluidPage(
  # Header
  box(
    title = "Pusat Unduhan",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Unduh data, hasil analisis, atau laporan lengkap dalam berbagai format.")
  ),
  
  fluidRow(
    # Panel Download Data
    column(6,
           box(
             title = "Unduh Data",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             h4("Unduh Dataset Saat Ini"),
             p("Dataset ini mencakup semua transformasi dan filter yang telah Anda terapkan."),
             
             selectInput("download_data_format", "Pilih Format:",
                         choices = list("CSV" = "csv", "Excel (.xlsx)" = "xlsx")),
             
             downloadButton("download_current_data", "Unduh Data", class = "btn-primary")
           )
    ),
    
    # Panel Download Laporan
    column(6,
           box(
             title = "Unduh Laporan",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             h4("Buat dan Unduh Laporan Analisis"),
             p("Fitur ini akan menghasilkan laporan lengkap dari semua analisis yang telah dilakukan."),
             
             selectInput("download_report_format", "Pilih Format Laporan:",
                         choices = list("PDF" = "pdf", "Word" = "docx")),
             
             downloadButton("download_report", "Unduh Laporan", class = "btn-success")
           )
    )
  )
)