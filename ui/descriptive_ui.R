# ui/descriptive_ui.R
tagList(
  box(
    title = "Statistik Deskriptif",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("Halaman ini menyajikan ringkasan statistik untuk setiap variabel numerik. Gunakan ini untuk memahami tendensi sentral, sebaran, dan bentuk distribusi data Anda.")
  ),
  box(
    title = "Tabel Statistik Deskriptif",
    status = "info",
    solidHeader = TRUE,
    width = 12,
    DT::dataTableOutput("descriptive_table"),
    hr(),
    h4("Unduh Tabel"),
    radioButtons("descriptive_format", "Pilih Format:", choices = list("PDF" = "pdf", "Word" = "docx"), inline = TRUE),
    downloadButton("download_descriptive", "Unduh", class = "btn-success")
  ),
  box(
    title = "Interpretasi Hasil",
    status = "success",
    solidHeader = TRUE,
    width = 12,
    collapsible = TRUE,
    verbatimTextOutput("descriptive_interpretation")
  )
)