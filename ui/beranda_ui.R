# ui/beranda_ui.R - Versi yang diperbaiki dengan status valid
fluidRow(
  # Header dengan informasi utama
  column(12,
         box(
           title = "Selamat Datang di Dashboard Kerentanan Sosial Indonesia 2017", 
           status = "primary", 
           solidHeader = TRUE,
           width = 12,
           div(
             style = "text-align: center; padding: 20px;",
             h3("Analisis Komprehensif Data Kerentanan Sosial", style = "color: #3c8dbc;"),
             p("Dashboard interaktif untuk menganalisis faktor-faktor kerentanan sosial di Indonesia berdasarkan data tahun 2017", 
               style = "font-size: 16px; margin-top: 15px;"),
             div(
               style = "background: #e3f2fd; padding: 15px; border-radius: 8px; margin-top: 15px;",
               h4("📚 Panduan Lengkap Tersedia!", style = "color: #1976d2; margin-top: 0;"),
               p(style = "color: #1976d2;", "Kunjungi tab "),
               actionButton("goto_guide", "Panduan Pengguna", 
                            class = "btn-info btn-sm",
                            style = "margin: 0 5px;"),
               p(style = "color: #1976d2; display: inline;", " untuk tutorial lengkap penggunaan dashboard.")
             )
           )
         )
  ),
  
  # Info boxes dengan statistik utama
  column(3,
         infoBox(
           title = "Total Kabupaten/Kota",
           value = textOutput("total_districts"),
           subtitle = "Wilayah yang dianalisis",
           icon = icon("map"),
           color = "lightblue",  # Ganti dari "blue" ke "lightblue"
           width = 12,
           fill = TRUE
         )
  ),
  column(3,
         infoBox(
           title = "Total Provinsi",
           value = textOutput("total_provinces"),
           subtitle = "Provinsi di Indonesia",
           icon = icon("map-marker-alt"),
           color = "lime",  # Ganti dari "green" ke "lime"
           width = 12,
           fill = TRUE
         )
  ),
  column(3,
         infoBox(
           title = "Total Populasi",
           value = textOutput("total_population"),
           subtitle = "Jumlah penduduk (juta jiwa)",
           icon = icon("users"),
           color = "orange",  # Ganti dari "yellow" ke "orange"
           width = 12,
           fill = TRUE
         )
  ),
  column(3,
         infoBox(
           title = "Rata-rata Kemiskinan",
           value = textOutput("average_poverty"),
           subtitle = "Persentase kemiskinan nasional",
           icon = icon("chart-line"),
           color = "maroon",  # Ganti dari "red" ke "maroon"
           width = 12,
           fill = TRUE
         )
  ),
  
  # Konten utama beranda
  column(6,
         # Tujuan dan deskripsi
         box(
           title = "Tujuan Analisis", 
           status = "info", 
           solidHeader = TRUE,
           width = 12,
           tags$ul(
             tags$li("Mengidentifikasi pola dan distribusi kerentanan sosial di Indonesia"),
             tags$li("Menganalisis hubungan antar variabel sosial ekonomi"),
             tags$li("Melakukan uji statistik untuk validasi hipotesis"),
             tags$li("Memberikan rekomendasi kebijakan berdasarkan temuan analisis"),
             tags$li("Menyediakan visualisasi interaktif untuk eksplorasi data"),
             tags$li(strong("BARU:"), " Analisis perbandingan antar provinsi dan pulau")
           )
         ),
         
         # Sumber data
         box(
           title = "Sumber Data", 
           status = "success", 
           solidHeader = TRUE,
           width = 12,
           tags$p(tags$strong("Dataset Utama:")),
           tags$ul(
             tags$li("SOVI Data: Data kerentanan sosial kabupaten/kota"),
             tags$li("Distance Matrix: Matriks jarak antar wilayah")
           ),
           tags$p(tags$strong("Variabel Geografis Baru:")),
           tags$ul(
             tags$li("Kode & Nama Provinsi berdasarkan BPS"),
             tags$li("Kelompok Pulau (7 kategori)"),
             tags$li("Wilayah Indonesia (3 kategori)")
           ),
           tags$p(tags$strong("Sumber:")),
           tags$a("GitHub - naspaclust", 
                  href = "https://github.com/bmlmcmc/naspaclust", 
                  target = "_blank")
         ),
         
         # Informasi variabel
         box(
           title = "Variabel Utama", 
           status = "warning", 
           solidHeader = TRUE,
           width = 12,
           DT::dataTableOutput("variable_info_table")
         )
  ),
  
  # Sidebar dengan visualisasi geografis
  column(6,
         # Visualisasi geografis
         box(
           title = "Distribusi Populasi", 
           status = "primary", 
           solidHeader = TRUE,
           width = 12,
           plotlyOutput("population_pie_chart", height = "400px")
         ),
         
         # Quick stats
         box(
           title = "Statistik Cepat",
           status = "secondary",
           solidHeader = TRUE,
           width = 12,
           verbatimTextOutput("quick_stats")
         )
  )
)