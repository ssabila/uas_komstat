# ui/beranda_ui.R
fluidRow(
  # Header dengan informasi utama
  column(12,
         box(
           title = "SOVI EXPLORE : Kerentanan Sosial Indonesia 2017", 
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
               h4("Panduan Lengkap Tersedia!", style = "color: #1976d2; margin-top: 0;"),
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
           color = "navy",
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
           color = "lime",
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
           color = "orange",
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
           color = "maroon",
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
             tags$li("Menyajikan gamabran umum kerentanan sosial melalui visual data yang informatif"),
             tags$li("Menyediakan fitur visualisasi data berupa grafik dan peta untuk memudahkan interpretasi data."),
             tags$li("Melakukan analisis statistik inferensia untuk mengetahui keterbandingan data antar kelompok."),
             tags$li("Mengidentifikasi pengaruh variabel sosial-ekonomi terhadap tingkat kemiskinan menggunakan analisis regresi linear.")
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
             tags$li("SOVI Data: Data kerentanan sosial kabupaten/kota")
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
         # Visualisasi distribusi populasi - PERBAIKAN: Menggunakan plotlyOutput
         box(
           title = "Distribusi Populasi per Wilayah", 
           status = "primary", 
           solidHeader = TRUE,
           width = 12,
           height = "450px",
           div(
             style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
             p(icon("info-circle"), " Pie chart menampilkan distribusi populasi berdasarkan wilayah Indonesia", 
               style = "color: #6c757d; margin: 0; font-size: 14px;")
           ),
           plotlyOutput("population_pie_chart", height = "350px")
         ),
         
         # Quick stats - PERBAIKAN: Box lebih panjang dengan konten yang nyaman dibaca
         box(
           title = "Statistik Cepat",
           status = "secondary",
           solidHeader = TRUE,
           width = 12,
           height = "400px",
           div(
             style = "background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
             p(icon("chart-bar"), " Ringkasan statistik utama dataset SOVI 2017", 
               style = "color: #6c757d; margin: 0; font-size: 14px;")
           ),
           div(
             style = "height: 320px; overflow-y: auto; background: #fff; border: 1px solid #e9ecef; border-radius: 4px; padding: 15px; font-family: 'Courier New', monospace; font-size: 14px; line-height: 1.4;",
             verbatimTextOutput("quick_stats")
           )
         ),
         
         # Top/Bottom Rankings - PERBAIKAN: Box lebih panjang
         box(
           title = "Peringkat Wilayah", 
           status = "primary", 
           solidHeader = TRUE,
           width = 12,
           height = "400px",
           div(
             style = "background: #e8f4f8; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
             p(icon("trophy"), " Ranking kabupaten/kota berdasarkan indikator utama", 
               style = "color: #6c757d; margin: 0; font-size: 14px;")
           ),
           fluidRow(
             column(6,
                    div(
                      style = "background: #d4edda; padding: 12px; border-radius: 5px; margin-bottom: 12px; text-align: center;",
                      h5("🏆 Top 5 Populasi Terbesar", 
                         style = "color: #155724; margin: 0; font-size: 16px; font-weight: bold;")
                    ),
                    div(
                      style = "height: 280px; overflow: hidden;",
                      DT::dataTableOutput("top_population_table")
                    )
             ),
             column(6,
                    div(
                      style = "background: #f8d7da; padding: 12px; border-radius: 5px; margin-bottom: 12px; text-align: center;",
                      h5("⚠️ Top 5 Kemiskinan Tertinggi", 
                         style = "color: #721c24; margin: 0; font-size: 16px; font-weight: bold;")
                    ),
                    div(
                      style = "height: 280px; overflow: hidden;",
                      DT::dataTableOutput("top_poverty_table")
                    )
             )
           )
         )
  )
)