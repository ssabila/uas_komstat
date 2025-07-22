# ui/data_management_ui.R

fluidRow(
  # Header informasi
  column(12,
         bs4Card(
           title = "Manajemen dan Transformasi Data",
           status = "primary",
           solidHeader = TRUE,
           width = 12,
           p("Kelola data dengan transformasi, kategorisasi, deteksi outlier, dan imputasi untuk analisis yang optimal."),
           div(
             style = "background: #e8f5e8; padding: 10px; border-radius: 5px; border-left: 4px solid #28a745; margin-top: 10px;",
             icon("info-circle", style = "color: #28a745;"),
             strong(" Fitur: "),
             "Transformasi matematik, kategorisasi data kontinyu, deteksi outlier, imputasi missing values, dan filter data."
           )
         )
  ),
  
  # Panel kontrol utama
  column(4,
         # Status data
         bs4Card(
           title = "Status Data",
           status = "info",
           solidHeader = TRUE,
           width = 12,
           verbatimTextOutput("data_status_summary")
         ),
         
         # Kontrol operasi
         bs4Card(
           title = "Operasi Data",
           status = "warning",
           solidHeader = TRUE,
           width = 12,
           
           tabsetPanel(
             id = "data_operations_tabs",
             type = "tabs",
             
             # Tab 1: Transformasi
             tabPanel(
               title = "Transformasi",
               icon = icon("magic"),
               value = "transform_tab",
               br(),
               selectInput("transform_var", "Variabel:", choices = NULL),
               selectInput("transform_method", "Metode:",
                           choices = list(
                             "Logaritma (log)" = "log",
                             "Logaritma (log1p)" = "log1p",
                             "Akar kuadrat (sqrt)" = "sqrt",
                             "Kuadrat (square)" = "square",
                             "Inverse (1/x)" = "inverse",
                             "Standardisasi (z-score)" = "standardize",
                             "Normalisasi (0-1)" = "normalize"
                           )),
               textInput("transform_suffix", "Suffix nama variabel baru:", placeholder = "Contoh: _log"),
               actionButton("btn_transform", "Terapkan Transformasi", class = "btn-primary btn-block")
             ),
             
             # Tab 2: Kategorisasi
             tabPanel(
               title = "Kategorisasi",
               icon = icon("object-group"),
               value = "categorize_tab",
               br(),
               selectInput("categorize_var", "Variabel:", choices = NULL),
               selectInput("categorize_method", "Metode:",
                           choices = list(
                             "Kuantil" = "quantile",
                             "Interval Sama" = "equal_width",
                             "Custom" = "custom"
                           )),
               conditionalPanel(
                 condition = "input.categorize_method == 'quantile'",
                 numericInput("quantile_bins", "Jumlah Kategori (Kuantil):", value = 4, min = 2, max = 10)
               ),
               conditionalPanel(
                 condition = "input.categorize_method == 'equal_width'",
                 numericInput("equal_width_bins", "Jumlah Kategori (Interval):", value = 4, min = 2, max = 10)
               ),
               conditionalPanel(
                 condition = "input.categorize_method == 'custom'",
                 textInput("custom_breaks", "Breakpoints (pisahkan koma):", placeholder = "e.g., 0,10,20,30")
               ),
               
               #   PENAMBAHAN FUNGSI PENAMAAN KATEGORI  
               textInput("categorize_labels", "Nama Kategori (opsional, pisahkan koma):", placeholder = "Contoh: Rendah,Sedang,Tinggi"),
               #   AKHIR PENAMBAHAN  
               
               actionButton("btn_categorize", "Buat Kategori", class = "btn-success btn-block")
             ),
             
             # Tab 3: Outlier
             tabPanel("Outlier", icon = icon("search-plus"), value = "outlier_tab", br(),
                      h5("1. Deteksi Outlier"),
                      selectInput("outlier_var", "Variabel:", choices = NULL),
                      selectInput("outlier_method", "Metode Deteksi:",
                                  choices = list("IQR" = "iqr", "Z-Score" = "zscore", "Modified Z-Score" = "modified_zscore")),
                      actionButton("btn_detect_outliers", "Deteksi Outlier", class = "btn-warning btn-block"),
                      hr(),
                      # UI untuk treatment akan muncul di sini secara dinamis
                      uiOutput("outlier_treatment_ui")
             ),
             
             # Tab 4: Filter Data
             tabPanel(
               title = "Filter",
               icon = icon("filter"),
               value = "filter_tab",
               br(),
               selectInput("filter_var", "Variabel:", choices = NULL),
               uiOutput("dynamic_filter_controls"),
               actionButton("btn_filter", "Terapkan Filter", class = "btn-info btn-block"),
               actionButton("btn_reset_filter", "Reset Filter", class = "btn-secondary btn-block")
             )
           )
         ),
         
         # Reset dan Export
         bs4Card(
           title = "Kontrol Umum",
           status = "danger",
           solidHeader = TRUE,
           width = 12,
           actionButton("btn_reset_all", "Reset Semua Perubahan", class = "btn-danger btn-block", icon = icon("undo")),
           br(),
           downloadButton("btn_export_data", "Export Data (.csv)", class = "btn-success btn-block")
         )
  ),
  
  # Panel hasil
  column(8,
         bs4Card(
           title = "Hasil dan Analisis",
           status = "primary",
           solidHeader = TRUE,
           width = 12,
           collapsible = TRUE,
           
           tabsetPanel(
             id = "analysis_tabs",
             
             tabPanel("Preview Data",
                      icon = icon("table"),
                      br(),
                      fluidRow(
                        column(3, valueBoxOutput("vbox_rows", width = NULL)),
                        column(3, valueBoxOutput("vbox_cols", width = NULL)),
                        column(3, valueBoxOutput("vbox_missing", width = NULL)),
                        column(3, valueBoxOutput("vbox_outliers", width = NULL))
                      ),
                      br(),
                      DT::dataTableOutput("data_preview_table")
             ),
             
             tabPanel("Log Operasi",
                      icon = icon("list-alt"),
                      br(),
                      verbatimTextOutput("operation_log")
             )
           )
         )
  )
)
