# ui/data_management_ui.R - Versi yang diperbaiki untuk mengatasi error tabsetPanel
fluidRow(
  # Header informasi
  column(12,
         box(
           title = "Manajemen dan Transformasi Data", 
           status = "primary", 
           solidHeader = TRUE,
           width = 12,
           div(
             style = "text-align: center; padding: 15px;",
             h3("Kelola Data untuk Analisis Optimal", style = "color: #2c3e50;"),
             p("Transformasi data, deteksi outlier, dan persiapan dataset untuk analisis statistik", 
               style = "font-size: 14px; color: #7f8c8d;"),
             div(
               style = "background: #e8f5e8; padding: 10px; border-radius: 5px; border-left: 4px solid #28a745; margin-top: 10px;",
               icon("info-circle", style = "color: #28a745;"),
               strong(" Fitur: "), 
               "Transformasi matematik, kategorisasi data kontinyu, deteksi outlier, imputasi missing values, dan filter data."
             )
           )
         )
  ),
  
  # Panel kontrol utama
  column(4,
         # Status data
         box(
           title = "Status Data", 
           status = "info", 
           solidHeader = TRUE,
           width = 12,
           verbatimTextOutput("data_info_summary")
         ),
         
         # Kontrol operasi
         box(
           title = "Operasi Data", 
           status = "warning", 
           solidHeader = TRUE,
           width = 12,
           
           # PERBAIKAN: Pastikan tabsetPanel memiliki parameter yang lengkap
           tabsetPanel(
             id = "data_operations_tabs",
             type = "tabs",  # Tambahkan parameter type
             
             # Tab 1: Transformasi
             tabPanel(
               title = "Transformasi", 
               value = "transform_tab",
               br(),
               helpText("Transformasi matematis untuk memperbaiki distribusi data"),
               
               selectInput("transform_var", 
                           "Variabel:", 
                           choices = NULL,
                           selected = NULL),
               
               selectInput("transform_method", 
                           "Metode:",
                           choices = list(
                             "Logaritma (log)" = "log",
                             "Akar kuadrat (sqrt)" = "sqrt", 
                             "Kuadrat (square)" = "square",
                             "Standardisasi (z-score)" = "standardize",
                             "Normalisasi (0-1)" = "normalize"
                           )),
               
               textInput("transform_suffix", 
                         "Suffix nama variabel baru:", 
                         value = ""),
               
               actionButton("btn_transform", 
                            "Terapkan Transformasi", 
                            class = "btn-primary btn-block"),
               
               br(),
               div(id = "transform_result", 
                   style = "margin-top: 10px; padding: 8px; background: #f8f9fa; border-radius: 4px;",
                   "Status: Siap untuk transformasi")
             ),
             
             # Tab 2: Kategorisasi
             tabPanel(
               title = "Kategorisasi",
               value = "categorize_tab", 
               br(),
               helpText("Ubah data kontinyu menjadi kategori diskret"),
               
               selectInput("categorize_var", 
                           "Variabel:", 
                           choices = NULL),
               
               selectInput("categorize_method", 
                           "Metode:",
                           choices = list(
                             "Kuantil (percentile)" = "quantile",
                             "Interval sama (equal width)" = "equal",
                             "Custom breakpoints" = "custom"
                           )),
               
               conditionalPanel(
                 condition = "input.categorize_method != 'custom'",
                 numericInput("categorize_breaks", 
                              "Jumlah kategori:", 
                              value = 3, min = 2, max = 10)
               ),
               
               conditionalPanel(
                 condition = "input.categorize_method == 'custom'",
                 textInput("custom_breaks", 
                           "Breakpoints (pisahkan dengan koma):", 
                           placeholder = "Contoh: 10,20,30,40")
               ),
               
               textInput("categorize_labels", 
                         "Label kategori (opsional, pisahkan dengan koma):", 
                         placeholder = "Contoh: Rendah,Sedang,Tinggi"),
               
               actionButton("btn_categorize", 
                            "Buat Kategori", 
                            class = "btn-success btn-block"),
               
               br(),
               div(id = "categorize_result",
                   style = "margin-top: 10px; padding: 8px; background: #f8f9fa; border-radius: 4px;",
                   "Status: Siap untuk kategorisasi")
             ),
             
             # Tab 3: Outlier & Imputasi  
             tabPanel(
               title = "Outlier & Imputasi",
               value = "outlier_tab",
               br(),
               helpText("Deteksi outlier dan imputasi missing values"),
               
               h5("Deteksi Outlier"),
               selectInput("outlier_var", 
                           "Variabel untuk deteksi outlier:", 
                           choices = NULL),
               
               selectInput("outlier_method", 
                           "Metode deteksi:",
                           choices = list(
                             "IQR (Interquartile Range)" = "iqr",
                             "Z-Score (Standard Score)" = "zscore",
                             "Modified Z-Score" = "modified_zscore"
                           )),
               
               numericInput("outlier_threshold", 
                            "Threshold (untuk Z-Score):", 
                            value = 3, min = 1, max = 5, step = 0.5),
               
               actionButton("btn_detect_outliers", 
                            "Deteksi Outlier", 
                            class = "btn-warning btn-block"),
               
               hr(),
               
               h5("Imputasi Missing Values"),
               selectInput("impute_var", 
                           "Variabel:", 
                           choices = NULL),
               
               selectInput("impute_method", 
                           "Metode imputasi:",
                           choices = list(
                             "Mean (rata-rata)" = "mean",
                             "Median" = "median",
                             "Mode (modus)" = "mode",
                             "Forward fill" = "ffill",
                             "Backward fill" = "bfill"
                           )),
               
               actionButton("btn_impute", 
                            "Terapkan Imputasi", 
                            class = "btn-info btn-block"),
               
               br(),
               div(id = "outlier_result",
                   style = "margin-top: 10px; padding: 8px; background: #f8f9fa; border-radius: 4px;",
                   "Status: Siap untuk deteksi outlier")
             ),
             
             # Tab 4: Filter Data
             tabPanel(
               title = "Filter",
               value = "filter_tab",
               br(),
               helpText("Filter data berdasarkan kriteria"),
               
               selectInput("filter_var", 
                           "Variabel:", 
                           choices = NULL),
               
               # Dynamic filter controls
               uiOutput("dynamic_filter_controls"),
               
               actionButton("btn_filter", 
                            "Terapkan Filter", 
                            class = "btn-info btn-block"),
               
               actionButton("btn_reset_filter", 
                            "Reset Filter", 
                            class = "btn-secondary btn-block"),
               
               br(),
               div(id = "filter_result",
                   style = "margin-top: 10px; padding: 8px; background: #f8f9fa; border-radius: 4px;",
                   "Status: Siap untuk filter")
             )
           )
         ),
         
         # Reset dan Export
         box(
           title = "Kontrol Umum", 
           status = "danger", 
           solidHeader = TRUE,
           width = 12,
           
           actionButton("btn_reset_all", 
                        "Reset Semua Perubahan", 
                        class = "btn-danger btn-block"),
           
           br(),
           
           downloadButton("btn_export_data", 
                          "Export Data", 
                          class = "btn-success btn-block")
         )
  ),
  
  # Panel hasil
  column(8,
         # Preview data
         box(
           title = "Preview Data", 
           status = "primary", 
           solidHeader = TRUE,
           width = 12,
           collapsible = TRUE,
           
           # Summary stats
           fluidRow(
             column(3, valueBoxOutput("vbox_rows", width = NULL)),
             column(3, valueBoxOutput("vbox_cols", width = NULL)),
             column(3, valueBoxOutput("vbox_missing", width = NULL)),
             column(3, valueBoxOutput("vbox_outliers", width = NULL))
           ),
           
           br(),
           DT::dataTableOutput("data_preview_table")
         ),
         
         # Analisis dan visualisasi
         box(
           title = "Analisis Operasi", 
           status = "info", 
           solidHeader = TRUE,
           width = 12,
           collapsible = TRUE,
           
           # PERBAIKAN: Pastikan tabsetPanel memiliki parameter yang lengkap
           tabsetPanel(
             id = "analysis_tabs",
             type = "tabs",  # Tambahkan parameter type
             
             # Statistik deskriptif
             tabPanel(
               title = "Statistik",
               icon = icon("table"),
               br(),
               DT::dataTableOutput("descriptive_stats_table")
             ),
             
             # Visualisasi
             tabPanel(
               title = "Visualisasi",
               icon = icon("chart-bar"),
               br(),
               fluidRow(
                 column(6,
                        selectInput("viz_var", "Pilih variabel untuk visualisasi:", choices = NULL)
                 ),
                 column(6,
                        selectInput("viz_type", "Jenis plot:", 
                                    choices = list("Histogram" = "hist", 
                                                   "Boxplot" = "box",
                                                   "Q-Q Plot" = "qq"))
                 )
               ),
               plotOutput("visualization_plot", height = "400px")
             ),
             
             # Log operasi
             tabPanel(
               title = "Log Operasi",
               icon = icon("list"),
               br(),
               verbatimTextOutput("operation_log")
             ),
             
             # Interpretasi
             tabPanel(
               title = "Interpretasi",
               icon = icon("comment"),
               br(),
               verbatimTextOutput("operation_interpretation")
             )
           )
         )
  )
)