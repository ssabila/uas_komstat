# ui/anova_ui.R
tagList(
  box(
    title = "Analysis of Variance (ANOVA)",
    status = "primary",
    solidHeader = TRUE,
    width = 12,
    p("ANOVA digunakan untuk membandingkan rata-rata dari tiga atau lebih kelompok. Pastikan asumsi normalitas dan homogenitas variansi terpenuhi.")
  ),
  fluidRow(
    column(4,
           box(
             title = "Pengaturan ANOVA",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             selectInput("anova_type", "Pilih Jenis ANOVA:",
                         choices = list("Satu Arah (One-Way)" = "one_way",
                                        "Dua Arah (Two-Way)" = "two_way")),
             uiOutput("anova_dependent_selector"),
             uiOutput("anova_independent1_selector"),
             conditionalPanel(
               condition = "input.anova_type == 'two_way'",
               uiOutput("anova_independent2_selector"),
               checkboxInput("anova_interaction", "Sertakan Efek Interaksi", value = TRUE)
             ),
             actionButton("run_anova", "Jalankan ANOVA", class = "btn-success", icon = icon("play"))
           )
    ),
    column(8,
           box(
             title = "Hasil Analisis ANOVA",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             tabsetPanel(
               tabPanel("Tabel ANOVA",
                        verbatimTextOutput("anova_summary_table"),
                        hr(),
                        h4("Unduh Tabel ANOVA"),
                        radioButtons("anova_format", "Pilih Format:", choices = list("PDF" = "pdf", "Word" = "docx"), inline = TRUE),
                        downloadButton("download_anova_result", "Unduh")
               ),
               tabPanel("Interpretasi", verbatimTextOutput("anova_interpretation")),
               tabPanel("Visualisasi", plotlyOutput("anova_plot"))
             )
           )
    )
  )
)