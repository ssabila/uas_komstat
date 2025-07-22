# server/download_server.R

#   1. LOGIKA DOWNLOAD DATA  
output$download_current_data <- downloadHandler(
  filename = function() {
    paste0("processed_data_", Sys.Date(), ".", input$download_data_format)
  },
  content = function(file) {
    req(processed_data$current)
    
    # Logika untuk menyimpan file berdasarkan format yang dipilih
    if (input$download_data_format == "csv") {
      write.csv(processed_data$current, file, row.names = FALSE)
    } else if (input$download_data_format == "xlsx") {
      tryCatch({
        openxlsx::write.xlsx(processed_data$current, file)
      }, error = function(e) {
        showNotification(paste("Gagal menyimpan file Excel:", e$message), type = "error")
      })
    }
  }
)

#   2. LOGIKA DOWNLOAD LAPORAN LENGKAP SEMUA ANALISIS  
output$download_report <- downloadHandler(
  filename = function() {
    paste0("laporan_analisis_lengkap_", Sys.Date(), ".", input$download_report_format)
  },
  content = function(file) {
    # Validasi bahwa ada minimal satu analisis yang telah dilakukan
    has_analysis <- FALSE
    analysis_count <- 0
    
    # Cek apakah ada hasil analisis yang tersimpan
    if (!is.null(analysis_results$descriptive)) {
      has_analysis <- TRUE
      analysis_count <- analysis_count + 1
    }
    if (!is.null(analysis_results$normality)) {
      has_analysis <- TRUE
      analysis_count <- analysis_count + 1
    }
    if (!is.null(analysis_results$homogeneity)) {
      has_analysis <- TRUE
      analysis_count <- analysis_count + 1
    }
    if (!is.null(analysis_results$ttest)) {
      has_analysis <- TRUE
      analysis_count <- analysis_count + 1
    }
    if (!is.null(analysis_results$proportion)) {
      has_analysis <- TRUE
      analysis_count <- analysis_count + 1
    }
    if (!is.null(analysis_results$variance)) {
      has_analysis <- TRUE
      analysis_count <- analysis_count + 1
    }
    if (!is.null(analysis_results$anova)) {
      has_analysis <- TRUE
      analysis_count <- analysis_count + 1
    }
    if (!is.null(analysis_results$regression)) {
      has_analysis <- TRUE
      analysis_count <- analysis_count + 1
    }
    
    # Jika tidak ada analisis, tampilkan modal peringatan
    if (!has_analysis) {
      showModal(modalDialog(
        title = "Tidak Ada Analisis",
        div(
          h4("Belum ada analisis yang dilakukan"),
          p("Untuk membuat laporan lengkap, Anda perlu melakukan minimal satu analisis terlebih dahulu."),
          br(),
          strong("Jenis analisis yang tersedia:"),
          tags$ul(
            tags$li("Statistik Deskriptif"),
            tags$li("Uji Normalitas"),
            tags$li("Uji Homogenitas"),
            tags$li("Uji-T"),
            tags$li("Uji Proporsi"),
            tags$li("Uji Variansi"),
            tags$li("Analisis ANOVA"),
            tags$li("Analisis Regresi Linear")
          ),
          p("Silakan lakukan analisis pada tab yang sesuai, kemudian kembali ke sini untuk mengunduh laporan lengkap.")
        ),
        easyClose = TRUE,
        footer = modalButton("Tutup")
      ))
      return(NULL)
    }
    
    # Tampilkan notifikasi bahwa laporan sedang dibuat
    showNotification(
      paste("Sedang menyiapkan laporan lengkap dengan", analysis_count, "analisis..."), 
      type = "message", 
      duration = 5
    )
    
    # Siapkan parameter untuk laporan
    params <- list(
      results = reactiveValuesToList(analysis_results)
    )
    
    # Render laporan menggunakan template laporan_lengkap.Rmd
    tryCatch({
      rmarkdown::render(
        input = "laporan_lengkap.Rmd",
        output_file = file,
        output_format = if(input$download_report_format == "pdf") {
          "pdf_document"
        } else {
          "word_document"
        },
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      
      # Notifikasi sukses
      showNotification(
        paste("Laporan lengkap berhasil dibuat! Total", analysis_count, "analisis dengan interpretasi telah disertakan."), 
        type = "message", 
        duration = 8
      )
      
    }, error = function(e) {
      # Notifikasi error dengan detail
      error_msg <- paste("Gagal membuat laporan:", e$message)
      showNotification(error_msg, type = "error", duration = 10)
      
      # Log error untuk debugging
      cat("Error dalam pembuatan laporan:\n")
      cat("Pesan error:", e$message, "\n")
      cat("Jumlah analisis:", analysis_count, "\n")
      cat("Format output:", input$download_report_format, "\n")
      cat("Struktur analysis_results:\n")
      str(reactiveValuesToList(analysis_results))
    })
  }
)

#   3. FUNGSI HELPER UNTUK VALIDASI ANALISIS  
validate_analysis_results <- function() {
  # Fungsi untuk memvalidasi bahwa hasil analisis dalam format yang benar
  valid <- TRUE
  issues <- c()
  
  # Validasi hasil descriptive
  if (!is.null(analysis_results$descriptive)) {
    if (!is.data.frame(analysis_results$descriptive)) {
      valid <- FALSE
      issues <- c(issues, "Hasil statistik deskriptif bukan data frame")
    }
  }
  
  # Validasi hasil normality
  if (!is.null(analysis_results$normality)) {
    if (!is.list(analysis_results$normality) && !inherits(analysis_results$normality, "htest")) {
      valid <- FALSE
      issues <- c(issues, "Hasil uji normalitas bukan dalam format yang valid")
    }
  }
  
  # Validasi hasil homogeneity
  if (!is.null(analysis_results$homogeneity)) {
    if (!is.data.frame(analysis_results$homogeneity) && !is.list(analysis_results$homogeneity)) {
      valid <- FALSE
      issues <- c(issues, "Hasil uji homogenitas bukan dalam format yang valid")
    }
  }
  
  # Validasi hasil ttest
  if (!is.null(analysis_results$ttest)) {
    if (!inherits(analysis_results$ttest, "htest")) {
      valid <- FALSE
      issues <- c(issues, "Hasil uji-t bukan dalam format htest")
    }
  }
  
  # Validasi hasil proportion
  if (!is.null(analysis_results$proportion)) {
    if (!inherits(analysis_results$proportion, "htest")) {
      valid <- FALSE
      issues <- c(issues, "Hasil uji proporsi bukan dalam format htest")
    }
  }
  
  # Validasi hasil variance
  if (!is.null(analysis_results$variance)) {
    if (!inherits(analysis_results$variance, "htest")) {
      valid <- FALSE
      issues <- c(issues, "Hasil uji variansi bukan dalam format htest")
    }
  }
  
  # Validasi hasil anova
  if (!is.null(analysis_results$anova)) {
    if (!inherits(analysis_results$anova, "anova") && !is.data.frame(analysis_results$anova)) {
      valid <- FALSE
      issues <- c(issues, "Hasil ANOVA bukan dalam format yang valid")
    }
  }
  
  # Validasi hasil regression
  if (!is.null(analysis_results$regression)) {
    if (!inherits(analysis_results$regression, "lm") && !is.list(analysis_results$regression)) {
      valid <- FALSE
      issues <- c(issues, "Hasil regresi bukan dalam format yang valid")
    }
  }
  
  return(list(valid = valid, issues = issues))
}

#   4. FUNGSI HELPER UNTUK MEMBUAT RINGKASAN ANALISIS  
create_analysis_summary <- function() {
  # Fungsi untuk membuat ringkasan singkat analisis yang telah dilakukan
  summary_text <- "RINGKASAN ANALISIS YANG TELAH DILAKUKAN:\n\n"
  
  if (!is.null(analysis_results$descriptive)) {
    var_count <- nrow(analysis_results$descriptive)
    summary_text <- paste0(summary_text, "- Statistik Deskriptif: ", var_count, " variabel dianalisis\n")
  }
  
  if (!is.null(analysis_results$normality)) {
    summary_text <- paste0(summary_text, "- Uji Normalitas: Shapiro-Wilk test dilakukan\n")
  }
  
  if (!is.null(analysis_results$homogeneity)) {
    summary_text <- paste0(summary_text, "- Uji Homogenitas: Levene test dilakukan\n")
  }
  
  if (!is.null(analysis_results$ttest)) {
    method <- ifelse(!is.null(analysis_results$ttest$method), 
                     analysis_results$ttest$method, "T-test")
    summary_text <- paste0(summary_text, "- Uji-T: ", method, " dilakukan\n")
  }
  
  if (!is.null(analysis_results$proportion)) {
    summary_text <- paste0(summary_text, "- Uji Proporsi: Test of proportions dilakukan\n")
  }
  
  if (!is.null(analysis_results$variance)) {
    summary_text <- paste0(summary_text, "- Uji Variansi: F-test atau variance test dilakukan\n")
  }
  
  if (!is.null(analysis_results$anova)) {
    summary_text <- paste0(summary_text, "- Analisis ANOVA: Analysis of Variance dilakukan\n")
  }
  
  if (!is.null(analysis_results$regression)) {
    summary_text <- paste0(summary_text, "- Analisis Regresi: Linear regression dilakukan\n")
  }
  
  summary_text <- paste0(summary_text, "\nLaporan ini berisi semua hasil analisis beserta interpretasi lengkap untuk setiap metode yang digunakan.")
  
  return(summary_text)
}

#   5. OBSERVER UNTUK MONITORING STATUS ANALISIS  
# Observer ini akan memantau perubahan pada analysis_results dan memberikan feedback
observeEvent({
  analysis_results$descriptive
  analysis_results$normality
  analysis_results$homogeneity
  analysis_results$ttest
  analysis_results$proportion
  analysis_results$variance
  analysis_results$anova
  analysis_results$regression
}, {
  # Hitung jumlah analisis yang tersedia
  analysis_count <- sum(
    !is.null(analysis_results$descriptive),
    !is.null(analysis_results$normality),
    !is.null(analysis_results$homogeneity),
    !is.null(analysis_results$ttest),
    !is.null(analysis_results$proportion),
    !is.null(analysis_results$variance),
    !is.null(analysis_results$anova),
    !is.null(analysis_results$regression)
  )
}, ignoreInit = TRUE)