# --- 1. LOGIKA DOWNLOAD DATA ---
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

# --- 2. LOGIKA DOWNLOAD LAPORAN ---
output$download_report <- downloadHandler(
  filename = function() {
    # --- PERUBAHAN DI SINI ---
    paste0("laporan_analisis_", Sys.Date(), ".", input$download_report_format)
    # --- AKHIR PERUBAHAN ---
  },
  content = function(file) {
    if (is.null(analysis_results)) {
      showModal(modalDialog(
        title = "Tidak Ada Analisis",
        "Silakan lakukan setidaknya satu analisis sebelum membuat laporan.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    params <- list(results = reactiveValuesToList(analysis_results))
    showNotification("Sedang membuat laporan...", type = "message", duration = 5)
    
    tryCatch({
      # --- PERUBAHAN DI SINI ---
      rmarkdown::render(
        input = "laporan_lengkap.Rmd",
        output_file = file,
        output_format = if(input$download_report_format == "pdf") "pdf_document" else "word_document",
        params = params,
        envir = new.env(parent = globalenv())
      )
      # --- AKHIR PERUBAHAN ---
    }, error = function(e) {
      showNotification(paste("Gagal membuat laporan:", e$message), type = "error", duration = 10)
    })
  }
)