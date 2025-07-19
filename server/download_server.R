# server/download_server.R

# --- 1. LOGIKA DOWNLOAD DATA ---
output$download_current_data <- downloadHandler(
  filename = function() {
    paste0("processed_data_", Sys.Date(), ".", input$download_data_format)
  },
  content = function(file) {
    # Validasi data
    if (is.null(processed_data$current)) {
      showNotification("Tidak ada data untuk diunduh. Silakan upload data terlebih dahulu.", 
                       type = "error", duration = 5)
      return(NULL)
    }
    
    showNotification("Memproses unduhan data...", type = "message", duration = 3)
    
    tryCatch({
      # Logika untuk menyimpan file berdasarkan format yang dipilih
      if (input$download_data_format == "csv") {
        write.csv(processed_data$current, file, row.names = FALSE)
        showNotification("Data berhasil diunduh dalam format CSV!", type = "success")
      } else if (input$download_data_format == "xlsx") {
        # Cek apakah openxlsx tersedia
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          showNotification("Package 'openxlsx' tidak tersedia. Menggunakan format CSV sebagai alternatif.", 
                           type = "warning")
          write.csv(processed_data$current, file, row.names = FALSE)
        } else {
          openxlsx::write.xlsx(processed_data$current, file)
          showNotification("Data berhasil diunduh dalam format Excel!", type = "success")
        }
      }
    }, error = function(e) {
      showNotification(paste("Gagal menyimpan file:", e$message), type = "error", duration = 10)
      # Fallback: coba simpan sebagai CSV
      tryCatch({
        write.csv(processed_data$current, file, row.names = FALSE)
        showNotification("File disimpan sebagai CSV (fallback)", type = "warning")
      }, error = function(e2) {
        showNotification(paste("Gagal total:", e2$message), type = "error")
      })
    })
  }
)

# --- 2. LOGIKA DOWNLOAD LAPORAN LENGKAP ---
output$download_report <- downloadHandler(
  filename = function() {
    paste0("laporan_analisis_lengkap_", Sys.Date(), ".", input$download_report_format)
  },
  content = function(file) {
    # Validasi: Cek apakah ada analisis yang tersimpan
    if (is.null(analysis_results)) {
      showModal(modalDialog(
        title = "Tidak Ada Analisis",
        "Silakan lakukan setidaknya satu analisis sebelum membuat laporan.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    analysis_list <- reactiveValuesToList(analysis_results)
    if (length(analysis_list) == 0 || all(sapply(analysis_list, is.null))) {
      showModal(modalDialog(
        title = "Tidak Ada Analisis",
        "Silakan lakukan setidaknya satu analisis sebelum membuat laporan.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    showNotification("Sedang membuat laporan lengkap...", type = "message", duration = 5)
    
    tryCatch({
      # Cek keberadaan template
      template_file <- "laporan_lengkap.Rmd"
      if (!file.exists(template_file)) {
        # Buat template sederhana jika tidak ada
        create_simple_template(template_file)
      }
      
      # Persiapan parameter untuk laporan
      params <- list(results = analysis_list)
      
      # Render laporan
      rmarkdown::render(
        input = template_file,
        output_file = file,
        output_format = if(input$download_report_format == "pdf") {
          rmarkdown::pdf_document(latex_engine = "pdflatex")
        } else {
          rmarkdown::word_document()
        },
        params = params,
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
      
      showNotification("Laporan berhasil dibuat!", type = "success", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error membuat laporan:", e$message), type = "error", duration = 10)
      
      # Fallback: buat laporan teks sederhana
      tryCatch({
        create_simple_text_report(file, analysis_list, e$message)
        showNotification("Laporan sederhana berhasil dibuat (fallback)", type = "warning")
      }, error = function(e2) {
        showNotification(paste("Gagal total membuat laporan:", e2$message), type = "error")
      })
    })
  }
)

# --- 3. FUNGSI HELPER UNTUK MEMBUAT TEMPLATE SEDERHANA ---
create_simple_template <- function(filename) {
  template_content <- '---
title: "Laporan Hasil Analisis Statistik"
output:
  word_document: default
  pdf_document: default
params:
  results: NA
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```

# Laporan Analisis Statistik

**Tanggal:** `r format(Sys.time(), "%d %B %Y")`

## Ringkasan Analisis

```{r}
if (!is.null(params$results)) {
  analysis_names <- names(params$results)
  analysis_count <- length(analysis_names[!sapply(params$results, is.null)])
  cat("Total analisis yang dilakukan:", analysis_count, "\\n\\n")
  
  for (name in analysis_names) {
    if (!is.null(params$results[[name]])) {
      cat("✓", tools::toTitleCase(gsub("_", " ", name)), "\\n")
    }
  }
}
```

## Detail Hasil

```{r results="asis"}
if (!is.null(params$results)) {
  for (analysis_name in names(params$results)) {
    result <- params$results[[analysis_name]]
    if (!is.null(result)) {
      cat("\\n### ", tools::toTitleCase(gsub("_", " ", analysis_name)), "\\n\\n")
      cat("```\\n")
      if (is.list(result)) {
        str(result)
      } else {
        print(result)
      }
      cat("```\\n\\n")
    }
  }
}
```
'

writeLines(template_content, filename)
}

# --- 4. FUNGSI HELPER UNTUK LAPORAN TEKS SEDERHANA ---
create_simple_text_report <- function(file, analysis_list, error_msg) {
  report_lines <- c(
    "LAPORAN ANALISIS STATISTIK",
    paste("Tanggal:", Sys.Date()),
    "",
    "=== RINGKASAN ===",
    paste("Total analisis:", length(analysis_list[!sapply(analysis_list, is.null)])),
    "",
    "=== ANALISIS YANG DILAKUKAN ===",
    ""
  )
  
  # Tambahkan daftar analisis
  for (name in names(analysis_list)) {
    if (!is.null(analysis_list[[name]])) {
      report_lines <- c(report_lines, paste("✓", tools::toTitleCase(gsub("_", " ", name))))
    }
  }
  
  report_lines <- c(report_lines, 
                    "",
                    "=== CATATAN ===",
                    "Laporan lengkap tidak dapat dibuat karena:",
                    paste("Error:", error_msg),
                    "",
                    "Silakan hubungi administrator atau coba format lain.",
                    "",
                    paste("Dibuat pada:", format(Sys.time(), "%d %B %Y %H:%M:%S"))
  )
  
  writeLines(report_lines, file)
}

# --- 5. STATUS ANALISIS UNTUK UI ---
output$status_descriptive <- renderUI({
  create_status_ui(analysis_results$descriptive)
})

output$status_normality <- renderUI({
  create_status_ui(analysis_results$normality)
})

output$status_ttest <- renderUI({
  create_status_ui(analysis_results$ttest)
})

output$status_anova <- renderUI({
  create_status_ui(analysis_results$anova)
})

output$status_proportion <- renderUI({
  create_status_ui(analysis_results$proportion)
})

output$status_regression <- renderUI({
  create_status_ui(analysis_results$regression)
})

output$status_homogeneity <- renderUI({
  create_status_ui(analysis_results$homogeneity)
})

output$status_variance <- renderUI({
  create_status_ui(analysis_results$variance)
})

# --- 6. FUNGSI HELPER UNTUK STATUS UI ---
create_status_ui <- function(analysis_result) {
  if (!is.null(analysis_result)) {
    div(icon("check-circle", style = "color: #5cb85c;"), " Selesai", 
        style = "color: #5cb85c; font-weight: bold;")
  } else {
    div(icon("times-circle", style = "color: #d9534f;"), " Belum", 
        style = "color: #d9534f;")
  }
}

# --- 7. HITUNG TOTAL ANALISIS ---
output$total_analyses_count <- renderText({
  if (is.null(analysis_results)) {
    return("0 dari 8 analisis")
  }
  
  analysis_list <- reactiveValuesToList(analysis_results)
  completed_count <- length(analysis_list[!sapply(analysis_list, is.null)])
  paste(completed_count, "dari 8 analisis")
})

# --- 8. VALIDASI DEPENDENCIES ---
observe({
  # Cek apakah rmarkdown tersedia
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    showNotification("Package 'rmarkdown' diperlukan untuk membuat laporan. Install dengan: install.packages('rmarkdown')", 
                     type = "warning", duration = 10)
  }
  
  # Cek apakah knitr tersedia
  if (!requireNamespace("knitr", quietly = TRUE)) {
    showNotification("Package 'knitr' diperlukan untuk membuat laporan. Install dengan: install.packages('knitr')", 
                     type = "warning", duration = 10)
  }
})