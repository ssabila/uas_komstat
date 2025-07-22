check_and_load_packages <- function() {
  required_packages <- c("ggplot2", "car", "rmarkdown", "knitr")
  
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Installing package:", pkg))
      install.packages(pkg, dependencies = TRUE)
    }
    
    if(!pkg %in% loadedNamespaces()) {
      library(pkg, character.only = TRUE)
    }
  }
}

#   1. UI DINAMIS  
output$normality_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("normality_var", "Pilih Variabel (Numerik):", choices = numeric_vars)
})

output$homogeneity_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("homogeneity_var", "Pilih Variabel Dependen (Numerik):", choices = numeric_vars)
})

output$homogeneity_group_selector <- renderUI({
  req(processed_data$current)
  
  data <- processed_data$current
  
  valid_group_vars <- sapply(names(data), function(col_name) {
    x <- data[[col_name]]
    if (!is.character(x) && !is.factor(x)) return(FALSE)
    clean_x <- x[!is.na(x)]
    if (length(clean_x) == 0) return(FALSE)
    unique_levels <- unique(clean_x)
    if (length(unique_levels) < 2 || length(unique_levels) > 10) return(FALSE)
    level_counts <- table(clean_x)
    if (any(level_counts < 2)) return(FALSE)
    return(TRUE)
  })
  
  cat_vars <- names(data)[valid_group_vars]
  
  if (length(cat_vars) == 0) {
    cat_vars <- c("Tidak ada variabel grup yang valid" = "")
  }
  
  selectInput("homogeneity_group", "Pilih Variabel Grup (Kategorik):", choices = cat_vars)
})

#   2. LOGIKA UJI  
normality_results <- eventReactive(input$run_normality_test, {
  req(processed_data$current, input$normality_var)
  
  result <- tryCatch({
    test_normality(processed_data$current, input$normality_var, method = "shapiro")
  }, error = function(e) {
    showNotification(paste("Error pada uji normalitas:", e$message), type = "error")
    return(list(
      test = "Shapiro-Wilk",
      statistic = NA,
      p_value = NA,
      interpretation = paste("Error:", e$message)
    ))
  })
  
  analysis_results$normality <- result
  return(result)
})

homogeneity_results <- eventReactive(input$run_homogeneity_test, {
  req(processed_data$current, input$homogeneity_var, input$homogeneity_group)
  
  if (input$homogeneity_group == "" || input$homogeneity_group == "Tidak ada variabel grup yang valid") {
    result <- list(
      test = "Levene Test",
      statistic = NA,
      p_value = NA,
      interpretation = "Silakan pilih variabel grup yang valid terlebih dahulu"
    )
    analysis_results$homogeneity <- result
    return(result)
  }
  
  result <- tryCatch({
    test_homogeneity(processed_data$current, input$homogeneity_var, input$homogeneity_group, method = "levene")
  }, error = function(e) {
    showNotification(paste("Error pada uji homogenitas:", e$message), type = "error", duration = 10)
    return(list(
      test = "Levene Test",
      statistic = NA,
      p_value = NA,
      interpretation = paste("Error:", e$message)
    ))
  })
  
  analysis_results$homogeneity <- result
  return(result)
})

#   3. TAMPILKAN HASIL  
output$normality_test_result <- renderPrint({
  req(normality_results())
  result <- normality_results()
  
  cat("  HASIL UJI NORMALITAS SHAPIRO-WILK  \n")
  cat("Variabel yang diuji:", input$normality_var, "\n")
  cat("\nHipotesis:\n")
  cat("H₀: Data berdistribusi normal\n")
  cat("H₁: Data tidak berdistribusi normal\n")
  cat("Tingkat signifikansi (α): 0.05\n\n")
  
  if (!is.na(result$statistic)) {
    cat("Hasil Pengujian:\n")
    cat("Statistik W:", round(result$statistic, 6), "\n")
    cat("P-value:", format.pval(result$p_value, eps = 1e-6, digits = 6), "\n\n")
    cat("  INTERPRETASI  \n")
    if (result$p_value < 0.05) {
      cat(sprintf("Dengan p-value = %s (< 0.05), H₀ ditolak. Terdapat cukup bukti untuk menyatakan bahwa data variabel '%s' TIDAK berdistribusi normal.", 
                  format.pval(result$p_value, eps = 1e-6, digits = 6), input$normality_var))
    } else {
      cat(sprintf("Dengan p-value = %s (≥ 0.05), H₀ gagal ditolak. Tidak cukup bukti untuk menyatakan bahwa data variabel '%s' tidak berdistribusi normal. Asumsi normalitas terpenuhi.", 
                  format.pval(result$p_value, eps = 1e-6, digits = 6), input$normality_var))
    }
  } else {
    cat("Hasil pengujian tidak tersedia atau terjadi error.")
  }
})

output$qq_plot <- renderPlot({
  req(processed_data$current, input$normality_var)
  
  tryCatch({
    create_qqplot(processed_data$current, input$normality_var)
  }, error = function(e) {
    plot(1, 1, type = "n", main = "Error Membuat Plot", xlab = "", ylab = "", axes = FALSE)
    text(1, 1, paste("Error:", e$message), col = "red", cex = 1.2)
  })
}, height = 400)

output$homogeneity_test_result <- renderPrint({
  req(homogeneity_results())
  result <- homogeneity_results()
  
  cat("  HASIL UJI HOMOGENITAS VARIANSI (LEVENE TEST)  \n")
  cat("Variabel dependen:", input$homogeneity_var, "\n")
  cat("Variabel grup:", input$homogeneity_group, "\n")
  cat("\nHipotesis:\n")
  cat("H₀: Variansi antar grup adalah homogen (sama)\n")
  cat("H₁: Variansi antar grup tidak homogen (berbeda)\n")
  cat("Tingkat signifikansi (α): 0.05\n\n")
  
  if (!is.na(result$statistic)) {
    cat("Hasil Pengujian:\n")
    cat("Statistik F:", round(result$statistic, 6), "\n")
    cat("P-value:", format.pval(result$p_value, eps = 1e-6, digits = 6), "\n\n")
    cat("  INTERPRETASI  \n")
    if (result$p_value < 0.05) {
      cat(sprintf("Dengan p-value = %s (< 0.05), H₀ ditolak. Terdapat cukup bukti untuk menyatakan bahwa variansi variabel '%s' TIDAK HOMOGEN antar grup '%s'.", 
                  format.pval(result$p_value, eps = 1e-6, digits = 6), input$homogeneity_var, input$homogeneity_group))
    } else {
      cat(sprintf("Dengan p-value = %s (≥ 0.05), H₀ gagal ditolak. Tidak cukup bukti untuk menyatakan bahwa variansi berbeda. Asumsi homogenitas variansi TERPENUHI.", 
                  format.pval(result$p_value, eps = 1e-6, digits = 6), input$homogeneity_var, input$homogeneity_group))
    }
  } else {
    cat("Hasil pengujian tidak tersedia atau terjadi error.")
  }
})

#   4. LOGIKA UNDUH  

# ##   UJI NORMALITAS   ##
output$download_normality_result <- downloadHandler(
  filename = function() { 
    ext <- switch(input$normality_format, "pdf" = "pdf", "docx" = "docx", "html" = "html")
    paste0("hasil-uji-normalitas-", Sys.Date(), ".", ext) 
  },
  content = function(file) {
    req(analysis_results$normality)
    
    temp_dir <- tempdir()
    plot_file <- file.path(temp_dir, "qq_plot.png")
    
    tryCatch({
      qq_plot_obj <- create_qqplot(processed_data$current, input$normality_var)
      ggsave(plot_file, plot = qq_plot_obj, width = 8, height = 6, dpi = 300, bg = "white")
    }, error = function(e) {
      png(plot_file, width = 800, height = 600, res = 150)
      data_col <- processed_data$current[[input$normality_var]]
      qqnorm(data_col[!is.na(data_col)], main = paste("Q-Q Plot:", input$normality_var))
      qqline(data_col[!is.na(data_col)], col = "red", lwd = 2)
      dev.off()
    })
    
    result <- analysis_results$normality
    output_format <- switch(input$normality_format,
                            "pdf" = "pdf_document",
                            "docx" = "word_document")
    
    temp_rmd <- tempfile(fileext = ".Rmd")
    
    rmd_content <- paste(
      " ",
      "title: 'Laporan Hasil Uji Normalitas Shapiro-Wilk'",
      paste0("date: '", format(Sys.time(), "%A, %d %B %Y %H:%M:%S"), "'"),
      "output:",
      if(input$normality_format == "pdf") {
        "  pdf_document:\n    latex_engine: xelatex\n    toc: true"
      } else if(input$normality_format == "docx") {
        "  word_document:\n    toc: true"
      },
      " ",
      "",
      "## 1. Ringkasan Analisis",
      paste("- **Variabel yang diuji:**", input$normality_var),
      paste("- **Jenis Uji:** Shapiro-Wilk"),
      paste("- **Tingkat Signifikansi (α):** 0.05"),
      "",
      "## 2. Hipotesis",
      "- **H₀ (Hipotesis Nol):** Data berdistribusi normal.",
      "- **H₁ (Hipotesis Alternatif):** Data tidak berdistribusi normal.",
      "",
      "## 3. Hasil Pengujian",
      if (!is.na(result$statistic)) paste("- **Statistik W:**", round(result$statistic, 6)) else "",
      if (!is.na(result$p_value)) paste("- **P-value:**", format.pval(result$p_value, eps = 1e-6, digits = 6)) else "",
      "",
      "## 4. Plot Diagnostik",
      "### Quantile-Quantile (Q-Q) Plot",
      "Plot Q-Q digunakan untuk membandingkan distribusi data sampel dengan distribusi normal teoritis. Jika data berdistribusi normal, titik-titik akan mendekati garis lurus.",
      "",
      if(file.exists(plot_file)) paste0("![Q-Q Plot untuk ", input$normality_var, "](", plot_file, ")") else "Plot tidak tersedia.",
      "",
      "## 5. Interpretasi dan Kesimpulan",
      if(result$p_value < 0.05) {
        paste0("Berdasarkan hasil uji Shapiro-Wilk, diperoleh **p-value = ", format.pval(result$p_value, eps = 1e-6, digits = 6), " (< 0.05)**. ",
               "Oleh karena itu, **keputusan yang diambil adalah menolak H₀**. ",
               "Dengan tingkat kepercayaan 95%, terdapat cukup bukti statistik untuk menyimpulkan bahwa data pada variabel **'", input$normality_var, "' TIDAK berdistribusi normal**.",
               "")
      } else {
        paste0("Berdasarkan hasil uji Shapiro-Wilk, diperoleh **p-value = ", format.pval(result$p_value, eps = 1e-6, digits = 6), " (≥ 0.05)**. ",
               "Oleh karena itu, **keputusan yang diambil adalah gagal menolak H₀**. ",
               "Dengan tingkat kepercayaan 95%, tidak terdapat cukup bukti statistik untuk menyatakan bahwa data pada variabel **'", input$normality_var, "' tidak berdistribusi normal**. Asumsi normalitas dapat dianggap terpenuhi.",
               "")
      },
      "",
      " ",
      paste0("*Laporan ini dibuat secara otomatis pada ", format(Sys.time(), "%d %B %Y, %H:%M:%S"), ".*"),
      sep = "\n"
    )
    
    writeLines(rmd_content, temp_rmd)
    
    tryCatch({
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        output_format = output_format,
        quiet = TRUE
      )
      showNotification("Laporan berhasil dibuat!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error membuat laporan:", e$message), type = "error")
    })
    
    if(file.exists(plot_file)) unlink(plot_file)
    if(file.exists(temp_rmd)) unlink(temp_rmd)
  }
)

# ##   UJI HOMOGENITAS
output$download_homogeneity_result <- downloadHandler(
  filename = function() { 
    ext <- switch(input$homogeneity_format, "pdf" = "pdf", "docx" = "docx", "html" = "html")
    paste0("hasil-uji-homogenitas-", Sys.Date(), ".", ext) 
  },
  content = function(file) {
    req(analysis_results$homogeneity)
    
    result <- analysis_results$homogeneity
    
    # Menentukan format output untuk rmarkdown::render
    output_format <- switch(input$homogeneity_format,
                            "pdf" = "pdf_document",
                            "docx" = "word_document")
    
    # Membuat file Rmd sementara
    temp_rmd <- tempfile(fileext = ".Rmd")
    
    # Membuat konten R Markdown secara dinamis
    rmd_content <- paste(
      " ",
      "title: 'Laporan Hasil Uji Homogenitas Variansi (Levene Test)'",
      paste0("date: '", format(Sys.time(), "%A, %d %B %Y %H:%M:%S"), "'"),
      "output:",
      if(input$homogeneity_format == "pdf") {
        "  pdf_document:\n    latex_engine: xelatex\n    toc: true"
      } else if(input$homogeneity_format == "docx") {
        "  word_document:\n    toc: true"
      },
      " ",
      "",
      "## 1. Ringkasan Analisis",
      paste("- **Variabel Dependen:**", input$homogeneity_var),
      paste("- **Variabel Grup:**", input$homogeneity_group),
      paste("- **Jenis Uji:** Levene Test"),
      paste("- **Tingkat Signifikansi (α):** 0.05"),
      "",
      "## 2. Hipotesis",
      "- **H₀ (Hipotesis Nol):** Variansi variabel dependen adalah sama (homogen) di semua grup.",
      "- **H₁ (Hipotesis Alternatif):** Setidaknya ada satu grup yang memiliki variansi yang berbeda (tidak homogen).",
      "",
      "## 3. Hasil Pengujian",
      if (!is.na(result$statistic)) paste("- **Statistik F:**", round(result$statistic, 6)) else "Statistik F: Tidak tersedia",
      if ("df1" %in% names(result) && !is.na(result$df1)) paste("- **Derajat Bebas (df1, df2):**", result$df1, ",", result$df2) else "",
      if (!is.na(result$p_value)) paste("- **P-value:**", format.pval(result$p_value, eps = 1e-6, digits = 6)) else "P-value: Tidak tersedia",
      "",
      "## 4. Interpretasi dan Kesimpulan",
      if(is.na(result$p_value) || is.na(result$statistic)){
        "**Analisis tidak dapat diselesaikan.** Pastikan variabel yang dipilih sudah benar dan data memenuhi syarat untuk pengujian."
      } else if(result$p_value < 0.05) {
        paste0("Berdasarkan hasil Levene Test, diperoleh **p-value = ", format.pval(result$p_value, eps = 1e-6, digits = 6), " (< 0.05)**. ",
               "Oleh karena itu, **keputusan yang diambil adalah menolak H₀**. ",
               "Dengan tingkat kepercayaan 95%, terdapat cukup bukti statistik untuk menyimpulkan bahwa variansi variabel **'", input$homogeneity_var, "' TIDAK HOMOGEN** di antara grup pada variabel **'", input$homogeneity_group, "'**.",
               "")
      } else {
        paste0("Berdasarkan hasil Levene Test, diperoleh **p-value = ", format.pval(result$p_value, eps = 1e-6, digits = 6), " (≥ 0.05)**. ",
               "Oleh karena itu, **keputusan yang diambil adalah gagal menolak H₀**. ",
               "Dengan tingkat kepercayaan 95%, tidak terdapat cukup bukti statistik untuk menyatakan bahwa variansi variabel **'", input$homogeneity_var, "' berbeda** di antara grup pada variabel **'", input$homogeneity_group, "'**. Asumsi homogenitas variansi **dapat dianggap terpenuhi**.",
               "")
      },
      "",
      " ",
      paste0("*Laporan ini dibuat secara otomatis pada ", format(Sys.time(), "%d %B %Y, %H:%M:%S"), ".*"),
      sep = "\n"
    )
    
    # Menulis konten ke file Rmd sementara
    writeLines(rmd_content, temp_rmd)
    
    # Merender laporan
    tryCatch({
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        output_format = output_format,
        quiet = TRUE
      )
      showNotification("Laporan berhasil dibuat!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error saat membuat laporan:", e$message), type = "error", duration = 10)
    })
    
    # Membersihkan file sementara
    if(file.exists(temp_rmd)) unlink(temp_rmd)
  }
)

#   VALIDASI & STATUS  


validate_homogeneity_inputs <- validate_homogeneity_inputs <- function(data, dep_var, group_var) {
  errors <- character(0)
  
  # Check if variables exist
  if (!dep_var %in% names(data)) {
    errors <- c(errors, "Variabel dependen tidak ditemukan")
  }
  
  if (!group_var %in% names(data)) {
    errors <- c(errors, "Variabel grup tidak ditemukan")
  }
  
  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = errors))
  }
  
  # Check data types
  if (!is.numeric(data[[dep_var]])) {
    errors <- c(errors, "Variabel dependen harus numerik")
  }
  
  group_data <- data[[group_var]]
  if (!is.character(group_data) && !is.factor(group_data)) {
    errors <- c(errors, "Variabel grup harus kategorik")
  }
  
  # Check for sufficient data
  complete_idx <- complete.cases(data[c(dep_var, group_var)])
  clean_data <- data[complete_idx, ]
  
  if (nrow(clean_data) < 3) {
    errors <- c(errors, "Data tidak cukup (minimal 3 observasi)")
  }
  
  # Check group levels
  if (nrow(clean_data) >= 3) {
    group_levels <- unique(clean_data[[group_var]])
    if (length(group_levels) < 2) {
      errors <- c(errors, "Minimal 2 grup diperlukan")
    }
    
    # Check observations per group
    group_counts <- table(clean_data[[group_var]])
    if (any(group_counts < 2)) {
      errors <- c(errors, "Setiap grup harus memiliki minimal 2 observasi")
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    n_total = if(exists("clean_data")) nrow(clean_data) else 0,
    n_groups = if(exists("group_levels")) length(group_levels) else 0
  ))
}

homogeneity_validation <- reactive({
  req(processed_data$current, input$homogeneity_var, input$homogeneity_group)
  if (input$homogeneity_group == "" || input$homogeneity_group == "Tidak ada variabel grup yang valid") {
    return(list(valid = FALSE, errors = "Pilih variabel grup yang valid"))
  }
  validate_homogeneity_inputs(processed_data$current, input$homogeneity_var, input$homogeneity_group)
})

output$homogeneity_validation_status <- renderUI({
  validation <- homogeneity_validation()
  if (validation$valid) {
    div(icon("check-circle", style = "color: #28a745;"), span("Data siap untuk diuji", style = "color: #28a745;"))
  } else {
    div(icon("exclamation-triangle", style = "color: #dc3545;"),
        span("Data belum siap:", style = "color: #dc3545;"),
        tags$ul(style = "font-size: 12px; margin-top: 5px;", lapply(validation$errors, tags$li)))
  }
})