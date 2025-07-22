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

# --- 1. UI DINAMIS ---
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
    
    # Harus berupa karakter atau faktor
    if (!is.character(x) && !is.factor(x)) {
      return(FALSE)
    }
    
    # Hapus nilai NA untuk pengecekan
    clean_x <- x[!is.na(x)]
    
    # Harus ada data setelah menghapus NA
    if (length(clean_x) == 0) {
      return(FALSE)
    }
    
    # Hitung jumlah level unik
    unique_levels <- unique(clean_x)
    
    # Harus memiliki 2-10 level (tidak terlalu sedikit, tidak terlalu banyak)
    if (length(unique_levels) < 2 || length(unique_levels) > 10) {
      return(FALSE)
    }
    
    # Setiap level harus memiliki minimal 2 observasi
    level_counts <- table(clean_x)
    if (any(level_counts < 2)) {
      return(FALSE)
    }
    
    return(TRUE)
  })
  
  cat_vars <- names(data)[valid_group_vars]
  
  # Jika tidak ada variabel yang valid, berikan pesan
  if (length(cat_vars) == 0) {
    cat_vars <- c("Tidak ada variabel grup yang valid" = "")
  }
  
  selectInput("homogeneity_group", "Pilih Variabel Grup (Kategorik):", choices = cat_vars)
})

# --- 2. LOGIKA UJI ---
normality_results <- eventReactive(input$run_normality_test, {
  req(processed_data$current, input$normality_var)
  
  # Tambahkan error handling
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
  
  # VALIDASI AWAL SEBELUM UJI
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
  
  # Tambahkan error handling yang lebih komprehensif
  result <- tryCatch({
    
    # PRE-CHECK: Pastikan data memenuhi syarat
    data <- processed_data$current
    var <- input$homogeneity_var
    group_var <- input$homogeneity_group
    
    cat("=== DEBUGGING UJI HOMOGENITAS ===\n")
    cat("Variabel dependen:", var, "\n")
    cat("Variabel grup:", group_var, "\n")
    
    # Check apakah variabel ada
    if (!var %in% names(data) || !group_var %in% names(data)) {
      stop("Variabel yang dipilih tidak ditemukan dalam data")
    }
    
    # Check tipe data
    if (!is.numeric(data[[var]])) {
      stop("Variabel dependen harus berupa numerik")
    }
    
    # Check data grup
    group_data <- data[[group_var]]
    cat("Tipe data grup:", class(group_data), "\n")
    cat("Jumlah total observasi:", length(group_data), "\n")
    cat("Jumlah NA di grup:", sum(is.na(group_data)), "\n")
    
    # Clean data untuk analisis
    clean_idx <- complete.cases(data[c(var, group_var)])
    clean_data <- data[clean_idx, ]
    
    cat("Jumlah observasi setelah cleaning:", nrow(clean_data), "\n")
    
    if (nrow(clean_data) < 3) {
      stop("Data tidak cukup untuk analisis (minimal 3 observasi)")
    }
    
    # Check grup
    group_levels <- unique(clean_data[[group_var]])
    cat("Level grup:", paste(group_levels, collapse = ", "), "\n")
    cat("Jumlah level:", length(group_levels), "\n")
    
    if (length(group_levels) < 2) {
      stop("Variabel grup harus memiliki minimal 2 kategori")
    }
    
    # Check ukuran setiap grup
    group_counts <- table(clean_data[[group_var]])
    cat("Ukuran setiap grup:\n")
    print(group_counts)
    
    if (any(group_counts < 2)) {
      stop("Setiap grup harus memiliki minimal 2 observasi")
    }
    
    # Jalankan uji homogenitas
    test_homogeneity(processed_data$current, input$homogeneity_var, input$homogeneity_group, method = "levene")
    
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
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

# --- 3. TAMPILKAN HASIL DENGAN INTERPRETASI PROFESIONAL ---
output$normality_test_result <- renderPrint({
  req(normality_results())
  result <- normality_results()
  
  # Format output yang lebih informatif
  cat("=== HASIL UJI NORMALITAS SHAPIRO-WILK ===\n")
  cat("Variabel yang diuji:", input$normality_var, "\n")
  cat("\nHipotesis:\n")
  cat("H₀: Data berdistribusi normal\n")
  cat("H₁: Data tidak berdistribusi normal\n")
  cat("Tingkat signifikansi (α): 0.05\n\n")
  
  if (!is.na(result$statistic)) {
    cat("Hasil Pengujian:\n")
    cat("Statistik W:", round(result$statistic, 6), "\n")
    cat("P-value:", round(result$p_value, 6), "\n\n")
    
    # Interpretasi profesional
    cat("=== INTERPRETASI ===\n")
    if (result$p_value < 0.05) {
      cat(sprintf("Berdasarkan hasil pengujian Shapiro-Wilk, diperoleh nilai p-value sebesar %.6f (< 0.05), 
sehingga keputusan yang diambil adalah TOLAK H₀. 

Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat cukup bukti untuk menyatakan bahwa 
variabel %s TIDAK berdistribusi normal.", 
                  result$p_value, input$normality_var))
    } else {
      cat(sprintf("Berdasarkan hasil pengujian Shapiro-Wilk, diperoleh nilai p-value sebesar %.6f (≥ 0.05), 
sehingga keputusan yang diambil adalah GAGAL TOLAK H₀. 

Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat cukup bukti untuk menyatakan 
bahwa variabel %s tidak berdistribusi normal. Data dapat diasumsikan berdistribusi normal.

Catatan: Hasil ini mendukung penggunaan metode statistik parametrik untuk analisis lanjutan.", 
                  result$p_value, input$normality_var))
    }
  } else {
    cat("Hasil pengujian tidak tersedia.")
  }
})

# Perbaiki output Q-Q plot
output$qq_plot <- renderPlot({
  req(processed_data$current, input$normality_var)
  
  tryCatch({
    # Pastikan fungsi create_qqplot tersedia
    if (!exists("create_qqplot")) {
      source("modules/visualization_functions.R")
    }
    
    create_qqplot(processed_data$current, input$normality_var)
    
  }, error = function(e) {
    # Fallback error handling
    data_col <- processed_data$current[[input$normality_var]]
    clean_data <- data_col[!is.na(data_col)]
    
    if(length(clean_data) > 0) {
      # Create simple Q-Q plot
      par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
      qqnorm(clean_data, 
             main = paste("Q-Q Plot:", input$normality_var),
             xlab = "Theoretical Quantiles", 
             ylab = "Sample Quantiles",
             pch = 16, col = "steelblue")
      qqline(clean_data, col = "red", lwd = 2)
      
      # Add grid
      grid(col = "lightgray", lty = "dotted")
      
      # Add interpretation text
      cor_test <- cor.test(qnorm(ppoints(length(clean_data))), sort(clean_data))
      r_value <- cor_test$estimate
      
      legend("topleft", 
             legend = c(
               paste("n =", length(clean_data)),
               paste("r =", round(r_value, 3)),
               if(r_value > 0.95) "Normal" else if(r_value > 0.90) "Mendekati Normal" else "Tidak Normal"
             ),
             bty = "n", cex = 0.9)
      
    } else {
      # Show error message
      plot(1, 1, type = "n", 
           main = "Error: Tidak dapat membuat Q-Q plot",
           xlab = "", ylab = "", axes = FALSE)
      text(1, 1, paste("Error:", e$message), col = "red", cex = 1.2)
      box()
    }
  })
}, height = 400)

# reactive value untuk menyimpan plot
qq_plot_data <- reactiveVal(NULL)

# Update observeEvent untuk menyimpan plot data
observeEvent(input$run_normality_test, {
  req(processed_data$current, input$normality_var)
  
  # Simpan data untuk plot
  qq_plot_data(list(
    data = processed_data$current,
    variable = input$normality_var,
    timestamp = Sys.time()
  ))
})

output$homogeneity_test_result <- renderPrint({
  req(homogeneity_results())
  result <- homogeneity_results()
  
  # Format output yang lebih informatif
  cat("=== HASIL UJI HOMOGENITAS VARIANSI (LEVENE TEST) ===\n")
  cat("Variabel dependen:", input$homogeneity_var, "\n")
  cat("Variabel grup:", input$homogeneity_group, "\n")
  cat("\nHipotesis:\n")
  cat("H₀: Variansi antar grup homogen (sama)\n")
  cat("H₁: Minimal terdapat satu grup dengan variansi yang berbeda\n")
  cat("Tingkat signifikansi (α): 0.05\n\n")
  
  if (!is.na(result$statistic)) {
    cat("Hasil Pengujian:\n")
    cat("Statistik F:", round(result$statistic, 6), "\n")
    
    if ("df1" %in% names(result) && !is.na(result$df1)) {
      cat("Derajat bebas 1:", result$df1, "\n")
      cat("Derajat bebas 2:", result$df2, "\n")
    }
    
    cat("P-value:", round(result$p_value, 6), "\n")
    
    if ("n_total" %in% names(result)) {
      cat("Jumlah observasi:", result$n_total, "\n")
      cat("Jumlah grup:", result$n_groups, "\n")
    }
    
    cat("\n=== INTERPRETASI ===\n")
    if (result$p_value < 0.05) {
      cat(sprintf("Berdasarkan hasil uji Levene, diperoleh nilai F = %.4f dengan p-value = %.6f (< 0.05), 
sehingga keputusan yang diambil adalah TOLAK H₀.

Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat cukup bukti untuk menyatakan bahwa 
variansi variabel %s TIDAK HOMOGEN antar grup %s.", 
                  result$statistic, result$p_value, input$homogeneity_var, input$homogeneity_group))
    } else {
      cat(sprintf("Berdasarkan hasil uji Levene, diperoleh nilai F = %.4f dengan p-value = %.6f (≥ 0.05), 
sehingga keputusan yang diambil adalah GAGAL TOLAK H₀.

Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat cukup bukti untuk menyatakan 
bahwa variansi variabel %s berbeda antar grup %s. Variansi dapat diasumsikan homogen.

Catatan: Hasil ini mendukung penggunaan metode ANOVA dan t-test standar untuk analisis lanjutan.", 
                  result$statistic, result$p_value, input$homogeneity_var, input$homogeneity_group))
    }
  } else {
    cat("\nHasil pengujian tidak tersedia atau terjadi error dalam analisis.")
  }
})

# --- 4. STATUS VALIDASI HOMOGENITAS ---
output$homogeneity_validation_status <- renderUI({
  validation <- homogeneity_validation()
  
  if (!is.null(validation)) {
    if (validation$valid) {
      div(
        icon("check-circle", style = "color: #28a745;"),
        span("Data siap untuk diuji", style = "color: #28a745;"),
        br(),
        span(paste("Total observasi:", validation$n_total), style = "font-size: 12px;"),
        br(),
        span(paste("Jumlah grup:", validation$n_groups), style = "font-size: 12px;")
      )
    } else {
      div(
        icon("exclamation-triangle", style = "color: #dc3545;"),
        span("Data belum siap:", style = "color: #dc3545;"),
        br(),
        tags$ul(
          style = "font-size: 12px; margin-top: 5px;",
          lapply(validation$errors, function(err) tags$li(err))
        )
      )
    }
  } else {
    div(
      icon("info-circle", style = "color: #6c757d;"),
      span("Pilih variabel untuk validasi", style = "color: #6c757d;")
    )
  }
})

# --- 5. LOGIKA UNDUH ---
# Update download handler untuk mendukung HTML
output$download_normality_result <- downloadHandler(
  filename = function() { 
    ext <- switch(input$normality_format,
                  "pdf" = "pdf",
                  "docx" = "docx", 
                  "html" = "html")
    paste("hasil-uji-normalitas-", Sys.Date(), ".", ext, sep = "") 
  },
  content = function(file) {
    req(analysis_results$normality)
    
    # Buat temporary directory untuk file
    temp_dir <- tempdir()
    plot_file <- file.path(temp_dir, "qq_plot.png")
    
    # Simpan Q-Q plot sebagai file
    tryCatch({
      if (!is.null(qq_plot_data())) {
        qq_plot <- create_qqplot(qq_plot_data()$data, qq_plot_data()$variable)
        ggsave(plot_file, plot = qq_plot, width = 10, height = 6, dpi = 300, bg = "white")
      }
    }, error = function(e) {
      # Create simple base R plot if ggplot fails
      png(plot_file, width = 800, height = 600, res = 150, bg = "white")
      data_col <- qq_plot_data()$data[[qq_plot_data()$variable]]
      clean_data <- data_col[!is.na(data_col)]
      
      if(length(clean_data) > 0) {
        qqnorm(clean_data, main = paste("Q-Q Plot:", qq_plot_data()$variable))
        qqline(clean_data, col = "red", lwd = 2)
      }
      dev.off()
    })
    
    result <- analysis_results$normality
    
    # Tentukan output format berdasarkan pilihan user
    output_format <- switch(input$normality_format,
                            "pdf" = "pdf_document",
                            "docx" = "word_document", 
                            "html" = "html_document")
    
    # Create temporary Rmd file
    temp_rmd <- tempfile(fileext = ".Rmd")
    
    # Konten Rmd yang diperbaiki untuk semua format
    rmd_content <- paste0(
      "---\n",
      "title: 'Hasil Uji Normalitas Shapiro-Wilk'\n",
      "date: '", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "'\n",
      "output:\n",
      if(input$normality_format == "pdf") {
        "  pdf_document:\n    latex_engine: xelatex\n    toc: true\n    number_sections: true\n"
      } else if(input$normality_format == "docx") {
        "  word_document:\n    toc: true\n    number_sections: true\n"
      } else {
        "  html_document:\n    theme: flatly\n    toc: true\n    toc_float: true\n    number_sections: true\n    df_print: paged\n"
      },
      "---\n\n",
      
      # CSS untuk HTML
      if(input$normality_format == "html") {
        paste0(
          "<style>\n",
          "body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }\n",
          ".header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 20px; border-radius: 10px; margin-bottom: 20px; }\n",
          ".result-box { background: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin: 15px 0; border-radius: 5px; }\n",
          ".interpretation { background: #d4edda; padding: 15px; border-left: 4px solid #28a745; border-radius: 5px; }\n",
          ".plot-container { text-align: center; margin: 20px 0; }\n",
          "img { max-width: 100%; height: auto; border: 1px solid #dee2e6; border-radius: 8px; }\n",
          "</style>\n\n"
        )
      } else "",
      
      # Header yang menarik untuk HTML
      if(input$normality_format == "html") {
        paste0(
          "<div class='header'>\n",
          "<h1 style='margin:0;'>📊 Hasil Uji Normalitas Shapiro-Wilk</h1>\n",
          "<p style='margin:5px 0 0 0; opacity: 0.9;'>Analisis Statistik - ", format(Sys.time(), "%d %B %Y"), "</p>\n",
          "</div>\n\n"
        )
      } else "",
      
      "## 📋 Informasi Uji\n\n",
      "- **Variabel yang diuji:** ", input$normality_var, "\n",
      "- **Tanggal analisis:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
      "- **Jenis uji:** Shapiro-Wilk Test\n",
      "- **Software:** R Statistical Computing\n\n",
      
      "## 🎯 Hipotesis\n\n",
      "- **H₀ (Hipotesis Nol):** Data berdistribusi normal\n",
      "- **H₁ (Hipotesis Alternatif):** Data tidak berdistribusi normal\n",
      "- **Tingkat signifikansi (α):** 0.05\n\n",
      
      if(input$normality_format == "html") "<div class='result-box'>\n" else "",
      "## 📊 Hasil Pengujian\n\n",
      if (!is.na(result$statistic)) paste("- **Statistik W:** ", round(result$statistic, 6), "\n") else "",
      if (!is.na(result$p_value)) paste("- **P-value:** ", round(result$p_value, 6), "\n") else "",
      "- **Jumlah observasi:** ", length(qq_plot_data()$data[[input$normality_var]][!is.na(qq_plot_data()$data[[input$normality_var]])]), "\n\n",
      if(input$normality_format == "html") "</div>\n\n" else "",
      
      "## 📈 Q-Q Plot\n\n",
      if(input$normality_format == "html") "<div class='plot-container'>\n" else "",
      if(file.exists(plot_file)) {
        if(input$normality_format == "html") {
          paste0("![Q-Q Plot](", plot_file, "){width=80%}\n\n")
        } else {
          paste0("![Q-Q Plot](", plot_file, ")\n\n")
        }
      } else "",
      if(input$normality_format == "html") "</div>\n\n" else "",
      
      if(input$normality_format == "html") "<div class='interpretation'>\n" else "",
      "## 🔍 Interpretasi\n\n",
      result$interpretation, "\n\n",
      if(input$normality_format == "html") "</div>\n\n" else "",
      
      "## 📝 Kesimpulan\n\n",
      if(result$p_value < 0.05) {
        paste0("Berdasarkan hasil uji Shapiro-Wilk dengan **p-value = ", round(result$p_value, 6), 
               " < 0.05**, keputusan yang diambil adalah **TOLAK H₀**. ",
               "Dengan tingkat kepercayaan 95%, data variabel '", input$normality_var, "' **TIDAK berdistribusi normal**.\n\n",
               "**Rekomendasi:**\n",
               "- Gunakan uji statistik non-parametrik\n",
               "- Pertimbangkan transformasi data\n",
               "- Hati-hati dalam interpretasi hasil regresi linear")
      } else {
        paste0("Berdasarkan hasil uji Shapiro-Wilk dengan **p-value = ", round(result$p_value, 6), 
               " ≥ 0.05**, keputusan yang diambil adalah **GAGAL TOLAK H₀**. ",
               "Dengan tingkat kepercayaan 95%, data variabel '", input$normality_var, "' dapat diasumsikan **berdistribusi normal**.\n\n",
               "**Rekomendasi:**\n",
               "- Aman menggunakan uji statistik parametrik\n",
               "- Dapat melanjutkan dengan analisis regresi linear\n",
               "- Sesuai untuk uji t-test dan ANOVA")
      },
      "\n\n",
      
      "---\n\n",
      "*Laporan ini dibuat secara otomatis oleh SOVI Dashboard pada ", format(Sys.time(), "%d %B %Y pukul %H:%M:%S"), "*"
    )
    
    # Write Rmd content
    writeLines(rmd_content, temp_rmd)
    
    # Render laporan
    tryCatch({
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        output_format = output_format,
        quiet = TRUE,
        envir = new.env(parent = globalenv())
      )
      
      showNotification("Laporan berhasil dibuat!", type = "success", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error saat membuat laporan:", e$message), type = "error")
      
      # Fallback: create simple text file
      simple_content <- paste(
        "=== HASIL UJI NORMALITAS SHAPIRO-WILK ===",
        paste("Variabel yang diuji:", input$normality_var),
        paste("Tanggal:", Sys.Date()),
        "",
        "Hasil Pengujian:",
        if (!is.na(result$statistic)) paste("Statistik W:", round(result$statistic, 6)) else "",
        if (!is.na(result$p_value)) paste("P-value:", round(result$p_value, 6)) else "",
        "",
        "Interpretasi:",
        result$interpretation,
        sep = "\n"
      )
      writeLines(simple_content, file)
    })
    
    # Clean up temporary files
    if(file.exists(plot_file)) unlink(plot_file)
    if(file.exists(temp_rmd)) unlink(temp_rmd)
  }
)

output$download_homogeneity_result <- downloadHandler(
  filename = function() { 
    paste("hasil-uji-homogenitas-", Sys.Date(), ".", input$homogeneity_format, sep = "") 
  },
  content = function(file) {
    req(analysis_results$homogeneity)
    
    result <- analysis_results$homogeneity
    text_content <- paste(
      "=== HASIL UJI HOMOGENITAS VARIANSI (LEVENE TEST) ===",
      paste("Variabel dependen:", input$homogeneity_var),
      paste("Variabel grup:", input$homogeneity_group),
      "",
      "Hipotesis:",
      "H₀: Variansi antar grup homogen (sama)",
      "H₁: Minimal terdapat satu grup dengan variansi yang berbeda",
      "Tingkat signifikansi (α): 0.05",
      "",
      "Hasil Pengujian:",
      if (!is.na(result$statistic)) paste("Statistik F:", round(result$statistic, 6)) else "",
      if (!is.na(result$p_value)) paste("P-value:", round(result$p_value, 6)) else "",
      "",
      "Interpretasi:",
      result$interpretation,
      sep = "\n"
    )
    
    tryCatch({
      rmarkdown::render(
        input = "text_report.Rmd", 
        output_file = file,
        output_format = if(input$homogeneity_format == "pdf") "pdf_document" else "word_document",
        params = list(
          report_title = "Hasil Uji Homogenitas Variansi", 
          text_output = text_content
        ),
        envir = new.env(parent = globalenv())
      )
    }, error = function(e) {
      showNotification(paste("Error saat membuat laporan:", e$message), type = "error")
    })
  }
)

# Helper Function
validate_homogeneity_inputs <- function(data, dep_var, group_var) {
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

# Validasi Reactive
homogeneity_validation <- reactive({
  req(processed_data$current, input$homogeneity_var, input$homogeneity_group)
  
  if (input$homogeneity_group == "" || input$homogeneity_group == "Tidak ada variabel grup yang valid") {
    return(list(valid = FALSE, errors = "Pilih variabel grup yang valid"))
  }
  
  validate_homogeneity_inputs(processed_data$current, input$homogeneity_var, input$homogeneity_group)
})