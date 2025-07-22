# server/proportion_server.R

# --- 1. RENDER UI DINAMIS ---
output$prop_variable_selector <- renderUI({
  req(processed_data$current)
  cat_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) is.character(x) || is.factor(x))]
  selectInput("prop_var", "1. Pilih Variabel Kategorik:", choices = cat_vars)
})

output$prop_success_selector <- renderUI({
  req(input$prop_var)
  choices <- unique(processed_data$current[[input$prop_var]])
  selectInput("prop_success", "2. Pilih Kategori yang Akan Dianalisis:", choices = choices)
})

output$prop_group_selector <- renderUI({
  req(input$prop_test_type == "two_prop")
  valid_groups <- names(processed_data$current)[sapply(names(processed_data$current), function(v) {
    (is.factor(processed_data$current[[v]]) || is.character(processed_data$current[[v]])) && length(unique(na.omit(processed_data$current[[v]]))) == 2
  })]
  selectInput("prop_group_var", "Pilih Variabel Grup (dengan 2 kategori):", choices = valid_groups)
})

# --- 2. LOGIKA UJI PROPORSI ---
observeEvent(input$run_prop_test, {
  req(processed_data$current, input$prop_var, input$prop_success)
  data <- processed_data$current
  
  result <- tryCatch({
    if (input$prop_test_type == "one_prop") {
      req(input$prop_p)
      success_count <- sum(data[[input$prop_var]] == input$prop_success, na.rm = TRUE)
      total_count <- sum(!is.na(data[[input$prop_var]]))
      if (total_count == 0) stop("Tidak ada data yang valid untuk dianalisis")
      prop.test(x = success_count, n = total_count, p = input$prop_p)
      
    } else if (input$prop_test_type == "two_prop") {
      req(input$prop_group_var)
      clean_data <- data[!is.na(data[[input$prop_var]]) & !is.na(data[[input$prop_group_var]]), ]
      if (nrow(clean_data) == 0) stop("Tidak ada data yang valid setelah menghapus missing values")
      
      contingency_table <- table(clean_data[[input$prop_var]], clean_data[[input$prop_group_var]])
      if (!input$prop_success %in% rownames(contingency_table)) stop(paste("Kategori '", input$prop_success, "' tidak ditemukan dalam data"))
      if (ncol(contingency_table) < 2) stop("Variabel grup harus memiliki minimal 2 kategori")
      
      success_row <- contingency_table[input$prop_success, ]
      total_counts <- colSums(contingency_table)
      prop.test(x = as.vector(success_row), n = as.vector(total_counts))
    }
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
    return(NULL)
  })
  
  analysis_results$proportion <- result
})

# --- 3. TAMPILKAN HASIL DENGAN INTERPRETASI PROFESIONAL ---

# Fungsi untuk menghasilkan teks interpretasi
generate_prop_interpretation <- function(result, inputs, data) {
  req(result)
  p_value <- result$p.value
  
  # Header
  interpretation <- "Dasar Pengujian:\n"
  
  # Hipotesis
  if (inputs$prop_test_type == "one_prop") {
    interpretation <- paste0(interpretation, sprintf(" • H₀: Proporsi populasi (p) = %.3f\n", inputs$prop_p))
    interpretation <- paste0(interpretation, sprintf(" • H₁: Proporsi populasi (p) ≠ %.3f\n", inputs$prop_p))
  } else {
    interpretation <- paste0(interpretation, " • H₀: Tidak ada perbedaan proporsi antara kedua grup (p₁ = p₂)\n")
    interpretation <- paste0(interpretation, " • H₁: Terdapat perbedaan proporsi antara kedua grup (p₁ ≠ p₂)\n")
  }
  
  # Tingkat Signifikansi
  interpretation <- paste0(interpretation, " • Tingkat Signifikansi (α) = 0.05\n\n")
  interpretation <- paste0(interpretation, "------------------------------------------\n")
  interpretation <- paste0(interpretation, "Keputusan dan Kesimpulan:\n")
  
  # Kesimpulan
  if (inputs$prop_test_type == "one_prop") {
    if (p_value < 0.05) {
      decision <- " • Keputusan: TOLAK H₀.\n • Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat cukup bukti statistik\nuntuk menyatakan bahwa proporsi kategori '%s' (%.1f%%) dalam populasi\nberbeda secara signifikan dari nilai hipotesis (%.1f%%), karena p-value (%.4f) < 0.05."
      interpretation <- paste0(interpretation, sprintf(decision, inputs$prop_success, result$estimate * 100, inputs$prop_p * 100, p_value))
    } else {
      decision <- " • Keputusan: GAGAL TOLAK H₀.\n • Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat cukup bukti statistik\nuntuk menyatakan bahwa proporsi kategori '%s' (%.1f%%) dalam populasi\nberbeda dari nilai hipotesis (%.1f%%), karena p-value (%.4f) ≥ 0.05."
      interpretation <- paste0(interpretation, sprintf(decision, inputs$prop_success, result$estimate * 100, inputs$prop_p * 100, p_value))
    }
  } else {
    req(inputs$prop_group_var)
    groups <- unique(data[[inputs$prop_group_var]])
    if (p_value < 0.05) {
      decision <- " • Keputusan: TOLAK H₀.\n • Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat perbedaan proporsi\nyang signifikan secara statistik untuk kategori '%s' antara grup '%s' dan '%s',\nkarena p-value (%.4f) < 0.05."
      interpretation <- paste0(interpretation, sprintf(decision, inputs$prop_success, groups[1], groups[2], p_value))
    } else {
      decision <- " • Keputusan: GAGAL TOLAK H₀.\n • Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat perbedaan proporsi\nyang signifikan secara statistik untuk kategori '%s' antara grup '%s' dan '%s',\nkarena p-value (%.4f) ≥ 0.05."
      interpretation <- paste0(interpretation, sprintf(decision, inputs$prop_success, groups[1], groups[2], p_value))
    }
  }
  return(interpretation)
}

output$prop_test_result_summary <- renderPrint({
  req(analysis_results$proportion)
  print(analysis_results$proportion)
})

output$prop_test_interpretation <- renderText({
  req(analysis_results$proportion)
  generate_prop_interpretation(analysis_results$proportion, input, processed_data$current)
})

# --- 4. LOGIKA UNDUH BARU ---
output$download_proportion_result <- downloadHandler(
  filename = function() {
    paste("laporan-uji-proporsi-", Sys.Date(), ".", input$proportion_format, sep = "")
  },
  content = function(file) {
    req(analysis_results$proportion)
    
    # Menyiapkan parameter untuk R Markdown
    report_params <- list()
    report_params$report_title <- "Hasil Analisis Uji Proporsi"
    
    # Menyiapkan konten utama laporan
    summary_output <- capture.output(print(analysis_results$proportion))
    interpretation_text <- generate_prop_interpretation(
      analysis_results$proportion, input, processed_data$current
    )
    
    full_report_text <- paste(
      "=== Ringkasan Uji Statistik ===\n",
      paste(summary_output, collapse = "\n"),
      "\n\n=== Interpretasi Hasil Analisis ===\n",
      interpretation_text,
      sep = "\n"
    )
    report_params$text_output <- full_report_text
    
    # Render file R Markdown dengan parameter
    rmarkdown::render(
      input = "text_report.Rmd", 
      output_file = file,
      output_format = if (input$proportion_format == "pdf") "pdf_document" else "word_document",
      params = report_params,
      envir = new.env(parent = globalenv())
    )
  }
)