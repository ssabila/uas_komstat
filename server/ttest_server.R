# server/ttest_server.R

# --- 1. RENDER UI DINAMIS ---
output$ttest_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("ttest_var", "Pilih Variabel Numerik:", choices = numeric_vars)
})

output$ttest_group_selector <- renderUI({
  req(processed_data$current, input$ttest_type == "two_sample")
  
  data <- processed_data$current
  is_valid_group <- sapply(names(data), function(col_name) {
    x <- data[[col_name]]
    is_cat <- is.character(x) || is.factor(x)
    if (!is_cat || all(is.na(x))) return(FALSE)
    return(length(unique(na.omit(x))) == 2)
  })
  valid_groups <- names(data)[is_valid_group]
  
  selectInput("ttest_group_var", "Pilih Variabel Grup (2 Kategori):", choices = valid_groups)
})

# --- 2. LOGIKA UJI T-TEST ---
observeEvent(input$run_ttest, {
  req(processed_data$current, input$ttest_var, input$ttest_type, input$ttest_alternative)
  data <- processed_data$current
  variable <- data[[input$ttest_var]]
  
  test_result <- if (input$ttest_type == "one_sample") {
    req(input$ttest_mu)
    t.test(variable, mu = input$ttest_mu, alternative = input$ttest_alternative)
  } else if (input$ttest_type == "two_sample") {
    req(input$ttest_group_var)
    formula <- as.formula(paste(input$ttest_var, "~", input$ttest_group_var))
    t.test(formula, data = data, alternative = input$ttest_alternative, var.equal = TRUE)
  }
  
  analysis_results$ttest <- test_result
})

# --- 3. TAMPILKAN HASIL DENGAN INTERPRETASI  ---
output$ttest_result_summary <- renderPrint({
  req(analysis_results$ttest)
  if (analysis_results$ttest$method == "One Sample t-test" && input$ttest_type != "one_sample") return()
  if (grepl("Two Sample", analysis_results$ttest$method) && input$ttest_type != "two_sample") return()
  
  result <- analysis_results$ttest
  
  cat("=== Ringkasan Uji Statistik ===\n")
  if (input$ttest_type == "one_sample") {
    cat(sprintf("Jenis Uji: One-Sample t-test\n"))
  } else {
    cat(sprintf("Jenis Uji: Independent Two-Sample t-test (Student's)\n"))
  }
  cat("----------------------------------------------------\n")
  print(result)
})

# --- FUNGSI INTERPRETASI  ---
generate_ttest_interpretation <- function(result, inputs, data) {
  req(result)
  p_value <- result$p.value
  
  interpretation <- "=== INTERPRETASI HASIL UJI T-TEST ===\n\n"
  interpretation <- paste0(interpretation, "Dasar Pengujian:\n")
  
  if (inputs$ttest_type == "one_sample") {
    h0 <- sprintf(" • H₀: Rata-rata populasi (μ) = %g\n", inputs$ttest_mu)
    h1_symbol <- switch(inputs$ttest_alternative, "two.sided" = "≠", "less" = "<", "greater" = ">")
    h1 <- sprintf(" • H₁: Rata-rata populasi (μ) %s %g\n", h1_symbol, inputs$ttest_mu)
    interpretation <- paste0(interpretation, h0, h1)
  } else {
    req(inputs$ttest_group_var)
    h0 <- " • H₀: Tidak ada perbedaan rata-rata antara kedua grup (μ₁ = μ₂)\n"
    h1_symbol <- switch(inputs$ttest_alternative, "two.sided" = "≠", "less" = "<", "greater" = ">")
    h1 <- sprintf(" • H₁: Terdapat perbedaan rata-rata antara kedua grup (μ₁ %s μ₂)\n", h1_symbol)
    interpretation <- paste0(interpretation, h0, h1)
  }
  
  interpretation <- paste0(interpretation, sprintf(" • Tingkat Signifikansi (α) = 0.05\n\n"))
  interpretation <- paste0(interpretation, "------------------------------------------\n")
  interpretation <- paste0(interpretation, "Keputusan dan Kesimpulan:\n")
  
  if (inputs$ttest_type == "one_sample") {
    # --- PERBAIKAN WRAPPING TEXT ---
    if (p_value < 0.05) {
      decision <- " • Keputusan: TOLAK H₀.\n • Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat cukup bukti statistik\nuntuk menyatakan bahwa rata-rata populasi variabel '%s' berbeda secara signifikan\ndari nilai hipotesis (%g), karena nilai p-value (%.4f) lebih kecil dari α (0.05)."
      interpretation <- paste0(interpretation, sprintf(decision, inputs$ttest_var, inputs$ttest_mu, p_value))
    } else {
      decision <- " • Keputusan: GAGAL TOLAK H₀.\n • Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat cukup bukti statistik\nuntuk menyatakan bahwa rata-rata populasi variabel '%s' berbeda secara signifikan\ndari nilai hipotesis (%g), karena nilai p-value (%.4f) lebih besar atau sama dengan α (0.05)."
      interpretation <- paste0(interpretation, sprintf(decision, inputs$ttest_var, inputs$ttest_mu, p_value))
    }
  } else {
    # --- PERBAIKAN WRAPPING TEXT ---
    groups <- unique(na.omit(data[[inputs$ttest_group_var]]))
    if (p_value < 0.05) {
      decision <- " • Keputusan: TOLAK H₀.\n • Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat perbedaan rata-rata\nyang signifikan secara statistik untuk variabel '%s' antara grup '%s' dan '%s',\nkarena nilai p-value (%.4f) lebih kecil dari α (0.05)."
      interpretation <- paste0(interpretation, sprintf(decision, inputs$ttest_var, groups[1], groups[2], p_value))
    } else {
      decision <- " • Keputusan: GAGAL TOLAK H₀.\n • Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat perbedaan rata-rata\nyang signifikan secara statistik untuk variabel '%s' antara grup '%s' dan '%s',\nkarena nilai p-value (%.4f) lebih besar atau sama dengan α (0.05)."
      interpretation <- paste0(interpretation, sprintf(decision, inputs$ttest_var, groups[1], groups[2], p_value))
    }
  }
  return(interpretation)
}

output$ttest_interpretation <- renderText({
  req(analysis_results$ttest)
  if (analysis_results$ttest$method == "One Sample t-test" && input$ttest_type != "one_sample") return()
  if (grepl("Two Sample", analysis_results$ttest$method) && input$ttest_type != "two_sample") return()
  generate_ttest_interpretation(analysis_results$ttest, input, processed_data$current)
})

# --- LOGIKA UNDUH ---
output$download_ttest_result <- downloadHandler(
  filename = function() {
    paste("laporan-uji-t-", Sys.Date(), ".", input$ttest_format, sep = "")
  },
  content = function(file) {
    req(analysis_results$ttest)
    
    report_params <- list()
    report_params$report_title <- "Hasil Analisis Uji T-Test"
    
    summary_output <- capture.output(print(analysis_results$ttest))
    interpretation_text <- generate_ttest_interpretation(
      analysis_results$ttest, input, processed_data$current
    )
    
    full_report_text <- paste(
      "=== Ringkasan Uji Statistik ===\n",
      paste(summary_output, collapse = "\n"),
      "\n\n",
      interpretation_text,
      sep = "\n"
    )
    report_params$text_output <- full_report_text
    
    rmarkdown::render(
      input = "text_report.Rmd",
      output_file = file,
      output_format = if (input$ttest_format == "pdf") "pdf_document" else "word_document",
      params = report_params,
      envir = new.env(parent = globalenv())
    )
  }
)