# --- 1. RENDER UI DINAMIS ---
output$ttest_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("ttest_var", "Pilih Variabel Numerik:", choices = numeric_vars)
})

output$ttest_group_selector <- renderUI({
  req(processed_data$current, input$ttest_type == "two_sample")
  
  # --- PERBAIKAN LOGIKA FILTER ---
  data <- processed_data$current
  is_valid_group <- sapply(names(data), function(col_name) {
    x <- data[[col_name]]
    is_cat <- is.character(x) || is.factor(x)
    if (!is_cat || all(is.na(x))) {
      return(FALSE)
    }
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
    t.test(formula, data = data, alternative = input$ttest_alternative)
  }
  
  analysis_results$ttest <- test_result # Simpan hasil
})

# --- 3. TAMPILKAN HASIL DENGAN INTERPRETASI PROFESIONAL ---
output$ttest_result_summary <- renderPrint({
  req(analysis_results$ttest)
  result <- analysis_results$ttest
  
  cat("=== HASIL UJI T-TEST ===\n")
  cat("Variabel yang diuji:", input$ttest_var, "\n")
  
  if (input$ttest_type == "one_sample") {
    cat("Jenis uji: One-Sample t-test\n")
    cat("\nHipotesis:\n")
    cat(sprintf("H₀: μ = %g\n", input$ttest_mu))
    cat(sprintf("H₁: μ %s %g\n", 
                switch(input$ttest_alternative,
                       "two.sided" = "≠",
                       "less" = "<",
                       "greater" = ">"),
                input$ttest_mu))
  } else {
    cat("Jenis uji: Two-Sample Independent t-test\n")
    cat("Variabel grup:", input$ttest_group_var, "\n")
    cat("\nHipotesis:\n")
    cat("H₀: μ₁ = μ₂ (tidak ada perbedaan rata-rata antar grup)\n")
    cat(sprintf("H₁: μ₁ %s μ₂\n",
                switch(input$ttest_alternative,
                       "two.sided" = "≠",
                       "less" = "<",
                       "greater" = ">")))
  }
  
  cat("Tingkat signifikansi (α): 0.05\n\n")
  cat("Hasil Pengujian:\n")
  print(result)
})

output$ttest_interpretation <- renderText({
  req(analysis_results$ttest)
  result <- analysis_results$ttest
  p_value <- result$p.value
  t_stat <- result$statistic
  df <- result$parameter
  ci_lower <- result$conf.int[1]
  ci_upper <- result$conf.int[2]
  
  interpretation <- "=== INTERPRETASI HASIL UJI T-TEST ===\n\n"
  
  if (input$ttest_type == "one_sample") {
    mean_sample <- result$estimate
    
    interpretation <- paste0(interpretation,
                             sprintf("Berdasarkan hasil uji t satu sampel, diperoleh:\n"),
                             sprintf("• Nilai t-hitung = %.4f\n", t_stat),
                             sprintf("• Derajat bebas (df) = %d\n", df),
                             sprintf("• P-value = %.6f\n", p_value),
                             sprintf("• Rata-rata sampel = %.4f\n", mean_sample),
                             sprintf("• 95%% Confidence Interval: [%.4f, %.4f]\n\n", ci_lower, ci_upper)
    )
    
    if (p_value < 0.05) {
      interpretation <- paste0(interpretation,
                               sprintf("Keputusan: TOLAK H₀\n\n"),
                               sprintf("Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat cukup bukti statistik untuk menyatakan "),
                               sprintf("bahwa rata-rata populasi variabel %s berbeda secara signifikan dari nilai %g.\n\n", 
                                       input$ttest_var, input$ttest_mu),
                               sprintf("Rata-rata sampel (%.4f) %s dari nilai yang dihipotesiskan (%g), ",
                                       mean_sample,
                                       ifelse(mean_sample > input$ttest_mu, "lebih besar", "lebih kecil"),
                                       input$ttest_mu),
                               sprintf("dengan perbedaan yang signifikan secara statistik (p = %.6f < 0.05).", p_value)
      )
    } else {
      interpretation <- paste0(interpretation,
                               sprintf("Keputusan: GAGAL TOLAK H₀\n\n"),
                               sprintf("Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat cukup bukti statistik untuk "),
                               sprintf("menyatakan bahwa rata-rata populasi variabel %s berbeda dari nilai %g.\n\n", 
                                       input$ttest_var, input$ttest_mu),
                               sprintf("Meskipun rata-rata sampel (%.4f) %s dari nilai yang dihipotesiskan (%g), ",
                                       mean_sample,
                                       ifelse(mean_sample != input$ttest_mu, 
                                              ifelse(mean_sample > input$ttest_mu, "sedikit lebih besar", "sedikit lebih kecil"),
                                              "sama dengan"),
                                       input$ttest_mu),
                               sprintf("perbedaan ini tidak signifikan secara statistik (p = %.6f ≥ 0.05).", p_value)
      )
    }
  } else {
    # Two-sample t-test
    mean1 <- result$estimate[1]
    mean2 <- result$estimate[2]
    mean_diff <- mean1 - mean2
    
    # Get group names
    data <- processed_data$current
    groups <- unique(data[[input$ttest_group_var]])
    
    interpretation <- paste0(interpretation,
                             sprintf("Berdasarkan hasil uji t dua sampel independen, diperoleh:\n"),
                             sprintf("• Nilai t-hitung = %.4f\n", t_stat),
                             sprintf("• Derajat bebas (df) = %.2f\n", df),
                             sprintf("• P-value = %.6f\n", p_value),
                             sprintf("• Rata-rata grup '%s' = %.4f\n", groups[1], mean1),
                             sprintf("• Rata-rata grup '%s' = %.4f\n", groups[2], mean2),
                             sprintf("• Selisih rata-rata = %.4f\n", mean_diff),
                             sprintf("• 95%% Confidence Interval untuk selisih: [%.4f, %.4f]\n\n", ci_lower, ci_upper)
    )
    
    if (p_value < 0.05) {
      interpretation <- paste0(interpretation,
                               sprintf("Keputusan: TOLAK H₀\n\n"),
                               sprintf("Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat cukup bukti statistik untuk menyatakan "),
                               sprintf("bahwa terdapat perbedaan yang signifikan antara rata-rata %s pada grup '%s' dan grup '%s'.\n\n", 
                                       input$ttest_var, groups[1], groups[2]),
                               sprintf("Rata-rata %s pada grup '%s' (%.4f) %s dibandingkan dengan grup '%s' (%.4f), ",
                                       input$ttest_var, groups[1], mean1,
                                       ifelse(mean1 > mean2, "lebih tinggi", "lebih rendah"),
                                       groups[2], mean2),
                               sprintf("dengan selisih sebesar %.4f yang signifikan secara statistik (p = %.6f < 0.05).\n\n", 
                                       abs(mean_diff), p_value),
                               sprintf("Confidence interval 95%% untuk selisih rata-rata tidak mencakup nilai 0, "),
                               sprintf("yang mengkonfirmasi adanya perbedaan yang signifikan.")
      )
    } else {
      interpretation <- paste0(interpretation,
                               sprintf("Keputusan: GAGAL TOLAK H₀\n\n"),
                               sprintf("Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat cukup bukti statistik untuk "),
                               sprintf("menyatakan bahwa ada perbedaan signifikan antara rata-rata %s pada grup '%s' dan grup '%s'.\n\n", 
                                       input$ttest_var, groups[1], groups[2]),
                               sprintf("Meskipun terdapat selisih rata-rata sebesar %.4f antara grup '%s' (%.4f) dan grup '%s' (%.4f), ",
                                       abs(mean_diff), groups[1], mean1, groups[2], mean2),
                               sprintf("perbedaan ini tidak signifikan secara statistik (p = %.6f ≥ 0.05).\n\n", p_value),
                               sprintf("Confidence interval 95%% untuk selisih rata-rata mencakup nilai 0, "),
                               sprintf("yang mengindikasikan bahwa perbedaan yang teramati mungkin terjadi karena variasi acak.")
      )
    }
  }
  
  return(interpretation)
})

# --- 4. LOGIKA UNDUH ---
output$download_ttest_result <- downloadHandler(
  filename = function() { paste("hasil-uji-t-", Sys.Date(), ".", input$ttest_format, sep = "") },
  content = function(file) {
    req(analysis_results$ttest)
    text_to_render <- capture.output({
      cat("=== HASIL ANALISIS UJI T-TEST ===\n\n")
      print(analysis_results$ttest)
      cat("\n\n")
      cat(output$ttest_interpretation())
    })
    rmarkdown::render(
      input = "text_report.Rmd", output_file = file,
      output_format = if(input$ttest_format == "pdf") "pdf_document" else "word_document",
      params = list(report_title = "Hasil Analisis Uji-T", text_output = paste(text_to_render, collapse = "\n")),
      envir = new.env(parent = globalenv())
    )
  }
)