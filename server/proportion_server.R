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
      
      # Pastikan ada data yang cukup
      if (total_count == 0) {
        stop("Tidak ada data yang valid untuk dianalisis")
      }
      
      prop.test(x = success_count, n = total_count, p = input$prop_p)
      
    } else if (input$prop_test_type == "two_prop") {
      req(input$prop_group_var)
      
      # Hapus missing values
      clean_data <- data[!is.na(data[[input$prop_var]]) & !is.na(data[[input$prop_group_var]]), ]
      
      if (nrow(clean_data) == 0) {
        stop("Tidak ada data yang valid setelah menghapus missing values")
      }
      
      # Buat tabel kontingensi
      contingency_table <- table(clean_data[[input$prop_var]], clean_data[[input$prop_group_var]])
      
      # Periksa apakah kategori yang dipilih ada dalam tabel
      if (!input$prop_success %in% rownames(contingency_table)) {
        stop(paste("Kategori '", input$prop_success, "' tidak ditemukan dalam data"))
      }
      
      # Periksa apakah ada minimal 2 grup
      if (ncol(contingency_table) < 2) {
        stop("Variabel grup harus memiliki minimal 2 kategori")
      }
      
      # Ambil baris yang sesuai dengan kategori sukses
      success_row <- contingency_table[input$prop_success, ]
      
      # Hitung total untuk setiap grup
      total_counts <- colSums(contingency_table)
      
      # PERBAIKAN UTAMA: Berikan argumen x dan n secara eksplisit
      prop.test(x = as.vector(success_row), 
                n = as.vector(total_counts))
      
    }
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
    return(NULL)
  })
  
  analysis_results$proportion <- result
})


# --- 3. TAMPILKAN HASIL DENGAN INTERPRETASI PROFESIONAL ---
output$prop_test_result_summary <- renderPrint({
  req(analysis_results$proportion)
  result <- analysis_results$proportion
  
  cat("=== HASIL UJI PROPORSI ===\n")
  cat("Variabel yang diuji:", input$prop_var, "\n")
  cat("Kategori yang dianalisis:", input$prop_success, "\n")
  
  if (input$prop_test_type == "one_prop") {
    cat("Jenis uji: One-Proportion Z-test\n")
    cat("\nHipotesis:\n")
    cat(sprintf("H₀: p = %.3f\n", input$prop_p))
    cat(sprintf("H₁: p ≠ %.3f\n", input$prop_p))
  } else {
    cat("Jenis uji: Two-Proportion Z-test\n")
    cat("Variabel grup:", input$prop_group_var, "\n")
    cat("\nHipotesis:\n")
    cat("H₀: p₁ = p₂ (proporsi kedua grup sama)\n")
    cat("H₁: p₁ ≠ p₂ (proporsi kedua grup berbeda)\n")
  }
  
  cat("Tingkat signifikansi (α): 0.05\n\n")
  cat("Hasil Pengujian:\n")
  print(result)
})

output$prop_test_interpretation <- renderText({
  req(analysis_results$proportion)
  result <- analysis_results$proportion
  p_value <- result$p.value
  chi_stat <- result$statistic
  
  interpretation <- "=== INTERPRETASI HASIL UJI PROPORSI ===\n\n"
  
  if (input$prop_test_type == "one_prop") {
    prop_sample <- result$estimate
    prop_hyp <- input$prop_p
    n_success <- result$estimate * sum(!is.na(processed_data$current[[input$prop_var]]))
    n_total <- sum(!is.na(processed_data$current[[input$prop_var]]))
    ci_lower <- result$conf.int[1]
    ci_upper <- result$conf.int[2]
    
    interpretation <- paste0(interpretation,
                             sprintf("Berdasarkan hasil uji proporsi satu sampel, diperoleh:\n"),
                             sprintf("• Statistik chi-square = %.4f\n", chi_stat),
                             sprintf("• P-value = %.6f\n", p_value),
                             sprintf("• Proporsi sampel = %.4f (%.1f%%)\n", prop_sample, prop_sample * 100),
                             sprintf("• Jumlah kategori '%s' = %d dari %d observasi\n", input$prop_success, n_success, n_total),
                             sprintf("• 95%% Confidence Interval: [%.4f, %.4f]\n\n", ci_lower, ci_upper)
    )
    
    if (p_value < 0.05) {
      interpretation <- paste0(interpretation,
                               sprintf("Keputusan: TOLAK H₀\n\n"),
                               sprintf("Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat cukup bukti statistik untuk menyatakan "),
                               sprintf("bahwa proporsi '%s' dalam variabel %s (%.1f%%) berbeda secara signifikan dari nilai ", 
                                       input$prop_success, input$prop_var, prop_sample * 100),
                               sprintf("yang dihipotesiskan (%.1f%%).\n\n", prop_hyp * 100),
                               sprintf("Proporsi yang teramati %.1f%% %s dari nilai hipotesis %.1f%%, ",
                                       prop_sample * 100,
                                       ifelse(prop_sample > prop_hyp, "lebih tinggi", "lebih rendah"),
                                       prop_hyp * 100),
                               sprintf("dengan perbedaan yang signifikan secara statistik (p = %.6f < 0.05).\n\n", p_value),
                               sprintf("Confidence interval 95%% [%.1f%%, %.1f%%] tidak mencakup nilai hipotesis %.1f%%, ",
                                       ci_lower * 100, ci_upper * 100, prop_hyp * 100),
                               sprintf("yang mengkonfirmasi adanya perbedaan signifikan.")
      )
    } else {
      interpretation <- paste0(interpretation,
                               sprintf("Keputusan: GAGAL TOLAK H₀\n\n"),
                               sprintf("Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat cukup bukti statistik untuk "),
                               sprintf("menyatakan bahwa proporsi '%s' dalam variabel %s (%.1f%%) berbeda dari nilai ", 
                                       input$prop_success, input$prop_var, prop_sample * 100),
                               sprintf("yang dihipotesiskan (%.1f%%).\n\n", prop_hyp * 100),
                               sprintf("Meskipun proporsi sampel (%.1f%%) %s dari nilai hipotesis (%.1f%%), ",
                                       prop_sample * 100,
                                       ifelse(prop_sample != prop_hyp,
                                              ifelse(prop_sample > prop_hyp, "sedikit lebih tinggi", "sedikit lebih rendah"),
                                              "sama dengan"),
                                       prop_hyp * 100),
                               sprintf("perbedaan ini tidak signifikan secara statistik (p = %.6f ≥ 0.05).", p_value)
      )
    }
  } else {
    # Two-proportion test
    prop1 <- result$estimate[1]
    prop2 <- result$estimate[2]
    prop_diff <- prop1 - prop2
    ci_lower <- result$conf.int[1]
    ci_upper <- result$conf.int[2]
    
    # Get group names
    data <- processed_data$current
    groups <- unique(data[[input$prop_group_var]])
    
    interpretation <- paste0(interpretation,
                             sprintf("Berdasarkan hasil uji proporsi dua sampel, diperoleh:\n"),
                             sprintf("• Statistik chi-square = %.4f\n", chi_stat),
                             sprintf("• P-value = %.6f\n", p_value),
                             sprintf("• Proporsi '%s' pada grup '%s' = %.4f (%.1f%%)\n", 
                                     input$prop_success, groups[1], prop1, prop1 * 100),
                             sprintf("• Proporsi '%s' pada grup '%s' = %.4f (%.1f%%)\n", 
                                     input$prop_success, groups[2], prop2, prop2 * 100),
                             sprintf("• Selisih proporsi = %.4f (%.1f%%)\n", prop_diff, prop_diff * 100),
                             sprintf("• 95%% Confidence Interval untuk selisih: [%.4f, %.4f]\n\n", ci_lower, ci_upper)
    )
    
    if (p_value < 0.05) {
      interpretation <- paste0(interpretation,
                               sprintf("Keputusan: TOLAK H₀\n\n"),
                               sprintf("Kesimpulan: Dengan tingkat kepercayaan 95%%, terdapat cukup bukti statistik untuk menyatakan "),
                               sprintf("bahwa proporsi '%s' berbeda secara signifikan antara grup '%s' dan grup '%s'.\n\n", 
                                       input$prop_success, groups[1], groups[2]),
                               sprintf("Proporsi pada grup '%s' (%.1f%%) %s dibandingkan dengan grup '%s' (%.1f%%), ",
                                       groups[1], prop1 * 100,
                                       ifelse(prop1 > prop2, "lebih tinggi", "lebih rendah"),
                                       groups[2], prop2 * 100),
                               sprintf("dengan selisih sebesar %.1f%% yang signifikan secara statistik (p = %.6f < 0.05).\n\n", 
                                       abs(prop_diff) * 100, p_value),
                               sprintf("Confidence interval 95%% untuk selisih proporsi [%.1f%%, %.1f%%] tidak mencakup nilai 0, ",
                                       ci_lower * 100, ci_upper * 100),
                               sprintf("yang mengkonfirmasi adanya perbedaan signifikan antara kedua grup.")
      )
    } else {
      interpretation <- paste0(interpretation,
                               sprintf("Keputusan: GAGAL TOLAK H₀\n\n"),
                               sprintf("Kesimpulan: Dengan tingkat kepercayaan 95%%, tidak terdapat cukup bukti statistik untuk "),
                               sprintf("menyatakan bahwa proporsi '%s' berbeda antara grup '%s' dan grup '%s'.\n\n", 
                                       input$prop_success, groups[1], groups[2]),
                               sprintf("Meskipun terdapat selisih proporsi sebesar %.1f%% antara grup '%s' (%.1f%%) ",
                                       abs(prop_diff) * 100, groups[1], prop1 * 100),
                               sprintf("dan grup '%s' (%.1f%%), perbedaan ini tidak signifikan secara statistik ",
                                       groups[2], prop2 * 100),
                               sprintf("(p = %.6f ≥ 0.05).\n\n", p_value),
                               sprintf("Confidence interval 95%% untuk selisih proporsi [%.1f%%, %.1f%%] mencakup nilai 0, ",
                                       ci_lower * 100, ci_upper * 100),
                               sprintf("yang mengindikasikan bahwa perbedaan yang teramati mungkin terjadi karena variasi acak.")
      )
    }
  }
  
  # Tambahkan implikasi praktis
  interpretation <- paste0(interpretation, "\n\n=== Ringkasan ===\n")
  if (p_value < 0.05) {
    interpretation <- paste0(interpretation,
                             "1. Perbedaan proporsi yang ditemukan signifikan secara statistik\n",
                             "2. Evaluasi apakah perbedaan ini juga bermakna secara praktis\n",
                             "3. Pertimbangkan faktor-faktor yang mungkin menyebabkan perbedaan\n",
                             "4. Gunakan temuan ini untuk pengambilan keputusan atau intervensi"
    )
  } else {
    interpretation <- paste0(interpretation,
                             "1. Tidak ada perbedaan proporsi yang signifikan secara statistik\n",
                             "2. Periksa apakah ukuran sampel sudah memadai untuk mendeteksi perbedaan\n",
                             "3. Evaluasi apakah kategori yang dipilih sudah tepat\n",
                             "4. Pertimbangkan analisis lanjutan dengan variabel kontrol"
    )
  }
  
  return(interpretation)
})

# --- 4. LOGIKA UNDUH ---
output$download_proportion_result <- downloadHandler(
  filename = function() {
    paste("hasil-uji-proporsi-", Sys.Date(), ".", input$proportion_format, sep = "")
  },
  content = function(file) {
    req(analysis_results$proportion)
    
    text_to_render <- capture.output({
      cat("=== HASIL ANALISIS UJI PROPORSI ===\n\n")
      print(analysis_results$proportion)
      cat("\n\n")
      cat(output$prop_test_interpretation())
    })
    
    rmarkdown::render(
      input = "text_report.Rmd",
      output_file = file,
      output_format = if(input$proportion_format == "pdf") "pdf_document" else "word_document",
      params = list(
        report_title = "Hasil Uji Proporsi",
        text_output = paste(text_to_render, collapse = "\n")
      ),
      envir = new.env(parent = globalenv())
    )
  }
)