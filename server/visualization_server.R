# server/visualization_server.R - Versi yang Diperbaiki

# Selektor dinamis untuk UI
output$vis_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("vis_var", "Pilih Variabel Utama:", choices = numeric_vars)
})

output$vis_group_selector <- renderUI({
  req(processed_data$current)
  categorical_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) is.character(x) || is.factor(x))]
  selectInput("vis_group", "Pilih Variabel Grup (Opsional):", choices = c("Tidak Ada", categorical_vars))
})

output$vis_plot_type_selector <- renderUI({
  selectInput("vis_plot_type", "Pilih Jenis Plot:", choices = c("Histogram", "Boxplot", "Scatter Plot"))
})

# Render plot utama
output$main_plot <- renderPlotly({
  req(input$vis_var, input$vis_plot_type, processed_data$current)
  
  tryCatch({
    data <- processed_data$current
    
    # Validasi data
    if(is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Validasi variabel
    if(!input$vis_var %in% names(data)) {
      return(NULL)
    }
    
    p <- switch(input$vis_plot_type,
                "Histogram" = {
                  create_histogram(data, input$vis_var)
                },
                "Boxplot" = {
                  group_var <- if(input$vis_group == "Tidak Ada") NULL else input$vis_group
                  create_boxplot(data, input$vis_var, group_var = group_var)
                },
                "Scatter Plot" = {
                  # Pastikan ada minimal 2 variabel numerik
                  numeric_vars <- names(data)[sapply(data, is.numeric)]
                  if(length(numeric_vars) < 2) {
                    # Fallback ke histogram jika tidak cukup variabel
                    create_histogram(data, input$vis_var)
                  } else {
                    x_var <- numeric_vars[numeric_vars != input$vis_var][1]
                    if(is.na(x_var)) x_var <- numeric_vars[1]
                    create_scatterplot(data, x_var = x_var, y_var = input$vis_var)
                  }
                }
    )
    
    if(is.null(p)) return(NULL)
    ggplotly(p)
    
  }, error = function(e) {
    showNotification(paste("Error creating plot:", e$message), type = "error")
    return(NULL)
  })
})

# Interpretasi sebaran data
output$data_interpretation <- renderUI({
  req(input$vis_var, input$vis_plot_type, processed_data$current)
  
  tryCatch({
    data <- processed_data$current
    
    # Validasi data dan variabel
    if(is.null(data) || !input$vis_var %in% names(data)) {
      return(HTML("<p>Data tidak tersedia untuk interpretasi.</p>"))
    }
    
    var_data <- data[[input$vis_var]]
    var_data <- var_data[!is.na(var_data)]
    
    if(length(var_data) == 0) {
      return(HTML("<p>Tidak ada data valid untuk interpretasi.</p>"))
    }
    
    # Statistik dasar dengan penanganan error
    mean_val <- tryCatch(mean(var_data), error = function(e) NA)
    median_val <- tryCatch(median(var_data), error = function(e) NA)
    sd_val <- tryCatch(sd(var_data), error = function(e) NA)
    
    if(is.na(mean_val) || is.na(median_val) || is.na(sd_val)) {
      return(HTML("<p>Error dalam perhitungan statistik dasar.</p>"))
    }
    
    cv_val <- if(mean_val != 0) sd_val / mean_val * 100 else NA
    
    # Load library e1071 jika belum ada
    if (!requireNamespace("e1071", quietly = TRUE)) {
      skewness_val <- NA
      kurtosis_val <- NA
    } else {
      skewness_val <- tryCatch(e1071::skewness(var_data), error = function(e) NA)
      kurtosis_val <- tryCatch(e1071::kurtosis(var_data), error = function(e) NA)
    }
    
    # Outliers
    q1 <- quantile(var_data, 0.25, na.rm = TRUE)
    q3 <- quantile(var_data, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    outliers <- sum(var_data < (q1 - 1.5 * iqr) | var_data > (q3 + 1.5 * iqr), na.rm = TRUE)
    
    # Normalitas (Shapiro-Wilk untuk n < 5000)
    normality_test <- if(length(var_data) <= 5000 && length(var_data) >= 3) {
      tryCatch(shapiro.test(var_data)$p.value, error = function(e) NA)
    } else {
      "Data terlalu besar atau kecil untuk uji Shapiro-Wilk"
    }
    
    # Interpretasi berdasarkan jenis plot
    interpretation <- switch(input$vis_plot_type,
                             "Histogram" = {
                               shape_desc <- if(!is.na(skewness_val)) {
                                 if(abs(skewness_val) < 0.5) {
                                   "Distribusi relatif simetris"
                                 } else if(skewness_val > 0.5) {
                                   "Distribusi miring kanan (tail panjang ke kanan)"
                                 } else {
                                   "Distribusi miring kiri (tail panjang ke kiri)"
                                 }
                               } else {
                                 "Bentuk distribusi tidak dapat dihitung"
                               }
                               
                               kurtosis_desc <- if(!is.na(kurtosis_val)) {
                                 if(abs(kurtosis_val) < 0.5) {
                                   "Kurtosis normal (mesokurtik)"
                                 } else if(kurtosis_val > 0.5) {
                                   "Kurtosis tinggi (leptokurtik) - distribusi runcing"
                                 } else {
                                   "Kurtosis rendah (platykurtik) - distribusi datar"
                                 }
                               } else {
                                 "Kurtosis tidak dapat dihitung"
                               }
                               
                               variability <- if(!is.na(cv_val)) {
                                 if(cv_val < 15) "Variabilitas rendah" 
                                 else if(cv_val < 30) "Variabilitas sedang" 
                                 else "Variabilitas tinggi"
                               } else {
                                 "Variabilitas tidak dapat dihitung"
                               }
                               
                               list(
                                 shape_desc = shape_desc,
                                 kurtosis_desc = kurtosis_desc,
                                 variability = variability
                               )
                             },
                             "Boxplot" = {
                               list(
                                 outlier_desc = paste0("Terdapat ", outliers, " outlier dalam data"),
                                 spread_desc = paste0("IQR = ", round(iqr, 2), " menunjukkan sebaran data di kuartil tengah"),
                                 median_vs_mean = if(abs(median_val - mean_val) < 0.1 * sd_val) "Median ≈ Mean (distribusi simetris)" else if(median_val < mean_val) "Median < Mean (miring kanan)" else "Median > Mean (miring kiri)"
                               )
                             },
                             "Scatter Plot" = {
                               numeric_vars <- names(data)[sapply(data, is.numeric)]
                               if(length(numeric_vars) >= 2) {
                                 x_var <- numeric_vars[numeric_vars != input$vis_var][1]
                                 if(is.na(x_var)) x_var <- numeric_vars[1]
                                 
                                 correlation <- tryCatch(
                                   cor(data[[x_var]], data[[input$vis_var]], use = "complete.obs"),
                                   error = function(e) NA
                                 )
                                 
                                 if(!is.na(correlation)) {
                                   list(
                                     correlation_desc = paste0("Korelasi dengan ", x_var, ": ", round(correlation, 3)),
                                     strength_desc = if(abs(correlation) < 0.3) "Korelasi lemah" else if(abs(correlation) < 0.7) "Korelasi sedang" else "Korelasi kuat",
                                     direction_desc = if(correlation > 0) "Hubungan positif" else "Hubungan negatif"
                                   )
                                 } else {
                                   list(correlation_desc = "Korelasi tidak dapat dihitung")
                                 }
                               } else {
                                 list(correlation_desc = "Tidak cukup variabel untuk analisis korelasi")
                               }
                             }
    )
    
    # Generate HTML output
    HTML(paste0(
      "<div style='padding: 10px;'>",
      "<h4 style='color: #2c3e50; margin-bottom: 15px;'>📊 Analisis ", input$vis_var, "</h4>",
      
      "<div style='background: #ecf0f1; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
      "<strong>Statistik Deskriptif:</strong><br>",
      "• Mean: ", if(!is.na(mean_val)) round(mean_val, 3) else "N/A", "<br>",
      "• Median: ", if(!is.na(median_val)) round(median_val, 3) else "N/A", "<br>",
      "• Std Dev: ", if(!is.na(sd_val)) round(sd_val, 3) else "N/A", "<br>",
      "• CV: ", if(!is.na(cv_val)) paste0(round(cv_val, 2), "%") else "N/A", "<br>",
      "</div>",
      
      "<div style='background: #e8f6f3; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
      "<strong>Karakteristik Distribusi:</strong><br>",
      if(input$vis_plot_type == "Histogram") {
        paste0(
          "• ", interpretation$shape_desc, "<br>",
          "• ", interpretation$kurtosis_desc, "<br>",
          "• ", interpretation$variability, "<br>",
          "• Skewness: ", if(!is.na(skewness_val)) round(skewness_val, 3) else "N/A"
        )
      } else if(input$vis_plot_type == "Boxplot") {
        paste0(
          "• ", interpretation$outlier_desc, "<br>",
          "• ", interpretation$spread_desc, "<br>",
          "• ", interpretation$median_vs_mean
        )
      } else {
        paste0(
          "• ", interpretation$correlation_desc, "<br>",
          if(!is.null(interpretation$strength_desc)) paste0("• ", interpretation$strength_desc, "<br>") else "",
          if(!is.null(interpretation$direction_desc)) paste0("• ", interpretation$direction_desc) else ""
        )
      },
      "</div>",
      
      if(is.numeric(normality_test)) {
        paste0(
          "<div style='background: #fef9e7; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
          "<strong>Uji Normalitas:</strong><br>",
          "• Shapiro-Wilk p-value: ", round(normality_test, 4), "<br>",
          "• Interpretasi: ", if(normality_test > 0.05) "Data terdistribusi normal" else "Data TIDAK terdistribusi normal",
          "</div>"
        )
      } else "",
      
      "<div style='background: #fdedec; padding: 10px; border-radius: 5px;'>",
      "<strong>💡 Rekomendasi Analisis:</strong><br>",
      if(is.numeric(normality_test) && normality_test > 0.05) {
        "• Gunakan uji parametrik (t-test, ANOVA)<br>• Regresi linear dapat diterapkan"
      } else {
        "• Pertimbangkan uji non-parametrik<br>• Transformasi data mungkin diperlukan<br>• Hati-hati dalam interpretasi regresi"
      },
      "</div>",
      "</div>"
    ))
    
  }, error = function(e) {
    HTML(paste0("<p>Error dalam interpretasi: ", e$message, "</p>"))
  })
})

# Render heatmap korelasi
output$correlation_heatmap <- renderPlotly({
  req(processed_data$current)
  
  tryCatch({
    data <- processed_data$current
    
    # Validasi data
    if(is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Periksa variabel numerik
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    if(length(numeric_vars) < 2) {
      showNotification("Minimal 2 variabel numerik diperlukan untuk heatmap korelasi", type = "warning")
      return(NULL)
    }
    
    p <- create_correlation_heatmap(data)
    if(is.null(p)) return(NULL)
    ggplotly(p)
    
  }, error = function(e) {
    showNotification(paste("Error creating correlation heatmap:", e$message), type = "error")
    return(NULL)
  })
})

# Interpretasi korelasi
output$correlation_interpretation <- renderUI({
  req(processed_data$current)
  
  # Ambil hanya variabel numerik
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  cor_data <- processed_data$current[numeric_vars]
  
  # Hapus kolom dengan variance 0
  cor_data <- cor_data[sapply(cor_data, function(x) var(x, na.rm = TRUE) > 0)]
  
  if(ncol(cor_data) < 2) {
    return(HTML("<p>Tidak cukup variabel numerik untuk analisis korelasi.</p>"))
  }
  
  # Hitung matriks korelasi
  cor_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
  
  # Analisis korelasi
  # Ekstrak korelasi (exclude diagonal)
  cor_values <- cor_matrix[upper.tri(cor_matrix)]
  cor_pairs <- which(upper.tri(cor_matrix), arr.ind = TRUE)
  
  # Buat dataframe untuk analisis
  cor_analysis <- data.frame(
    var1 = rownames(cor_matrix)[cor_pairs[,1]],
    var2 = colnames(cor_matrix)[cor_pairs[,2]],
    correlation = cor_values,
    abs_correlation = abs(cor_values)
  )
  
  # Urutkan berdasarkan korelasi absolut
  cor_analysis <- cor_analysis[order(cor_analysis$abs_correlation, decreasing = TRUE),]
  
  # Identifikasi korelasi terkuat dan terlemah
  strongest_pos <- cor_analysis[which.max(cor_analysis$correlation),]
  strongest_neg <- cor_analysis[which.min(cor_analysis$correlation),]
  strongest_abs <- cor_analysis[1,]
  weakest_abs <- cor_analysis[nrow(cor_analysis),]
  
  # Kategorisasi kekuatan korelasi
  very_strong <- sum(cor_analysis$abs_correlation >= 0.8)
  strong <- sum(cor_analysis$abs_correlation >= 0.6 & cor_analysis$abs_correlation < 0.8)
  moderate <- sum(cor_analysis$abs_correlation >= 0.4 & cor_analysis$abs_correlation < 0.6)
  weak <- sum(cor_analysis$abs_correlation >= 0.2 & cor_analysis$abs_correlation < 0.4)
  very_weak <- sum(cor_analysis$abs_correlation < 0.2)
  
  # Top 5 korelasi terkuat
  top_5 <- head(cor_analysis, 5)
  
  # Generate HTML output
  HTML(paste0(
    "<div style='padding: 10px;'>",
    "<h4 style='color: #2c3e50; margin-bottom: 15px;'>🔗 Analisis Korelasi Lengkap</h4>",
    
    # Ringkasan umum
    "<div style='background: #e8f6f3; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
    "<strong>📈 Ringkasan Korelasi:</strong><br>",
    "• Total pasangan variabel: ", nrow(cor_analysis), "<br>",
    "• Korelasi sangat kuat (≥0.8): ", very_strong, "<br>",
    "• Korelasi kuat (0.6-0.8): ", strong, "<br>",
    "• Korelasi sedang (0.4-0.6): ", moderate, "<br>",
    "• Korelasi lemah (0.2-0.4): ", weak, "<br>",
    "• Korelasi sangat lemah (<0.2): ", very_weak,
    "</div>",
    
    # Korelasi terkuat
    "<div style='background: #fdf2e9; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
    "<strong>💪 Korelasi Terkuat (Absolut):</strong><br>",
    "• <span style='color: #d35400; font-weight: bold;'>", strongest_abs$var1, " ↔ ", strongest_abs$var2, "</span><br>",
    "• Nilai: ", round(strongest_abs$correlation, 3), "<br>",
    "• Interpretasi: ", 
    if(strongest_abs$abs_correlation >= 0.8) "Korelasi sangat kuat" else 
      if(strongest_abs$abs_correlation >= 0.6) "Korelasi kuat" else 
        if(strongest_abs$abs_correlation >= 0.4) "Korelasi sedang" else "Korelasi lemah",
    if(strongest_abs$correlation > 0) " (positif)" else " (negatif)",
    "</div>",
    
    # Korelasi terlemah
    "<div style='background: #eaf2f8; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
    "<strong>🔸 Korelasi Terlemah:</strong><br>",
    "• <span style='color: #2980b9; font-weight: bold;'>", weakest_abs$var1, " ↔ ", weakest_abs$var2, "</span><br>",
    "• Nilai: ", round(weakest_abs$correlation, 3), "<br>",
    "• Interpretasi: Hampir tidak ada hubungan linear",
    "</div>",
    
    # Korelasi positif dan negatif terkuat
    "<div style='background: #e8f8f5; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
    "<strong>📊 Ekstrem Korelasi:</strong><br>",
    "• <span style='color: #27ae60;'>Positif terkuat:</span> ", strongest_pos$var1, " ↔ ", strongest_pos$var2, " (", round(strongest_pos$correlation, 3), ")<br>",
    "• <span style='color: #e74c3c;'>Negatif terkuat:</span> ", strongest_neg$var1, " ↔ ", strongest_neg$var2, " (", round(strongest_neg$correlation, 3), ")",
    "</div>",
    
    # Top 5 korelasi
    "<div style='background: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
    "<strong>🏆 Top 5 Korelasi Terkuat:</strong><br>",
    paste(sapply(1:min(5, nrow(top_5)), function(i) {
      paste0(i, ". ", top_5$var1[i], " ↔ ", top_5$var2[i], " (", round(top_5$correlation[i], 3), ")")
    }), collapse = "<br>"),
    "</div>",
    
    # Interpretasi dan rekomendasi
    "<div style='background: #fff3cd; padding: 10px; border-radius: 5px;'>",
    "<strong>💡 Interpretasi & Rekomendasi:</strong><br>",
    if(very_strong > 0) {
      "• <span style='color: #856404;'>Perhatian:</span> Ada korelasi sangat kuat yang mungkin menunjukkan multikolinearitas<br>"
    } else "",
    if(strong + very_strong >= 3) {
      "• Banyak variabel berkorelasi kuat - pertimbangkan analisis faktor<br>"
    } else "",
    "• Gunakan korelasi >0.6 untuk identifikasi redundansi variabel<br>",
    "• Korelasi <0.3 menunjukkan variabel relatif independen<br>",
    "• Pertimbangkan transformasi untuk korelasi non-linear",
    "</div>",
    "</div>"
  ))
})