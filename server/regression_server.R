# server/regression_server.R - VERSI DIPERBAIKI DENGAN INTERPRETASI LENGKAP

# --- 1. RENDER UI DINAMIS ---
output$reg_dependent_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("reg_dep_var", "Pilih Variabel Dependen (Y):", choices = numeric_vars)
})

output$reg_independent_selector <- renderUI({
  req(processed_data$current, input$reg_dep_var)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  choices <- setdiff(numeric_vars, input$reg_dep_var)
  selectizeInput("reg_indep_vars", "Pilih Variabel Independen (X):", choices = choices, multiple = TRUE)
})

# --- 2. LOGIKA REGRESI - DIPERBAIKI ---
regression_model <- eventReactive(input$run_regression, {
  req(input$reg_dep_var, length(input$reg_indep_vars) > 0)
  
  tryCatch({
    formula_str <- paste(input$reg_dep_var, "~", paste(input$reg_indep_vars, collapse = " + "))
    model <- lm(as.formula(formula_str), data = processed_data$current)
    
    # Simpan model dan hasilnya untuk analisis asumsi
    analysis_results$regression <- list(
      model = model,
      summary = summary(model),
      formula = formula_str
    )
    
    cat("Regression model created successfully\n")
    return(model)
    
  }, error = function(e) {
    showNotification(paste("Error dalam analisis regresi:", e$message), type = "error")
    return(NULL)
  })
})

# --- 3. TAMPILKAN HASIL MODEL ---
output$regression_summary <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    summary(model)
  } else {
    cat("Model regresi belum dibuat. Klik 'Bangun Model' untuk memulai analisis.")
  }
})

# --- 4. UJI ASUMSI DENGAN INTERPRETASI LENGKAP ---

# 4.1 Normalitas Residual dengan Interpretasi
output$regression_qqplot <- renderPlot({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      par(mfrow = c(1, 2))
      
      # Q-Q Plot
      plot(model, which = 2, main = "Q-Q Plot Residual", 
           sub = "Untuk memeriksa normalitas residual")
      
      # Histogram residual sebagai tambahan
      residuals_data <- residuals(model)
      hist(residuals_data, main = "Histogram Residual", 
           xlab = "Residual", ylab = "Frekuensi",
           breaks = 20, col = "lightblue", border = "black")
      
      par(mfrow = c(1, 1))
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error membuat Q-Q plot")
      text(1, 1, paste("Error:", e$message))
    })
  } else {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model' terlebih dahulu")
  }
})

# 4.2 Interpretasi Normalitas Residual
output$normality_residual_interpretation <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      # Uji Shapiro-Wilk pada residual
      residuals_data <- residuals(model)
      
      # Hanya lakukan uji jika sampel <= 5000 (batasan Shapiro-Wilk)
      if (length(residuals_data) <= 5000) {
        shapiro_test <- shapiro.test(residuals_data)
        
        cat("=== UJI NORMALITAS RESIDUAL (SHAPIRO-WILK) ===\n")
        cat("Hipotesis:\n")
        cat("H₀: Residual berdistribusi normal\n")
        cat("H₁: Residual tidak berdistribusi normal\n")
        cat("Tingkat signifikansi (α): 0.05\n\n")
        
        cat("Hasil Pengujian:\n")
        cat("Statistik W:", round(shapiro_test$statistic, 6), "\n")
        cat("P-value:", round(shapiro_test$p.value, 6), "\n\n")
        
        cat("=== INTERPRETASI ===\n")
        if (shapiro_test$p.value < 0.05) {
          cat(sprintf("Berdasarkan uji Shapiro-Wilk yang telah dilakukan, diketahui bahwa residual model regresi \nTIDAK berdistribusi normal karena nilai p-value (%.6f) < α (0.05), \nsehingga keputusan yang diambil adalah TOLAK H₀.\n\n", 
                      shapiro_test$p.value))
          cat("Kesimpulan: Asumsi normalitas residual TIDAK terpenuhi.\n\n")
          cat("Rekomendasi:\n")
          cat("1. Periksa keberadaan outlier dalam data\n")
          cat("2. Pertimbangkan transformasi variabel dependen (log, sqrt, Box-Cox)\n")
          cat("3. Gunakan regresi robust atau metode non-parametrik\n")
          cat("4. Tambahkan variabel independen yang relevan\n")
        } else {
          cat(sprintf("Berdasarkan uji Shapiro-Wilk yang telah dilakukan, diketahui bahwa residual model regresi \nberdistribusi normal karena nilai p-value (%.6f) > α (0.05), \nsehingga keputusan yang diambil adalah GAGAL TOLAK H₀.\n\n", 
                      shapiro_test$p.value))
          cat("Kesimpulan: Asumsi normalitas residual TERPENUHI.\n\n")
          cat("Catatan: Hasil ini mendukung validitas inferensi statistik dari model regresi.\n")
        }
      } else {
        cat("=== UJI NORMALITAS RESIDUAL ===\n")
        cat("Sampel terlalu besar untuk uji Shapiro-Wilk (n > 5000).\n")
        cat("Silakan evaluasi secara visual melalui Q-Q plot:\n")
        cat("- Jika titik-titik mengikuti garis diagonal: residual normal\n")
        cat("- Jika titik-titik menyimpang dari garis: residual tidak normal\n")
      }
      
    }, error = function(e) {
      cat("Error dalam uji normalitas residual:", e$message)
    })
  } else {
    cat("Model regresi belum dibuat. Klik 'Bangun Model' terlebih dahulu.")
  }
})

# 4.3 Homoskedastisitas dengan Interpretasi
output$regression_residual_plot <- renderPlot({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      par(mfrow = c(1, 2))
      
      # Residuals vs Fitted
      plot(model, which = 1, main = "Residuals vs Fitted", 
           sub = "Untuk memeriksa homoskedastisitas")
      
      # Scale-Location plot
      plot(model, which = 3, main = "Scale-Location", 
           sub = "Akar residual terstandar vs fitted")
      
      par(mfrow = c(1, 1))
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error membuat residual plot")
      text(1, 1, paste("Error:", e$message))
    })
  } else {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model' terlebih dahulu")
  }
})

# 4.4 Interpretasi Homoskedastisitas
output$homoskedasticity_interpretation <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      # Uji Breusch-Pagan untuk homoskedastisitas
      if (requireNamespace("lmtest", quietly = TRUE)) {
        bp_test <- lmtest::bptest(model)
        
        cat("=== UJI HOMOSKEDASTISITAS (BREUSCH-PAGAN) ===\n")
        cat("Hipotesis:\n")
        cat("H₀: Varians residual konstan (homoskedastisitas)\n")
        cat("H₁: Varians residual tidak konstan (heteroskedastisitas)\n")
        cat("Tingkat signifikansi (α): 0.05\n\n")
        
        cat("Hasil Pengujian:\n")
        cat("Statistik BP:", round(bp_test$statistic, 6), "\n")
        cat("P-value:", round(bp_test$p.value, 6), "\n\n")
        
        cat("=== INTERPRETASI ===\n")
        if (bp_test$p.value < 0.05) {
          cat(sprintf("Berdasarkan uji Breusch-Pagan yang telah dilakukan, diketahui bahwa model regresi \nmengalami masalah heteroskedastisitas karena nilai p-value (%.6f) < α (0.05), \nsehingga keputusan yang diambil adalah TOLAK H₀.\n\n", 
                      bp_test$p.value))
          cat("Kesimpulan: Asumsi homoskedastisitas TIDAK terpenuhi.\n\n")
          cat("Rekomendasi:\n")
          cat("1. Gunakan standard error yang robust (HC, HAC)\n")
          cat("2. Transformasi variabel (log, sqrt)\n")
          cat("3. Weighted Least Squares (WLS)\n")
          cat("4. Periksa spesifikasi model yang mungkin salah\n")
        } else {
          cat(sprintf("Berdasarkan uji Breusch-Pagan yang telah dilakukan, diketahui bahwa model regresi \nTIDAK mengalami masalah heteroskedastisitas karena nilai p-value (%.6f) > α (0.05), \nsehingga keputusan yang diambil adalah GAGAL TOLAK H₀.\n\n", 
                      bp_test$p.value))
          cat("Kesimpulan: Asumsi homoskedastisitas TERPENUHI.\n\n")
          cat("Catatan: Varians residual konstan, mendukung validitas inferensi statistik.\n")
        }
      } else {
        cat("=== UJI HOMOSKEDASTISITAS ===\n")
        cat("Package 'lmtest' tidak tersedia.\n")
        cat("Silakan evaluasi secara visual melalui plot Residuals vs Fitted:\n")
        cat("- Jika titik tersebar acak tanpa pola: homoskedastisitas\n")
        cat("- Jika membentuk pola corong/kerucut: heteroskedastisitas\n")
      }
      
    }, error = function(e) {
      cat("Error dalam uji homoskedastisitas:", e$message)
    })
  } else {
    cat("Model regresi belum dibuat. Klik 'Bangun Model' terlebih dahulu.")
  }
})

# 4.5 Multikolinearitas dengan Interpretasi
output$regression_vif <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    if (length(input$reg_indep_vars) > 1) {
      tryCatch({
        if (requireNamespace("car", quietly = TRUE)) {
          vif_values <- car::vif(model)
          
          cat("=== UJI MULTIKOLINEARITAS (VARIANCE INFLATION FACTOR) ===\n")
          cat("Kriteria VIF:\n")
          cat("- VIF < 5: Tidak ada masalah multikolinearitas\n")
          cat("- VIF 5-10: Multikolinearitas sedang, perlu diwaspadai\n")
          cat("- VIF > 10: Multikolinearitas serius\n\n")
          
          cat("Hasil VIF untuk setiap variabel:\n")
          for (i in 1:length(vif_values)) {
            var_name <- names(vif_values)[i]
            vif_val <- vif_values[i]
            cat(sprintf("%-20s: %.4f", var_name, vif_val))
            
            if (vif_val < 5) {
              cat(" [✓ Baik]")
            } else if (vif_val <= 10) {
              cat(" [⚠ Waspada]")
            } else {
              cat(" [✗ Bermasalah]")
            }
            cat("\n")
          }
          
          cat("\n=== INTERPRETASI ===\n")
          max_vif <- max(vif_values)
          problematic_vars <- names(vif_values)[vif_values > 10]
          warning_vars <- names(vif_values)[vif_values > 5 & vif_values <= 10]
          
          if (max_vif > 10) {
            cat(sprintf("Berdasarkan uji VIF yang telah dilakukan, diketahui bahwa model regresi \nmengalami masalah multikolinearitas SERIUS karena terdapat variabel dengan VIF > 10.\n\n"))
            cat("Variabel bermasalah:", paste(problematic_vars, collapse = ", "), "\n\n")
            cat("Kesimpulan: Asumsi tidak adanya multikolinearitas TIDAK terpenuhi.\n\n")
            cat("Rekomendasi:\n")
            cat("1. Hapus salah satu variabel yang berkorelasi tinggi\n")
            cat("2. Gabungkan variabel yang mirip menjadi indeks komposit\n")
            cat("3. Gunakan Principal Component Analysis (PCA)\n")
            cat("4. Gunakan Ridge Regression atau Lasso Regression\n")
          } else if (max_vif > 5) {
            cat(sprintf("Berdasarkan uji VIF yang telah dilakukan, diketahui bahwa model regresi \nmengalami multikolinearitas SEDANG karena terdapat variabel dengan VIF antara 5-10.\n\n"))
            cat("Variabel yang perlu diwaspadai:", paste(warning_vars, collapse = ", "), "\n\n")
            cat("Kesimpulan: Asumsi tidak adanya multikolinearitas CUKUP terpenuhi dengan catatan.\n\n")
            cat("Rekomendasi:\n")
            cat("1. Monitor stabilitas koefisien regresi\n")
            cat("2. Pertimbangkan untuk menghapus variabel dengan VIF tertinggi\n")
            cat("3. Interpretasi koefisien dengan hati-hati\n")
          } else {
            cat(sprintf("Berdasarkan uji VIF yang telah dilakukan, diketahui bahwa model regresi \nTIDAK mengalami masalah multikolinearitas karena semua variabel memiliki VIF < 5.\n\n"))
            cat("Kesimpulan: Asumsi tidak adanya multikolinearitas TERPENUHI.\n\n")
            cat("Catatan: Semua variabel independen dapat dipertahankan dalam model.\n")
          }
          
        } else {
          cat("Package 'car' tidak tersedia untuk menghitung VIF.\n")
          cat("Silakan install package 'car' terlebih dahulu.")
        }
      }, error = function(e) {
        cat("Error menghitung VIF:", e$message)
      })
    } else {
      cat("VIF (Variance Inflation Factor) memerlukan minimal 2 variabel independen.\n")
      cat("Model saat ini hanya memiliki 1 variabel independen, sehingga tidak ada masalah multikolinearitas.")
    }
  } else {
    cat("Model regresi belum dibuat. Klik 'Bangun Model' terlebih dahulu.")
  }
})

# --- 5. INTERPRETASI MODEL LENGKAP ---
output$regression_interpretation <- renderText({
  model <- regression_model()
  
  if (!is.null(model)) {
    summary_model <- summary(model)
    
    interpretation <- paste(
      "=== INTERPRETASI MODEL REGRESI ===\n\n",
      "FORMULA MODEL:\n",
      paste(input$reg_dep_var, "~", paste(input$reg_indep_vars, collapse = " + ")), "\n\n",
      
      "KUALITAS MODEL:\n",
      sprintf("- R-squared: %.4f (%.1f%% variabilitas dijelaskan)", 
              summary_model$r.squared, summary_model$r.squared * 100), "\n",
      sprintf("- Adjusted R-squared: %.4f", summary_model$adj.r.squared), "\n",
      sprintf("- F-statistic: %.4f, p-value: %.6f", 
              summary_model$fstatistic[1], 
              pf(summary_model$fstatistic[1], summary_model$fstatistic[2], 
                 summary_model$fstatistic[3], lower.tail = FALSE)), "\n\n",
      
      "INTERPRETASI KOEFISIEN:\n",
      "- Estimate: Perubahan Y untuk setiap kenaikan 1 unit X (ceteris paribus)\n",
      "- Pr(>|t|): Jika < 0.05, variabel signifikan mempengaruhi Y\n",
      "- Signifikansi: *** p<0.001, ** p<0.01, * p<0.05\n\n",
      
      "RINGKASAN UJI ASUMSI:\n",
      "Periksa tab 'Uji Asumsi' untuk interpretasi lengkap mengenai:\n",
      "1. NORMALITAS RESIDUAL: Shapiro-Wilk test dan Q-Q plot\n",
      "2. HOMOSKEDASTISITAS: Breusch-Pagan test dan residual plot\n",
      "3. MULTIKOLINEARITAS: VIF untuk setiap variabel independen\n",
      "4. LINIERITAS: Evaluasi visual melalui scatter plot\n\n",
      
      if (summary_model$r.squared > 0.7) {
        "✓ Model memiliki daya prediksi yang baik (R² > 70%)"
      } else if (summary_model$r.squared > 0.5) {
        "△ Model memiliki daya prediksi sedang (R² 50-70%)"
      } else {
        "⚠ Model memiliki daya prediksi rendah (R² < 50%)"
      }
    )
    
    return(interpretation)
  } else {
    return("Model regresi belum dibuat.\n\nLangkah-langkah:\n1. Pilih variabel dependen (Y)\n2. Pilih variabel independen (X)\n3. Klik 'Bangun Model'\n4. Interpretasi hasil dan uji asumsi")
  }
})

# --- 6. LOGIKA UNDUH - DIPERBAIKI ---
output$download_regression_summary <- downloadHandler(
  filename = function() {
    paste("ringkasan-regresi-", Sys.Date(), ".", input$regression_format, sep = "")
  },
  content = function(file) {
    model <- regression_model()
    
    if (!is.null(model)) {
      # Gabungkan semua hasil analisis
      summary_text <- capture.output(summary(model))
      
      # Tambahkan interpretasi asumsi jika tersedia
      assumptions_text <- ""
      
      tryCatch({
        # Uji normalitas
        residuals_data <- residuals(model)
        if (length(residuals_data) <= 5000) {
          shapiro_test <- shapiro.test(residuals_data)
          assumptions_text <- paste(assumptions_text, 
                                    "\n\n=== UJI ASUMSI ===\n",
                                    "1. Normalitas Residual (Shapiro-Wilk):\n",
                                    sprintf("   W = %.6f, p-value = %.6f", shapiro_test$statistic, shapiro_test$p.value),
                                    if (shapiro_test$p.value < 0.05) "\n   Kesimpulan: Residual TIDAK normal" else "\n   Kesimpulan: Residual normal",
                                    sep = "")
        }
        
        # Uji homoskedastisitas
        if (requireNamespace("lmtest", quietly = TRUE)) {
          bp_test <- lmtest::bptest(model)
          assumptions_text <- paste(assumptions_text,
                                    "\n\n2. Homoskedastisitas (Breusch-Pagan):\n",
                                    sprintf("   BP = %.6f, p-value = %.6f", bp_test$statistic, bp_test$p.value),
                                    if (bp_test$p.value < 0.05) "\n   Kesimpulan: Terjadi heteroskedastisitas" else "\n   Kesimpulan: Varians homogen",
                                    sep = "")
        }
        
        # VIF
        if (length(input$reg_indep_vars) > 1 && requireNamespace("car", quietly = TRUE)) {
          vif_values <- car::vif(model)
          assumptions_text <- paste(assumptions_text,
                                    "\n\n3. Multikolinearitas (VIF):\n",
                                    paste(sapply(names(vif_values), function(x) 
                                      sprintf("   %s: %.4f", x, vif_values[x])), collapse = "\n"),
                                    if (max(vif_values) > 10) "\n   Kesimpulan: Terdapat multikolinearitas serius" else 
                                      if (max(vif_values) > 5) "\n   Kesimpulan: Multikolinearitas sedang" else "\n   Kesimpulan: Tidak ada masalah multikolinearitas",
                                    sep = "")
        }
      }, error = function(e) {
        assumptions_text <- paste(assumptions_text, "\n\nError dalam uji asumsi:", e$message)
      })
      
      full_text <- paste(paste(summary_text, collapse = "\n"), assumptions_text, sep = "")
      
      rmarkdown::render(
        input = "text_report.Rmd",
        output_file = file,
        output_format = if(input$regression_format == "pdf") "pdf_document" else "word_document",
        params = list(
          report_title = "Ringkasan Model Regresi dengan Uji Asumsi",
          text_output = full_text
        ),
        envir = new.env(parent = globalenv())
      )
    } else {
      # Buat file dengan pesan error
      rmarkdown::render(
        input = "text_report.Rmd",
        output_file = file,
        output_format = if(input$regression_format == "pdf") "pdf_document" else "word_document",
        params = list(
          report_title = "Ringkasan Model Regresi",
          text_output = "Model regresi belum dibuat. Silakan buat model terlebih dahulu."
        ),
        envir = new.env(parent = globalenv())
      )
    }
  }
)

# --- 7. DIAGNOSTIK LANJUTAN ---

# 7.1 Cook's Distance Plot
output$cooks_distance_plot <- renderPlot({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      cooks_d <- cooks.distance(model)
      n <- length(cooks_d)
      
      plot(1:n, cooks_d, 
           type = "h", 
           main = "Cook's Distance",
           xlab = "Observasi", 
           ylab = "Cook's Distance",
           col = ifelse(cooks_d > 1, "red", "blue"))
      abline(h = 1, col = "red", lty = 2, lwd = 2)
      abline(h = 4/n, col = "orange", lty = 2, lwd = 1)
      
      # Tambahkan label untuk outlier
      outliers <- which(cooks_d > 1)
      if (length(outliers) > 0) {
        text(outliers, cooks_d[outliers], labels = outliers, pos = 3, col = "red")
      }
      
      legend("topright", 
             legend = c("Normal", "Berpengaruh (>1)", "Waspada (>4/n)"),
             col = c("blue", "red", "orange"),
             lty = c(1, 1, 2),
             cex = 0.8)
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error membuat Cook's Distance plot")
      text(1, 1, paste("Error:", e$message))
    })
  } else {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model' terlebih dahulu")
  }
})

# 7.2 Leverage Plot
output$leverage_plot <- renderPlot({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      leverage <- hatvalues(model)
      n <- length(leverage)
      p <- length(coef(model))
      threshold <- 2 * p / n
      
      plot(1:n, leverage, 
           type = "h", 
           main = "Leverage Values",
           xlab = "Observasi", 
           ylab = "Leverage",
           col = ifelse(leverage > threshold, "red", "blue"))
      abline(h = threshold, col = "red", lty = 2, lwd = 2)
      
      # Tambahkan label untuk high leverage
      high_lev <- which(leverage > threshold)
      if (length(high_lev) > 0) {
        text(high_lev, leverage[high_lev], labels = high_lev, pos = 3, col = "red")
      }
      
      legend("topright", 
             legend = c("Normal", paste("High Leverage (>", round(threshold, 3), ")")),
             col = c("blue", "red"),
             lty = c(1, 2),
             cex = 0.8)
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error membuat Leverage plot")
      text(1, 1, paste("Error:", e$message))
    })
  } else {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model' terlebih dahulu")
  }
})

# 7.3 Residual vs Leverage Plot
output$residual_leverage_plot <- renderPlot({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      plot(model, which = 5, main = "Residuals vs Leverage", 
           sub = "Identifikasi outlier dan data berpengaruh")
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error membuat Residual vs Leverage plot")
      text(1, 1, paste("Error:", e$message))
    })
  } else {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model' terlebih dahulu")
  }
})

# --- 8. TAMBAHAN: OBSERVER UNTUK DEBUGGING ---
observe({
  if (!is.null(input$run_regression)) {
    cat("Run regression button clicked:", input$run_regression, "\n")
  }
})

observe({
  model <- regression_model()
  if (!is.null(model)) {
    cat("Regression model available, summary:\n")
    cat("Formula:", as.character(formula(model)), "\n")
    cat("R-squared:", summary(model)$r.squared, "\n")
  }
})