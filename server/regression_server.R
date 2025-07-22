# server/regression_server.R 
# Fungsi helper untuk generate interpretasi
generate_regression_interpretation <- function(model, reg_dep_var, reg_indep_vars) {
  if (is.null(model)) {
    return("Model regresi belum dibuat.")
  }
  
  summary_model <- summary(model)
  coefficients_df <- as.data.frame(summary_model$coefficients)
  
  # Ambil informasi dasar model
  formula_str <- paste(reg_dep_var, "~", paste(reg_indep_vars, collapse = " + "))
  r_squared <- summary_model$r.squared
  adj_r_squared <- summary_model$adj.r.squared
  f_statistic <- summary_model$fstatistic[1]
  f_p_value <- pf(summary_model$fstatistic[1], summary_model$fstatistic[2], 
                  summary_model$fstatistic[3], lower.tail = FALSE)
  
  # --- BAGIAN 1: INTERPRETASI SIGNIFIKANSI VARIABEL ---
  signif_interpretation <- ""
  significant_vars <- c()
  
  # Cek variabel yang signifikan (p < 0.05)
  for (var_name in reg_indep_vars) {
    if (var_name %in% rownames(coefficients_df)) {
      p_value <- coefficients_df[var_name, "Pr(>|t|)"]
      if (p_value < 0.05) {
        significant_vars <- c(significant_vars, var_name)
      }
    }
  }
  
  if (length(significant_vars) > 0) {
    # Buat daftar p-values untuk variabel signifikan
    p_values_text <- sapply(significant_vars, function(var) {
      p_val <- coefficients_df[var, "Pr(>|t|)"]
      paste("p-value untuk", var, "sebesar", sprintf("%.3f", p_val))
    })
    
    signif_interpretation <- paste(
      "Dari hasil tersebut, diperoleh", paste(p_values_text, collapse = " dan "), ".",
      if (length(significant_vars) == 1) {
        paste("Karena p-value lebih kecil dari 0,05, maka", significant_vars[1], 
              "berpengaruh signifikan terhadap", reg_dep_var, "pada tingkat signifikansi 5%.")
      } else {
        paste("Karena semua p-value lebih kecil dari 0,05, maka", 
              paste(significant_vars[-length(significant_vars)], collapse = ", "), 
              "maupun", significant_vars[length(significant_vars)], 
              "berpengaruh signifikan terhadap", reg_dep_var, "pada tingkat signifikansi 5%.")
      }
    )
  }
  
# BAGIAN 2: INTERPRETASI KOEFISIEN
  coeff_interpretation <- ""
  
  if (length(significant_vars) > 0) {
    coeff_texts <- sapply(significant_vars, function(var) {
      coeff_value <- coefficients_df[var, "Estimate"]
      
      # Tentukan satuan berdasarkan nama variabel
      if (grepl("(?i)(populasi|population)", var)) {
        # Variabel populasi (satuan: jiwa)
        paste("Koefisien", var, "sebesar", sprintf("%.6f", coeff_value), 
              "menunjukkan bahwa setiap tambahan satu jiwa", var, 
              "akan", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
              reg_dep_var, "sebesar", sprintf("%.6f", abs(coeff_value)), 
              "persen, dengan asumsi variabel lain tetap konstan.")
      } else if (grepl("(?i)(family.*size|familysize|ukuran.*keluarga|size.*family)", var)) {
        # Variabel family size (satuan: orang)
        paste("Koefisien", var, "sebesar", sprintf("%.3f", coeff_value), 
              "menunjukkan bahwa setiap tambahan satu orang dalam", var, 
              "akan", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
              reg_dep_var, "sebesar", sprintf("%.3f", abs(coeff_value)), 
              "persen, dengan asumsi variabel lain tetap konstan.")
      } else {
        # Variabel dengan satuan persen (default)
        paste("Koefisien", var, "sebesar", sprintf("%.3f", coeff_value), 
              "mengindikasikan bahwa setiap kenaikan satu persen", var, 
              "akan", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
              reg_dep_var, "sebesar", sprintf("%.3f", abs(coeff_value)), 
              "persen, dengan asumsi variabel lain tetap konstan.")
      }
    })
    
    coeff_interpretation <- paste(coeff_texts, collapse = " Sementara itu, ")
  }
  
  # --- BAGIAN 3: INTERPRETASI R-SQUARED ---
  r_squared_interpretation <- paste(
    "Nilai R² sebesar", sprintf("%.3f", r_squared), 
    "menunjukkan bahwa", sprintf("%.1f", r_squared * 100), 
    "% variasi", reg_dep_var, 
    "dapat dijelaskan oleh", paste(reg_indep_vars, collapse = ", "), 
    ", sementara sisanya", sprintf("%.1f", (1 - r_squared) * 100), 
    "% dijelaskan oleh faktor lain yang tidak dimasukkan dalam model."
  )
  
  # --- BAGIAN 4: KESIMPULAN ---
  conclusion <- paste(
    "Oleh karena itu, terdapat cukup bukti bahwa", 
    if (length(significant_vars) == 1) {
      paste("variabel", significant_vars[1], "memiliki")
    } else if (length(significant_vars) > 1) {
      paste("variabel-variabel independen tersebut secara bersama-sama memiliki")
    } else {
      "model secara keseluruhan memiliki"
    },
    "pengaruh yang signifikan terhadap", reg_dep_var, "."
  )
  
  # --- GABUNGKAN SEMUA INTERPRETASI ---
  full_interpretation <- paste(
    "=== INFORMASI MODEL ===",
    paste("Formula Model:", formula_str),
    sprintf("R-squared: %.4f (%.1f%% variabilitas dijelaskan)", r_squared, r_squared * 100),
    sprintf("Adjusted R-squared: %.4f", adj_r_squared),
    sprintf("F-statistic: %.4f, p-value: %.6f", f_statistic, f_p_value),
    "",
    "=== INTERPRETASI HASIL ===",
    "",
    if (signif_interpretation != "") signif_interpretation else "",
    "",
    if (coeff_interpretation != "") coeff_interpretation else "",
    "",
    r_squared_interpretation,
    "",
    conclusion,
    "",
    "=== PENILAIAN KUALITAS MODEL ===",
    if (r_squared > 0.7) {
      "Model memiliki daya prediksi yang baik (R² > 70%)"
    } else if (r_squared > 0.5) {
      "Model memiliki daya prediksi sedang (R² 50-70%)"
    } else {
      "Model memiliki daya prediksi rendah (R² < 50%)"
    },
    sep = "\n"
  )
  
  return(full_interpretation)
}


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
      # Create layout for multiple plots
      par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
      
      # Q-Q Plot of residuals
      residuals_data <- residuals(model)
      qqnorm(residuals_data, 
             main = "Q-Q Plot Residual", 
             sub = "Untuk memeriksa normalitas residual",
             pch = 16, col = "steelblue")
      qqline(residuals_data, col = "red", lwd = 2)
      grid(col = "lightgray", lty = "dotted")
      
      # Add correlation info
      n_residuals <- length(residuals_data)
      theoretical_q <- qnorm(ppoints(n_residuals))
      empirical_q <- sort(residuals_data)
      correlation <- cor(theoretical_q, empirical_q)
      
      legend("topleft", 
             legend = paste("r =", round(correlation, 3)),
             bty = "n", cex = 0.9)
      
      # Histogram residual sebagai tambahan
      hist(residuals_data, 
           main = "Histogram Residual", 
           xlab = "Residual", 
           ylab = "Frekuensi",
           breaks = 20, 
           col = "lightblue", 
           border = "black")
      
      # Add normal curve overlay
      x_seq <- seq(min(residuals_data), max(residuals_data), length.out = 100)
      y_seq <- dnorm(x_seq, mean = mean(residuals_data), sd = sd(residuals_data))
      y_seq <- y_seq * length(residuals_data) * diff(range(residuals_data)) / 20
      lines(x_seq, y_seq, col = "red", lwd = 2)
      
      par(mfrow = c(1, 1))
      
    }, error = function(e) {
      par(mfrow = c(1, 1))
      plot(1, 1, type = "n", main = "Error membuat Q-Q plot")
      text(1, 1, paste("Error:", e$message), col = "red")
    })
  } else {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model' terlebih dahulu")
  }
}, height = 400)

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

#4.3 autokorelasi
output$independence_interpretation <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      # Membutuhkan package 'lmtest'
      if (requireNamespace("lmtest", quietly = TRUE)) {
        dw_test <- lmtest::dwtest(model)
        
        cat("=== UJI AUTOKORELASI (DURBIN-WATSON) ===\n")
        cat("Hipotesis:\n")
        cat("H₀: Tidak ada autokorelasi (residual independen)\n")
        cat("H₁: Ada autokorelasi positif\n")
        cat("Tingkat signifikansi (α): 0.05\n\n")
        
        cat("Hasil Pengujian:\n")
        cat("Statistik DW:", round(dw_test$statistic, 4), "\n")
        cat("P-value:", round(dw_test$p.value, 6), "\n\n")
        
        cat("=== INTERPRETASI ===\n")
        if (dw_test$p.value < 0.05) {
          cat(sprintf("Berdasarkan uji Durbin-Watson, terindikasi adanya masalah autokorelasi karena nilai \np-value (%.6f) < α (0.05), sehingga keputusan yang diambil adalah TOLAK H₀.\n\n", dw_test$p.value))
          cat("Kesimpulan: Asumsi independensi residual TIDAK terpenuhi.\n")
        } else {
          cat(sprintf("Berdasarkan uji Durbin-Watson, tidak ditemukan adanya masalah autokorelasi karena nilai \np-value (%.6f) ≥ α (0.05), sehingga keputusan yang diambil adalah GAGAL TOLAK H₀.\n\n", dw_test$p.value))
          cat("Kesimpulan: Asumsi independensi residual TERPENUHI.\n")
        }
        
      } else {
        cat("Package 'lmtest' tidak tersedia. Silakan install package 'lmtest' untuk melakukan uji Durbin-Watson.")
      }
      
    }, error = function(e) {
      cat("Error dalam uji autokorelasi:", e$message)
    })
  } else {
    cat("Model regresi belum dibuat. Klik 'Bangun Model' terlebih dahulu.")
  }
})


# 4.4 Interpretasi Homoskedastisitas
output$homoscedasticity_interpretation <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      if (requireNamespace("lmtest", quietly = TRUE)) {
        bp_test <- lmtest::bptest(model)
        
        cat("=== UJI HOMOSKEDASTISITAS (BREUSCH-PAGAN) ===\n")
        cat("Hipotesis:\n")
        cat("H₀: Varians residual konstan (homoskedastisitas)\n")
        cat("H₁: Varians residual tidak konstan (heteroskedastisitas)\n")
        cat("Tingkat signifikansi (α): 0.05\n\n")
        
        cat("Hasil Pengujian:\n")
        cat("Statistik BP:", round(bp_test$statistic, 4), "\n")
        cat("P-value:", format.pval(bp_test$p.value, digits = 6), "\n\n")
        
        cat("=== INTERPRETASI ===\n")
        if (bp_test$p.value < 0.05) {
          cat(sprintf(
            "Berdasarkan uji Breusch-Pagan, model regresi mengalami masalah heteroskedastisitas karena \nnilai p-value (%.6f) < α (0.05).\nKeputusan yang diambil adalah TOLAK H₀.\n", 
            bp_test$p.value
          ))
          cat("\nKesimpulan: Asumsi homoskedastisitas TIDAK terpenuhi.\n")
        } else {
          cat(sprintf(
            "Berdasarkan uji Breusch-Pagan, model regresi TIDAK mengalami masalah heteroskedastisitas karena \nnilai p-value (%.6f) ≥ α (0.05).\nKeputusan yang diambil adalah GAGAL TOLAK H₀.\n", 
            bp_test$p.value
          ))
          cat("\nKesimpulan: Asumsi homoskedastisitas TERPENUHI.\n")
          cat("Catatan: Varians residual yang konstan mendukung validitas inferensi statistik.\n")
        }
      } else {
        cat("Package 'lmtest' tidak tersedia. Silakan install package 'lmtest' terlebih dahulu.")
      }
      
    }, error = function(e) {
      cat("Error dalam uji homoskedastisitas:", e$message)
    })
  } else {
    cat("Model regresi belum dibuat. Klik 'Bangun Model' terlebih dahulu.")
  }
})

# 4.5 Multikolinearitas dengan Interpretasi
output$multikolinearitas_vif <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    # VIF hanya relevan jika ada lebih dari 1 variabel independen
    if (length(input$reg_indep_vars) > 1) {
      tryCatch({
        # Membutuhkan package 'car'
        if (requireNamespace("car", quietly = TRUE)) {
          vif_values <- car::vif(model)
          
          cat("=== UJI MULTIKOLINEARITAS (VARIANCE INFLATION FACTOR) ===\n")
          cat("Kriteria VIF:\n")
          cat("- VIF < 5:  Tidak ada masalah multikolinearitas\n")
          cat("- VIF 5-10: Multikolinearitas sedang, perlu diwaspadai\n")
          cat("- VIF > 10: Multikolinearitas serius, perlu perbaikan model\n\n")
          
          cat("Hasil VIF untuk setiap variabel:\n")
          print(vif_values)
          
          cat("\n=== INTERPRETASI ===\n")
          max_vif <- max(vif_values)
          
          if (max_vif > 10) {
            problematic_vars <- names(vif_values)[vif_values > 10]
            cat("Ditemukan masalah multikolinearitas SERIUS pada model.\n")
            cat("Variabel dengan VIF > 10:", paste(problematic_vars, collapse = ", "), "\n")
            cat("Kesimpulan: Asumsi bebas multikolinearitas TIDAK terpenuhi.\n")
          } else if (max_vif > 5) {
            warning_vars <- names(vif_values)[vif_values > 5]
            cat("Ditemukan potensi multikolinearitas SEDANG pada model.\n")
            cat("Variabel dengan VIF > 5:", paste(warning_vars, collapse = ", "), "\n")
            cat("Kesimpulan: Asumsi bebas multikolinearitas CUKUP terpenuhi, namun perlu kewaspadaan.\n")
          } else {
            cat("Tidak ditemukan masalah multikolinearitas yang signifikan pada model (semua VIF < 5).\n")
            cat("Kesimpulan: Asumsi bebas multikolinearitas TERPENUHI.\n")
          }
          
        } else {
          cat("Package 'car' tidak tersedia untuk menghitung VIF.\nSilakan install package 'car' terlebih dahulu.")
        }
      }, error = function(e) {
        cat("Error saat menghitung VIF:", e$message)
      })
    } else {
      cat("Uji VIF (Variance Inflation Factor) memerlukan minimal 2 variabel independen.\n")
      cat("Model saat ini hanya memiliki 1 variabel independen, sehingga multikolinearitas tidak dapat dihitung (dan tidak menjadi masalah).\n")
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
    coefficients_df <- as.data.frame(summary_model$coefficients)
    
    # Ambil informasi dasar model
    formula_str <- paste(input$reg_dep_var, "~", paste(input$reg_indep_vars, collapse = " + "))
    r_squared <- summary_model$r.squared
    adj_r_squared <- summary_model$adj.r.squared
    f_statistic <- summary_model$fstatistic[1]
    f_p_value <- pf(summary_model$fstatistic[1], summary_model$fstatistic[2], 
                    summary_model$fstatistic[3], lower.tail = FALSE)
    
    # --- BAGIAN 1: INTERPRETASI SIGNIFIKANSI VARIABEL ---
    signif_interpretation <- ""
    significant_vars <- c()
    
    # Cek variabel yang signifikan (p < 0.05)
    for (var_name in input$reg_indep_vars) {
      if (var_name %in% rownames(coefficients_df)) {
        p_value <- coefficients_df[var_name, "Pr(>|t|)"]
        if (p_value < 0.05) {
          significant_vars <- c(significant_vars, var_name)
        }
      }
    }
    
    if (length(significant_vars) > 0) {
      # Buat daftar p-values untuk variabel signifikan
      p_values_text <- sapply(significant_vars, function(var) {
        p_val <- coefficients_df[var, "Pr(>|t|)"]
        paste("p-value untuk", var, "sebesar", sprintf("%.3f", p_val))
      })
      
      signif_interpretation <- paste(
        "\nDari hasil tersebut, diperoleh", paste(p_values_text, collapse = " dan \n"), ".",
        if (length(significant_vars) == 1) {
          paste("Karena p-value lebih kecil dari 0,05, \n maka", significant_vars[1], 
                "\nberpengaruh signifikan terhadap", input$reg_dep_var, "pada tingkat signifikansi 5%.")
        } else {
          paste("Karena semua p-value lebih kecil dari 0,05, \n maka", 
                paste(significant_vars[-length(significant_vars)], collapse = ", "), 
                "\nmaupun", significant_vars[length(significant_vars)], 
                "\nberpengaruh signifikan terhadap", input$reg_dep_var, "pada tingkat signifikansi 5%.")
        }
      )
    }
    
    # --- BAGIAN 2: INTERPRETASI KOEFISIEN ---
    coeff_interpretation <- ""
    
    if (length(significant_vars) > 0) {
      coeff_texts <- sapply(significant_vars, function(var) {
        coeff_value <- coefficients_df[var, "Estimate"]
        
        # Tentukan satuan berdasarkan nama variabel
        if (grepl("(?i)(populasi|population)", var)) {
          # Variabel populasi (satuan: jiwa)
          paste("Koefisien", var, "sebesar", sprintf("%.6f", coeff_value), 
                "menunjukkan bahwa setiap tambahan satu jiwa", var, 
                "akan\n", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
                input$reg_dep_var, "sebesar", sprintf("%.6f", abs(coeff_value)), 
                "persen, \ndengan asumsi variabel lain tetap konstan.")
        } else if (grepl("(?i)(family.*size|familysize|ukuran.*keluarga|size.*family)", var)) {
          # Variabel family size (satuan: orang)
          paste("Koefisien", var, "sebesar", sprintf("%.3f", coeff_value), 
                "menunjukkan bahwa setiap tambahan satu orang dalam", var, 
                "akan\n", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
                input$reg_dep_var, "sebesar", sprintf("%.3f", abs(coeff_value)), 
                "persen, \ndengan asumsi variabel lain tetap konstan.")
        } else {
          # Variabel dengan satuan persen (default)
          paste("Koefisien", var, "sebesar", sprintf("%.3f", coeff_value), 
                "mengindikasikan bahwa setiap kenaikan satu persen\n", var, 
                "akan\n", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
                input$reg_dep_var, "sebesar", sprintf("%.3f", abs(coeff_value)), 
                "persen, \ndengan asumsi variabel lain tetap konstan.")
        }
      })
      
      coeff_interpretation <- paste(coeff_texts, collapse = " \nSementara itu, ")
    }
    
    # --- BAGIAN 3: INTERPRETASI R-SQUARED ---
    r_squared_interpretation <- paste(
      "Nilai R² sebesar", sprintf("%.3f", r_squared), 
      "menunjukkan bahwa", sprintf("%.1f", r_squared * 100), 
      "% variasi", input$reg_dep_var, 
      "dapat dijelaskan \noleh", paste(input$reg_indep_vars, collapse = ", "), 
      ". \nSementara sisanya", sprintf("%.1f", (1 - r_squared) * 100), 
      "% dijelaskan oleh faktor lain yang tidak dimasukkan dalam model."
    )
    
    # --- BAGIAN 4: KESIMPULAN ---
    conclusion <- paste(
      "Oleh karena itu, terdapat cukup bukti bahwa", 
      if (length(significant_vars) == 1) {
        paste("variabel", significant_vars[1], "memiliki")
      } else if (length(significant_vars) > 1) {
        paste("variabel-variabel independen tersebut\n secara bersama-sama memiliki")
      } else {
        "model secara keseluruhan memiliki"
      },
      "pengaruh yang signifikan terhadap", input$reg_dep_var, "."
    )
    
    # --- BAGIAN 5: INFORMASI TAMBAHAN MODEL ---
    model_info <- paste(
      "=== INFORMASI MODEL ===\n",
      "Formula Model:", formula_str, "\n",
      sprintf("R-squared: %.4f (%.1f%% variabilitas dijelaskan)", r_squared, r_squared * 100), "\n",
      sprintf("Adjusted R-squared: %.4f", adj_r_squared), "\n",
      sprintf("F-statistic: %.4f, p-value: %.6f", f_statistic, f_p_value), "\n\n"
    )
    
    # --- GABUNGKAN SEMUA INTERPRETASI ---
    full_interpretation <- paste(
      model_info,
      "=== INTERPRETASI HASIL ===\n\n",
      if (signif_interpretation != "") paste(signif_interpretation, "\n\n") else "",
      if (coeff_interpretation != "") paste(coeff_interpretation, "\n\n") else "",
      r_squared_interpretation, "\n\n",
      conclusion, "\n\n",
      
      
      # Penilaian kualitas model
      if (r_squared > 0.7) {
        "✓ Model memiliki daya prediksi yang baik (R² > 70%)"
      } else if (r_squared > 0.5) {
        "△ Model memiliki daya prediksi sedang (R² 50-70%)"
      } else {
        "⚠ Model memiliki daya prediksi rendah (R² < 50%)"
      }
    )
    
    return(full_interpretation)
    
  } else {
    return("Model regresi belum dibuat.\n\nLangkah-langkah:\n1. Pilih variabel dependen (Y)\n2. Pilih variabel independen (X)\n3. Klik 'Bangun Model'\n4. Interpretasi hasil dan uji asumsi")
  }
})

# --- 6. LOGIKA UNDUH ---

output$download_regression_summary <- downloadHandler(
  filename = function() {
    ext <- switch(input$regression_format, "pdf" = "pdf", "docx" = "docx")
    paste0("laporan-analisis-regresi-", Sys.Date(), ".", ext)
  },
  content = function(file) {
    req(regression_model())
    
    showNotification("Menyiapkan laporan...", type = "message", duration = 5)
    
    model <- regression_model()
    
    # --- LANGKAH 1: PERSIAPKAN SEMUA TEKS ---
    
    # Ringkasan Model
    summary_text <- capture.output(summary(model))
    
    # Interpretasi
    interpretation_text_raw <- generate_regression_interpretation(
      model, 
      input$reg_dep_var, 
      input$reg_indep_vars
    )
    
    # Format teks interpretasi
    interpretation_text_formatted <- gsub("\\n", "  \n", interpretation_text_raw)
    
    # Uji Asumsi
    normality_test_text <- tryCatch(
      capture.output(print(shapiro.test(residuals(model)))),
      error = function(e) "Gagal melakukan uji normalitas."
    )
    homogeneity_test_text <- tryCatch(
      if(requireNamespace("lmtest", quietly=TRUE)) capture.output(print(lmtest::bptest(model))) else "Package 'lmtest' tidak terinstal.",
      error = function(e) "Gagal melakukan uji homoskedastisitas."
    )
    autocorrelation_test_text <- tryCatch(
      if(requireNamespace("lmtest", quietly=TRUE)) capture.output(print(lmtest::dwtest(model))) else "Package 'lmtest' tidak terinstal.",
      error = function(e) "Gagal melakukan uji autokorelasi."
    )
    vif_test_text <- tryCatch(
      if(length(input$reg_indep_vars) > 1 && requireNamespace("car", quietly=TRUE)) capture.output(print(car::vif(model))) else "VIF tidak dihitung.",
      error = function(e) "Gagal menghitung VIF."
    )
    
    # --- LANGKAH 2: BUAT KONTEN LAPORAN ---
    temp_rmd <- tempfile(fileext = ".Rmd")
    
    rmd_content <- paste(
      "---",
      "title: 'Laporan Analisis Regresi Linear'",
      paste0("date: '", format(Sys.time(), "%A, %d %B %Y %H:%M:%S"), "'"),
      "output: default",
      "---",
      "",
      "### 1. Ringkasan Model Regresi",
      "```",
      paste(summary_text, collapse = "\n"),
      "```",
      "",
      "### 2. Interpretasi Model",
      "", 
      paste(interpretation_text_formatted, collapse = "\n"),
      "",
      "### 3. Hasil Uji Asumsi Klasik",
      "#### 3.1. Uji Normalitas Residual (Shapiro-Wilk)",
      "```",
      paste(normality_test_text, collapse = "\n"),
      "```",
      "",
      "#### 3.2. Uji Homoskedastisitas (Breusch-Pagan)",
      "```",
      paste(homogeneity_test_text, collapse = "\n"),
      "```",
      "",
      "#### 3.3. Uji Autokorelasi (Durbin-Watson)",
      "```",
      paste(autocorrelation_test_text, collapse = "\n"),
      "```",
      "",
      "#### 3.4. Uji Multikolinearitas (VIF)",
      "```",
      paste(vif_test_text, collapse = "\n"),
      "```",
      "",
      "---",
      paste0("*Laporan ini dibuat secara otomatis pada ", format(Sys.time(), "%d %B %Y, %H:%M:%S"), ".*"),
      sep = "\n"
    )
    
    writeLines(rmd_content, temp_rmd)
    
    # --- LANGKAH 3: RENDER LAPORAN ---
    tryCatch({
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        output_format = switch(input$regression_format, "pdf" = "pdf_document", "docx" = "word_document"),
        quiet = TRUE
      )
      showNotification("Laporan berhasil dibuat!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error membuat laporan:", e$message), type = "error", duration = 10)
    })
    
    if(file.exists(temp_rmd)) unlink(temp_rmd)
  }
)