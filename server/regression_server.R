# server/regression_server.R - VERSI DIPERBAIKI DENGAN INTERPRETASI LENGKAP
# Fungsi helper untuk generate interpretasi (tambahkan di awal regression_server.R)
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
          } else if (max_vif > 5) {
            cat(sprintf("Berdasarkan uji VIF yang telah dilakukan, diketahui bahwa model regresi \nmengalami multikolinearitas SEDANG karena terdapat variabel dengan VIF antara 5-10.\n\n"))
            cat("Variabel yang perlu diwaspadai:", paste(warning_vars, collapse = ", "), "\n\n")
            cat("Kesimpulan: Asumsi tidak adanya multikolinearitas CUKUP terpenuhi dengan catatan.\n\n")
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
        "Dari hasil tersebut, diperoleh", paste(p_values_text, collapse = " dan "), ".",
        if (length(significant_vars) == 1) {
          paste("Karena p-value lebih kecil dari 0,05, maka", significant_vars[1], 
                "berpengaruh signifikan terhadap", input$reg_dep_var, "pada tingkat signifikansi 5%.")
        } else {
          paste("Karena semua p-value lebih kecil dari 0,05, maka", 
                paste(significant_vars[-length(significant_vars)], collapse = ", "), 
                "maupun", significant_vars[length(significant_vars)], 
                "berpengaruh signifikan terhadap", input$reg_dep_var, "pada tingkat signifikansi 5%.")
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
                "akan", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
                input$reg_dep_var, "sebesar", sprintf("%.6f", abs(coeff_value)), 
                "persen, dengan asumsi variabel lain tetap konstan.")
        } else if (grepl("(?i)(family.*size|familysize|ukuran.*keluarga|size.*family)", var)) {
          # Variabel family size (satuan: orang)
          paste("Koefisien", var, "sebesar", sprintf("%.3f", coeff_value), 
                "menunjukkan bahwa setiap tambahan satu orang dalam", var, 
                "akan", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
                input$reg_dep_var, "sebesar", sprintf("%.3f", abs(coeff_value)), 
                "persen, dengan asumsi variabel lain tetap konstan.")
        } else {
          # Variabel dengan satuan persen (default)
          paste("Koefisien", var, "sebesar", sprintf("%.3f", coeff_value), 
                "mengindikasikan bahwa setiap kenaikan satu persen", var, 
                "akan", if(coeff_value > 0) "meningkatkan" else "menurunkan", 
                input$reg_dep_var, "sebesar", sprintf("%.3f", abs(coeff_value)), 
                "persen, dengan asumsi variabel lain tetap konstan.")
        }
      })
      
      coeff_interpretation <- paste(coeff_texts, collapse = " Sementara itu, ")
    }
    
    # --- BAGIAN 3: INTERPRETASI R-SQUARED ---
    r_squared_interpretation <- paste(
      "Nilai R² sebesar", sprintf("%.3f", r_squared), 
      "menunjukkan bahwa", sprintf("%.1f", r_squared * 100), 
      "% variasi", input$reg_dep_var, 
      "dapat dijelaskan oleh", paste(input$reg_indep_vars, collapse = ", "), 
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

# --- 6. LOGIKA UNDUH - DIPERBAIKI ---
output$download_regression_summary <- downloadHandler(
  filename = function() {
    paste("laporan-regresi-lengkap-", Sys.Date(), ".", input$regression_format, sep = "")
  },
  content = function(file) {
    model <- regression_model()
    
    if (!is.null(model)) {
      # --- BAGIAN 1: SUMMARY STATISTIK MODEL ---
      summary_text <- capture.output(summary(model))
      
      # --- BAGIAN 2: INTERPRETASI LENGKAP (SAMA DENGAN DASHBOARD) ---
      interpretation_text <- generate_regression_interpretation(
        model, 
        input$reg_dep_var, 
        input$reg_indep_vars
      )
      
      # --- BAGIAN 3: UJI ASUMSI LENGKAP ---
      assumptions_text <- "\n\n=== UJI ASUMSI REGRESI ==="
      
      tryCatch({
        # 1. Uji normalitas residual
        residuals_data <- residuals(model)
        if (length(residuals_data) <= 5000) {
          shapiro_test <- shapiro.test(residuals_data)
          assumptions_text <- paste(assumptions_text, 
                                    "\n\n1. UJI NORMALITAS RESIDUAL (SHAPIRO-WILK)",
                                    "Hipotesis:",
                                    "H₀: Residual berdistribusi normal",
                                    "H₁: Residual tidak berdistribusi normal",
                                    "Tingkat signifikansi (α): 0.05",
                                    "",
                                    "Hasil Pengujian:",
                                    sprintf("Statistik W: %.6f", shapiro_test$statistic),
                                    sprintf("P-value: %.6f", shapiro_test$p.value),
                                    "",
                                    "Interpretasi:",
                                    if (shapiro_test$p.value < 0.05) {
                                      sprintf("Berdasarkan uji Shapiro-Wilk yang telah dilakukan, diketahui bahwa residual model regresi TIDAK berdistribusi normal karena nilai p-value (%.6f) < α (0.05), sehingga keputusan yang diambil adalah TOLAK H₀.\n\nKesimpulan: Asumsi normalitas residual TIDAK terpenuhi.", shapiro_test$p.value)
                                    } else {
                                      sprintf("Berdasarkan uji Shapiro-Wilk yang telah dilakukan, diketahui bahwa residual model regresi berdistribusi normal karena nilai p-value (%.6f) ≥ α (0.05), sehingga keputusan yang diambil adalah TERIMA H₀.\n\nKesimpulan: Asumsi normalitas residual TERPENUHI.", shapiro_test$p.value)
                                    },
                                    sep = "\n")
        }
        
        # 2. Uji homoskedastisitas
        if (requireNamespace("lmtest", quietly = TRUE)) {
          bp_test <- lmtest::bptest(model)
          assumptions_text <- paste(assumptions_text,
                                    "\n\n2. UJI HOMOSKEDASTISITAS (BREUSCH-PAGAN)",
                                    "Hipotesis:",
                                    "H₀: Varians residual konstan (homoskedastisitas)",
                                    "H₁: Varians residual tidak konstan (heteroskedastisitas)",
                                    "Tingkat signifikansi (α): 0.05",
                                    "",
                                    "Hasil Pengujian:",
                                    sprintf("Statistik BP: %.6f", bp_test$statistic),
                                    sprintf("P-value: %.6f", bp_test$p.value),
                                    "",
                                    "Interpretasi:",
                                    if (bp_test$p.value < 0.05) {
                                      sprintf("Berdasarkan uji Breusch-Pagan yang telah dilakukan, diketahui bahwa model regresi mengalami masalah heteroskedastisitas karena nilai p-value (%.6f) < α (0.05), sehingga keputusan yang diambil adalah TOLAK H₀.\n\nKesimpulan: Asumsi homoskedastisitas TIDAK terpenuhi.", bp_test$p.value)
                                    } else {
                                      sprintf("Berdasarkan uji Breusch-Pagan yang telah dilakukan, diketahui bahwa model regresi TIDAK mengalami masalah heteroskedastisitas karena nilai p-value (%.6f) ≥ α (0.05), sehingga keputusan yang diambil adalah TERIMA H₀.\n\nKesimpulan: Asumsi homoskedastisitas TERPENUHI.", bp_test$p.value)
                                    },
                                    sep = "\n")
        }
        
        # 3. Uji multikolinearitas (VIF)
        if (length(input$reg_indep_vars) > 1 && requireNamespace("car", quietly = TRUE)) {
          vif_values <- car::vif(model)
          vif_text <- paste(sapply(names(vif_values), function(x) 
            sprintf("%s: %.4f", x, vif_values[x])), collapse = "\n")
          
          assumptions_text <- paste(assumptions_text,
                                    "\n\n3. UJI MULTIKOLINEARITAS (VIF)",
                                    "Variance Inflation Factor (VIF) untuk setiap variabel:",
                                    vif_text,
                                    "",
                                    "Interpretasi:",
                                    if (max(vif_values) > 10) {
                                      "Berdasarkan uji VIF yang telah dilakukan, diketahui bahwa model regresi mengalami masalah multikolinearitas SERIUS karena terdapat variabel dengan VIF > 10.\n\nKesimpulan: Asumsi tidak adanya multikolinearitas TIDAK terpenuhi."
                                    } else if (max(vif_values) > 5) {
                                      "Berdasarkan uji VIF yang telah dilakukan, diketahui bahwa model regresi mengalami masalah multikolinearitas SEDANG karena terdapat variabel dengan VIF > 5.\n\nKesimpulan: Asumsi tidak adanya multikolinearitas TIDAK terpenuhi sepenuhnya."
                                    } else {
                                      "Berdasarkan uji VIF yang telah dilakukan, diketahui bahwa model regresi TIDAK mengalami masalah multikolinearitas karena semua variabel memiliki VIF < 5.\n\nKesimpulan: Asumsi tidak adanya multikolinearitas TERPENUHI."
                                    },
                                    sep = "\n")
        } else if (length(input$reg_indep_vars) == 1) {
          assumptions_text <- paste(assumptions_text,
                                    "\n\n3. UJI MULTIKOLINEARITAS (VIF)",
                                    "Model hanya memiliki 1 variabel independen, sehingga tidak ada masalah multikolinearitas.",
                                    "Kesimpulan: Asumsi tidak adanya multikolinearitas TERPENUHI.",
                                    sep = "\n")
        }
        
      }, error = function(e) {
        assumptions_text <- paste(assumptions_text, "\n\nError dalam uji asumsi:", e$message)
      })
      
      # --- GABUNGKAN SEMUA KONTEN ---
      full_content <- paste(
        "LAPORAN ANALISIS REGRESI LINEAR BERGANDA",
        paste("Tanggal:", Sys.Date()),
        "",
        "=== SUMMARY STATISTIK MODEL ===",
        paste(summary_text, collapse = "\n"),
        "",
        interpretation_text,
        "",
        assumptions_text,
        "",
        "=== CATATAN ===",
        "1. Periksa plot residual untuk validasi visual asumsi",
        "2. Pertimbangkan transformasi variabel jika asumsi dilanggar",
        sep = "\n"
      )
      
      # --- RENDER KE FILE ---
      tryCatch({
        rmarkdown::render(
          input = "text_report.Rmd",
          output_file = file,
          output_format = if(input$regression_format == "pdf") "pdf_document" else "word_document",
          params = list(
            report_title = "Laporan Analisis Regresi Linear Berganda",
            text_output = full_content
          ),
          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        # Fallback: buat file teks sederhana
        writeLines(full_content, file)
      })
      
    } else {
      # Jika model belum dibuat
      rmarkdown::render(
        input = "text_report.Rmd",
        output_file = file,
        output_format = if(input$regression_format == "pdf") "pdf_document" else "word_document",
        params = list(
          report_title = "Laporan Regresi",
          text_output = "Model regresi belum dibuat. Silakan buat model terlebih dahulu."
        ),
        envir = new.env(parent = globalenv())
      )
    }
  }
)

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