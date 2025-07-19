# server/regression_server.R - VERSI SEDERHANA DAN AMAN

cat("Loading regression server...\n")

# --- 1. DEPENDENT VARIABLE SELECTOR ---
output$reg_dependent_selector <- renderUI({
  cat("Rendering dependent selector...\n")
  
  if (is.null(processed_data$current)) {
    return(div(
      style = "padding: 10px; background: #f8d7da; border-radius: 5px;",
      "Data belum dimuat. Silakan upload data terlebih dahulu."
    ))
  }
  
  # Get numeric variables
  tryCatch({
    numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
    
    if (length(numeric_vars) == 0) {
      return(div(
        style = "padding: 10px; background: #fff3cd; border-radius: 5px;",
        "Tidak ada variabel numerik dalam data."
      ))
    }
    
    return(selectInput(
      "reg_dep_var",
      "Pilih Variabel Dependen (Y):",
      choices = numeric_vars,
      selected = numeric_vars[1]
    ))
    
  }, error = function(e) {
    return(div(
      style = "padding: 10px; background: #f8d7da; border-radius: 5px;",
      paste("Error:", e$message)
    ))
  })
})

# --- 2. INDEPENDENT VARIABLE SELECTOR ---
output$reg_independent_selector <- renderUI({
  cat("Rendering independent selector...\n")
  
  if (is.null(processed_data$current)) {
    return(div("Data belum dimuat"))
  }
  
  if (is.null(input$reg_dep_var) || input$reg_dep_var == "") {
    return(div(
      style = "padding: 10px; background: #d1ecf1; border-radius: 5px;",
      "Pilih variabel dependen terlebih dahulu"
    ))
  }
  
  tryCatch({
    numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
    available_vars <- setdiff(numeric_vars, input$reg_dep_var)
    
    if (length(available_vars) == 0) {
      return(div(
        style = "padding: 10px; background: #fff3cd; border-radius: 5px;",
        "Tidak ada variabel independen yang tersedia"
      ))
    }
    
    return(selectizeInput(
      "reg_indep_vars",
      "Pilih Variabel Independen (X):",
      choices = available_vars,
      multiple = TRUE,
      options = list(
        placeholder = "Pilih satu atau lebih variabel...",
        plugins = list('remove_button')
      )
    ))
    
  }, error = function(e) {
    return(div(paste("Error:", e$message)))
  })
})

# --- 3. VALIDATION STATUS ---
output$regression_validation_status <- renderUI({
  if (is.null(processed_data$current)) {
    return(div(
      style = "padding: 10px; background: #f8d7da; border-radius: 5px; margin-top: 10px;",
      icon("times-circle", style = "color: #dc3545;"),
      " Data belum dimuat"
    ))
  }
  
  if (is.null(input$reg_dep_var) || input$reg_dep_var == "") {
    return(div(
      style = "padding: 10px; background: #fff3cd; border-radius: 5px; margin-top: 10px;",
      icon("exclamation-triangle", style = "color: #ffc107;"),
      " Pilih variabel dependen"
    ))
  }
  
  if (is.null(input$reg_indep_vars) || length(input$reg_indep_vars) == 0) {
    return(div(
      style = "padding: 10px; background: #fff3cd; border-radius: 5px; margin-top: 10px;",
      icon("exclamation-triangle", style = "color: #ffc107;"),
      " Pilih variabel independen"
    ))
  }
  
  return(div(
    style = "padding: 10px; background: #d4edda; border-radius: 5px; margin-top: 10px;",
    icon("check-circle", style = "color: #28a745;"),
    " Siap untuk analisis!"
  ))
})

# --- 4. REGRESSION MODEL ---
regression_model <- eventReactive(input$run_regression, {
  cat("Running regression analysis...\n")
  
  # Validasi input
  req(processed_data$current, input$reg_dep_var, input$reg_indep_vars)
  
  tryCatch({
    # Buat formula
    formula_str <- paste(input$reg_dep_var, "~", paste(input$reg_indep_vars, collapse = " + "))
    cat("Formula:", formula_str, "\n")
    
    # Fit model
    model <- lm(as.formula(formula_str), data = processed_data$current)
    
    # Simpan hasil
    analysis_results$regression <- list(
      model = model,
      summary = summary(model),
      formula = formula_str
    )
    
    showNotification("Model regresi berhasil dibuat!", type = "success")
    return(model)
    
  }, error = function(e) {
    cat("Error in regression:", e$message, "\n")
    showNotification(paste("Error:", e$message), type = "error")
    return(NULL)
  })
})

# --- 5. OUTPUT RESULTS ---
output$regression_summary <- renderPrint({
  model <- regression_model()
  
  if (is.null(model)) {
    cat("=== STATUS REGRESI ===\n")
    cat("Model belum dibuat.\n\n")
    cat("LANGKAH:\n")
    cat("1. Pilih variabel dependen (Y)\n")
    cat("2. Pilih variabel independen (X)\n") 
    cat("3. Klik 'Bangun Model'\n")
  } else {
    cat("=== HASIL REGRESI ===\n\n")
    summary(model)
  }
})

# --- 6. DIAGNOSTIC PLOTS ---
output$regression_qqplot <- renderPlot({
  model <- regression_model()
  
  if (is.null(model)) {
    plot(1, 1, type = "n", main = "Model belum dibuat", 
         xlab = "", ylab = "")
    text(1, 1, "Klik 'Bangun Model'\nterlebih dahulu", cex = 1.2)
  } else {
    tryCatch({
      plot(model, which = 2, main = "Q-Q Plot Residual")
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error dalam Q-Q Plot")
      text(1, 1, paste("Error:", e$message))
    })
  }
})

output$regression_residual_plot <- renderPlot({
  model <- regression_model()
  
  if (is.null(model)) {
    plot(1, 1, type = "n", main = "Model belum dibuat",
         xlab = "", ylab = "")
    text(1, 1, "Klik 'Bangun Model'\nterlebih dahulu", cex = 1.2)
  } else {
    tryCatch({
      plot(model, which = 1, main = "Residuals vs Fitted")
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error dalam Residual Plot")
      text(1, 1, paste("Error:", e$message))
    })
  }
})

# --- 7. VIF TEST ---
output$regression_vif <- renderPrint({
  model <- regression_model()
  
  if (is.null(model)) {
    cat("Model belum dibuat.")
  } else if (length(input$reg_indep_vars) < 2) {
    cat("VIF memerlukan minimal 2 variabel independen.")
  } else {
    tryCatch({
      if (requireNamespace("car", quietly = TRUE)) {
        vif_values <- car::vif(model)
        cat("=== VARIANCE INFLATION FACTOR (VIF) ===\n\n")
        print(vif_values)
        cat("\nInterpretasi:\n")
        cat("VIF < 5: Tidak ada multikolinearitas\n")
        cat("VIF 5-10: Multikolinearitas sedang\n") 
        cat("VIF > 10: Multikolinearitas serius\n")
      } else {
        cat("Package 'car' tidak tersedia untuk VIF")
      }
    }, error = function(e) {
      cat("Error:", e$message)
    })
  }
})

# --- 8. INTERPRETATION ---
output$regression_interpretation <- renderText({
  model <- regression_model()
  
  if (is.null(model)) {
    return("Model belum dibuat.\n\nLangkah:\n1. Pilih Y\n2. Pilih X\n3. Klik 'Bangun Model'")
  }
  
  summary_model <- summary(model)
  r_squared <- summary_model$r.squared
  
  interpretation <- paste0(
    "=== INTERPRETASI MODEL ===\n\n",
    "Formula: ", input$reg_dep_var, " ~ ", paste(input$reg_indep_vars, collapse = " + "), "\n\n",
    "R-squared: ", round(r_squared, 4), " (", round(r_squared * 100, 1), "%)\n",
    "Model menjelaskan ", round(r_squared * 100, 1), "% variasi dalam ", input$reg_dep_var, "\n\n",
    "Kualitas Model:\n",
    if (r_squared > 0.7) {
      "✓ BAIK (R² > 70%)"
    } else if (r_squared > 0.5) {
      "○ SEDANG (R² 50-70%)" 
    } else {
      "△ PERLU PERBAIKAN (R² < 50%)"
    }
  )
  
  return(interpretation)
})

# --- 9. ADDITIONAL PLOTS ---
output$cooks_distance_plot <- renderPlot({
  model <- regression_model()
  
  if (is.null(model)) {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model'")
  } else {
    tryCatch({
      cooks_d <- cooks.distance(model)
      plot(cooks_d, type = "h", main = "Cook's Distance",
           ylab = "Cook's Distance", xlab = "Observation")
      abline(h = 1, col = "red", lty = 2)
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error Cook's Distance")
      text(1, 1, paste("Error:", e$message))
    })
  }
})

output$leverage_plot <- renderPlot({
  model <- regression_model()
  
  if (is.null(model)) {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model'")
  } else {
    tryCatch({
      leverage <- hatvalues(model)
      plot(leverage, type = "h", main = "Leverage Values",
           ylab = "Leverage", xlab = "Observation") 
      abline(h = 2 * length(coef(model)) / length(leverage), col = "red", lty = 2)
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error Leverage")
      text(1, 1, paste("Error:", e$message))
    })
  }
})

output$residual_leverage_plot <- renderPlot({
  model <- regression_model()
  
  if (is.null(model)) {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model'")
  } else {
    tryCatch({
      plot(model, which = 5, main = "Residuals vs Leverage")
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error Residual vs Leverage")
      text(1, 1, paste("Error:", e$message))
    })
  }
})

# --- 10. NORMALITY AND HOMOSCEDASTICITY TESTS ---
output$normality_residual_interpretation <- renderPrint({
  model <- regression_model()
  
  if (is.null(model)) {
    cat("Model belum dibuat.")
  } else {
    tryCatch({
      residuals_data <- residuals(model)
      
      if (length(residuals_data) <= 5000) {
        shapiro_test <- shapiro.test(residuals_data)
        cat("=== UJI NORMALITAS (SHAPIRO-WILK) ===\n")
        cat("W =", round(shapiro_test$statistic, 6), "\n")
        cat("p-value =", round(shapiro_test$p.value, 6), "\n\n")
        
        if (shapiro_test$p.value < 0.05) {
          cat("Kesimpulan: Residual TIDAK berdistribusi normal (p < 0.05)")
        } else {
          cat("Kesimpulan: Residual berdistribusi normal (p > 0.05)")
        }
      } else {
        cat("Sampel terlalu besar untuk Shapiro-Wilk.\nGunakan Q-Q plot untuk evaluasi visual.")
      }
    }, error = function(e) {
      cat("Error:", e$message)
    })
  }
})

output$homoskedasticity_interpretation <- renderPrint({
  model <- regression_model()
  
  if (is.null(model)) {
    cat("Model belum dibuat.")
  } else {
    tryCatch({
      if (requireNamespace("lmtest", quietly = TRUE)) {
        bp_test <- lmtest::bptest(model)
        cat("=== UJI HOMOSKEDASTISITAS (BREUSCH-PAGAN) ===\n")
        cat("BP =", round(bp_test$statistic, 6), "\n")
        cat("p-value =", round(bp_test$p.value, 6), "\n\n")
        
        if (bp_test$p.value < 0.05) {
          cat("Kesimpulan: Ada heteroskedastisitas (p < 0.05)")
        } else {
          cat("Kesimpulan: Homoskedastisitas terpenuhi (p > 0.05)")
        }
      } else {
        cat("Package 'lmtest' tidak tersedia.\nGunakan plot Residuals vs Fitted untuk evaluasi visual.")
      }
    }, error = function(e) {
      cat("Error:", e$message)
    })
  }
})

cat("✓ Regression server loaded successfully\n")