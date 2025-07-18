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
    
    # Simpan ringkasan model untuk diunduh
    analysis_results$regression <- summary(model)
    
    cat("Regression model created successfully\n")
    return(model)
    
  }, error = function(e) {
    showNotification(paste("Error dalam analisis regresi:", e$message), type = "error")
    return(NULL)
  })
})

# --- 3. TAMPILKAN HASIL - DIPERBAIKI ---
output$regression_summary <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    summary(model)
  } else {
    cat("Model regresi belum dibuat. Klik 'Bangun Model' untuk memulai analisis.")
  }
})

output$regression_qqplot <- renderPlot({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      plot(model, which = 2, main = "Q-Q Plot Residual")
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error membuat Q-Q plot")
      text(1, 1, paste("Error:", e$message))
    })
  } else {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model' terlebih dahulu")
  }
})

output$regression_residual_plot <- renderPlot({
  model <- regression_model()
  
  if (!is.null(model)) {
    tryCatch({
      plot(model, which = 1, main = "Residuals vs Fitted")
    }, error = function(e) {
      plot(1, 1, type = "n", main = "Error membuat residual plot")
      text(1, 1, paste("Error:", e$message))
    })
  } else {
    plot(1, 1, type = "n", main = "Model belum dibuat")
    text(1, 1, "Klik 'Bangun Model' terlebih dahulu")
  }
})

output$regression_vif <- renderPrint({
  model <- regression_model()
  
  if (!is.null(model)) {
    if (length(input$reg_indep_vars) > 1) {
      tryCatch({
        if (requireNamespace("car", quietly = TRUE)) {
          car::vif(model)
        } else {
          cat("Package 'car' tidak tersedia untuk menghitung VIF")
        }
      }, error = function(e) {
        cat("Error menghitung VIF:", e$message)
      })
    } else {
      cat("VIF (Variance Inflation Factor) memerlukan minimal 2 variabel independen.")
    }
  } else {
    cat("Model belum dibuat. Klik 'Bangun Model' terlebih dahulu.")
  }
})

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
      
      "PANDUAN UJI ASUMSI:\n",
      "1. NORMALITAS: Q-Q plot harus membentuk garis lurus\n",
      "2. HOMOSKEDASTISITAS: Residual vs Fitted harus tersebar acak\n",
      "3. MULTIKOLINEARITAS: VIF < 10 (idealnya < 5)\n",
      "4. LINIERITAS: Hubungan X-Y harus linear\n\n",
      
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

# --- 4. LOGIKA UNDUH - DIPERBAIKI ---
output$download_regression_summary <- downloadHandler(
  filename = function() {
    paste("ringkasan-regresi-", Sys.Date(), ".", input$regression_format, sep = "")
  },
  content = function(file) {
    model <- regression_model()
    
    if (!is.null(model)) {
      text_to_render <- capture.output(summary(model))
      
      rmarkdown::render(
        input = "text_report.Rmd",
        output_file = file,
        output_format = if(input$regression_format == "pdf") "pdf_document" else "word_document",
        params = list(
          report_title = "Ringkasan Model Regresi",
          text_output = paste(text_to_render, collapse = "\n")
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

# --- 5. TAMBAHAN: OBSERVER UNTUK DEBUGGING ---
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