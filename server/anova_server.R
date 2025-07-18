# --- 1. RENDER UI DINAMIS ---
output$anova_dependent_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("anova_dep_var", "Pilih Variabel Dependen (Numerik):", choices = numeric_vars)
})

output$anova_independent1_selector <- renderUI({
  req(processed_data$current)
  # Memastikan variabel grup memiliki lebih dari 1 level
  cat_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) (is.character(x) || is.factor(x)) && length(unique(x)) > 1)]
  selectInput("anova_indep_var1", "Pilih Variabel Independen 1 (Kategorik):", choices = cat_vars)
})

output$anova_independent2_selector <- renderUI({
  req(input$anova_type == "two_way", processed_data$current)
  cat_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) (is.character(x) || is.factor(x)) && length(unique(x)) > 1)]
  selectInput("anova_indep_var2", "Pilih Variabel Independen 2 (Kategorik):", choices = cat_vars)
})

# --- 2. LOGIKA ANOVA ---
observeEvent(input$run_anova, {
  req(processed_data$current, input$anova_dep_var, input$anova_indep_var1)
  
  formula_str <- paste(input$anova_dep_var, "~", input$anova_indep_var1)
  
  if (input$anova_type == "two_way") {
    req(input$anova_indep_var2)
    interaction_term <- if (input$anova_interaction) "*" else "+"
    formula_str <- paste(formula_str, interaction_term, input$anova_indep_var2)
  }
  
  model <- aov(as.formula(formula_str), data = processed_data$current)
  summary_model <- summary(model)
  
  # Simpan hasil untuk diunduh
  analysis_results$anova <- summary_model
})

# --- 3. TAMPILKAN HASIL ---
# --- 3. TAMPILKAN HASIL ---
output$anova_summary_table <- renderPrint({
  req(analysis_results$anova)
  analysis_results$anova
})

output$anova_interpretation <- renderText({
  req(analysis_results$anova)
  summary_aov <- analysis_results$anova
  p_values <- summary_aov[[1]]$`Pr(>F)`
  
  # Membuat interpretasi berdasarkan tipe ANOVA
  if (input$anova_type == "one_way") {
    # ANOVA Satu Arah
    main_p_value <- p_values[1]
    
    if (!is.na(main_p_value)) {
      if (main_p_value < 0.05) {
        interpretation <- paste0(
          "Berdasarkan uji ANOVA satu arah yang dilakukan, didapatkan p-value = ", 
          round(main_p_value, 4), 
          " sehingga p-value lebih kecil dari tingkat signifikansi 0.05. ",
          "Oleh karena itu, cukup bukti untuk menolak hipotesis nol yang menyatakan bahwa rata-rata semua kelompok sama. ",
          "Dengan demikian, dapat disimpulkan bahwa terdapat setidaknya satu kelompok yang memiliki rata-rata yang berbeda secara signifikan dibandingkan kelompok lainnya."
        )
      } else {
        interpretation <- paste0(
          "Berdasarkan uji ANOVA satu arah yang dilakukan, didapatkan p-value = ", 
          round(main_p_value, 4), 
          " sehingga p-value lebih besar dari tingkat signifikansi 0.05. ",
          "Oleh karena itu, tidak cukup bukti untuk menolak hipotesis nul yang menyatakan bahwa rata-rata semua kelompok sama. ",
          "Dengan demikian, dapat disimpulkan bahwa tidak ada perbedaan rata-rata yang signifikan antar kelompok."
        )
      }
    } else {
      interpretation <- "Tidak dapat menghitung p-value untuk analisis ini."
    }
    
  } else if (input$anova_type == "two_way") {
    # ANOVA Dua Arah
    var1_p_value <- p_values[1]
    var2_p_value <- p_values[2]
    
    interpretation <- "Berdasarkan uji ANOVA dua arah yang dilakukan:\n\n"
    
    # Interpretasi untuk variabel independen 1
    if (!is.na(var1_p_value)) {
      if (var1_p_value < 0.05) {
        interpretation <- paste0(interpretation,
          "1. Variabel '", input$anova_indep_var1, "': p-value = ", round(var1_p_value, 4), 
          " (< 0.05). Terdapat pengaruh yang signifikan dari variabel ", input$anova_indep_var1, 
          " terhadap ", input$anova_dep_var, ".\n\n")
      } else {
        interpretation <- paste0(interpretation,
          "1. Variabel '", input$anova_indep_var1, "': p-value = ", round(var1_p_value, 4), 
          " (≥ 0.05). Tidak terdapat pengaruh yang signifikan dari variabel ", input$anova_indep_var1, 
          " terhadap ", input$anova_dep_var, ".\n\n")
      }
    }
    
    # Interpretasi untuk variabel independen 2
    if (!is.na(var2_p_value)) {
      if (var2_p_value < 0.05) {
        interpretation <- paste0(interpretation,
          "2. Variabel '", input$anova_indep_var2, "': p-value = ", round(var2_p_value, 4), 
          " (< 0.05). Terdapat pengaruh yang signifikan dari variabel ", input$anova_indep_var2, 
          " terhadap ", input$anova_dep_var, ".\n\n")
      } else {
        interpretation <- paste0(interpretation,
          "2. Variabel '", input$anova_indep_var2, "': p-value = ", round(var2_p_value, 4), 
          " (≥ 0.05). Tidak terdapat pengaruh yang signifikan dari variabel ", input$anova_indep_var2, 
          " terhadap ", input$anova_dep_var, ".\n\n")
      }
    }
    
    # Interpretasi untuk interaksi (jika ada)
    if (input$anova_interaction && length(p_values) >= 3) {
      interaction_p_value <- p_values[3]
      if (!is.na(interaction_p_value)) {
        if (interaction_p_value < 0.05) {
          interpretation <- paste0(interpretation,
            "3. Interaksi '", input$anova_indep_var1, ":", input$anova_indep_var2, "': p-value = ", 
            round(interaction_p_value, 4), 
            " (< 0.05). Terdapat efek interaksi yang signifikan antara ", input$anova_indep_var1, 
            " dan ", input$anova_indep_var2, " terhadap ", input$anova_dep_var, ".")
        } else {
          interpretation <- paste0(interpretation,
            "3. Interaksi '", input$anova_indep_var1, ":", input$anova_indep_var2, "': p-value = ", 
            round(interaction_p_value, 4), 
            " (≥ 0.05). Tidak terdapat efek interaksi yang signifikan antara ", input$anova_indep_var1, 
            " dan ", input$anova_indep_var2, " terhadap ", input$anova_dep_var, ".")
        }
      }
    }
  }
  
  return(interpretation)
})

output$anova_plot <- renderPlotly({
  req(processed_data$current, input$anova_dep_var, input$anova_indep_var1)
  
  p <- ggplot(processed_data$current, aes_string(x = input$anova_indep_var1, y = input$anova_dep_var, fill = input$anova_indep_var1)) +
    geom_boxplot() +
    labs(title = paste("Boxplot", input$anova_dep_var, "berdasarkan", input$anova_indep_var1)) +
    theme_minimal()
  
  if (input$anova_type == "two_way" && !is.null(input$anova_indep_var2)) {
    p <- p + facet_wrap(as.formula(paste("~", input$anova_indep_var2)))
  }
  
  ggplotly(p)
})

# --- 4. LOGIKA UNDUH ---
output$download_anova_result <- downloadHandler(
  filename = function() {
    paste("hasil-anova-", Sys.Date(), ".", input$anova_format, sep = "")
  },
  content = function(file) {
    req(analysis_results$anova)
    
    # Ambil output teks dari hasil ANOVA
    text_to_render <- capture.output(analysis_results$anova)
    
    # Render laporan menggunakan template
    rmarkdown::render(
      input = "text_report.Rmd",
      output_file = file,
      output_format = if(input$anova_format == "pdf") "pdf_document" else "word_document",
      params = list(
        report_title = "Hasil Analisis ANOVA",
        text_output = text_to_render
      ),
      envir = new.env(parent = globalenv())
    )
  }
)