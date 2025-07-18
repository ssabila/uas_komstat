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
output$anova_summary_table <- renderPrint({
  req(analysis_results$anova)
  analysis_results$anova
})

output$anova_interpretation <- renderText({
  req(analysis_results$anova)
  summary_aov <- analysis_results$anova
  p_values <- summary_aov[[1]]$`Pr(>F)`
  
  "Interpretasi Tabel ANOVA:\n
  Perhatikan kolom 'Pr(>F)' (p-value). Jika p-value untuk suatu variabel atau interaksi < 0.05, maka variabel tersebut memiliki pengaruh yang signifikan secara statistik terhadap variabel dependen."
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