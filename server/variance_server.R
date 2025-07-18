# --- 1. UI DINAMIS ---
output$variance_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("variance_var", 
              "Pilih Variabel Numerik:", 
              choices = numeric_vars)
})

output$variance_group_selector <- renderUI({
  req(processed_data$current, input$variance_test_type == "two_sample")
  
  # Filter variabel kategori yang valid (tepat 2 level)
  data <- processed_data$current
  valid_group_vars <- sapply(names(data), function(col_name) {
    x <- data[[col_name]]
    if (!is.character(x) && !is.factor(x)) return(FALSE)
    
    clean_x <- x[!is.na(x)]
    if (length(clean_x) == 0) return(FALSE)
    
    unique_levels <- unique(clean_x)
    if (length(unique_levels) != 2) return(FALSE)
    
    level_counts <- table(clean_x)
    if (any(level_counts < 2)) return(FALSE)
    
    return(TRUE)
  })
  
  cat_vars <- names(data)[valid_group_vars]
  if (length(cat_vars) == 0) {
    cat_vars <- c("Tidak ada variabel grup yang valid" = "")
  }
  
  selectInput("variance_group_var", 
              "Pilih Variabel Grup (2 Kategori):", 
              choices = cat_vars)
})

# --- 2. FUNGSI UJI VARIANCE ---
perform_variance_test <- function(data, variable, test_type, sigma2 = NULL, group_var = NULL, alternative = "two.sided", alpha = 0.05) {
  
  if (test_type == "one_sample") {
    # Chi-square test untuk variance satu populasi
    values <- data[[variable]][!is.na(data[[variable]])]
    n <- length(values)
    sample_var <- var(values)
    
    # Chi-square statistic
    chi_stat <- (n - 1) * sample_var / sigma2
    
    # P-value berdasarkan alternative hypothesis
    if (alternative == "two.sided") {
      p_value <- 2 * min(pchisq(chi_stat, df = n-1), 1 - pchisq(chi_stat, df = n-1))
    } else if (alternative == "greater") {
      p_value <- 1 - pchisq(chi_stat, df = n-1)
    } else { # less
      p_value <- pchisq(chi_stat, df = n-1)
    }
    
    # Confidence interval untuk variance
    if (alternative == "two.sided") {
      ci_lower <- (n-1) * sample_var / qchisq(1 - alpha/2, df = n-1)
      ci_upper <- (n-1) * sample_var / qchisq(alpha/2, df = n-1)
    } else {
      ci_lower <- ifelse(alternative == "greater", 
                         (n-1) * sample_var / qchisq(1 - alpha, df = n-1), 0)
      ci_upper <- ifelse(alternative == "less", 
                         (n-1) * sample_var / qchisq(alpha, df = n-1), Inf)
    }
    
    result <- list(
      test_type = "Chi-square test for variance",
      statistic = chi_stat,
      df = n - 1,
      p_value = p_value,
      sample_var = sample_var,
      hypothesized_var = sigma2,
      conf_int = c(ci_lower, ci_upper),
      alternative = alternative,
      alpha = alpha,
      n = n
    )
    
  } else { # two_sample
    # F-test untuk perbandingan variance dua kelompok
    complete_data <- data[complete.cases(data[c(variable, group_var)]), ]
    groups <- unique(complete_data[[group_var]])
    
    group1_data <- complete_data[[variable]][complete_data[[group_var]] == groups[1]]
    group2_data <- complete_data[[variable]][complete_data[[group_var]] == groups[2]]
    
    var1 <- var(group1_data)
    var2 <- var(group2_data)
    n1 <- length(group1_data)
    n2 <- length(group2_data)
    
    # F statistic
    f_stat <- var1 / var2
    
    # P-value berdasarkan alternative hypothesis
    if (alternative == "two.sided") {
      p_value <- 2 * min(pf(f_stat, df1 = n1-1, df2 = n2-1), 
                         1 - pf(f_stat, df1 = n1-1, df2 = n2-1))
    } else if (alternative == "greater") {
      p_value <- 1 - pf(f_stat, df1 = n1-1, df2 = n2-1)
    } else { # less
      p_value <- pf(f_stat, df1 = n1-1, df2 = n2-1)
    }
    
    # Confidence interval untuk ratio variance
    if (alternative == "two.sided") {
      f_lower <- qf(alpha/2, df1 = n1-1, df2 = n2-1)
      f_upper <- qf(1 - alpha/2, df1 = n1-1, df2 = n2-1)
      ci_lower <- f_stat / f_upper
      ci_upper <- f_stat / f_lower
    } else {
      ci_lower <- ifelse(alternative == "greater", f_stat / qf(1 - alpha, df1 = n1-1, df2 = n2-1), 0)
      ci_upper <- ifelse(alternative == "less", f_stat / qf(alpha, df1 = n1-1, df2 = n2-1), Inf)
    }
    
    result <- list(
      test_type = "F-test for equality of variances",
      statistic = f_stat,
      df1 = n1 - 1,
      df2 = n2 - 1,
      p_value = p_value,
      var1 = var1,
      var2 = var2,
      group1 = groups[1],
      group2 = groups[2],
      n1 = n1,
      n2 = n2,
      conf_int = c(ci_lower, ci_upper),
      alternative = alternative,
      alpha = alpha
    )
  }
  
  return(result)
}

# --- 3. LOGIKA UJI VARIANCE ---
variance_results <- eventReactive(input$run_variance_test, {
  req(processed_data$current, input$variance_var, input$variance_test_type)
  
  result <- tryCatch({
    if (input$variance_test_type == "one_sample") {
      req(input$variance_sigma2)
      perform_variance_test(
        data = processed_data$current,
        variable = input$variance_var,
        test_type = "one_sample",
        sigma2 = input$variance_sigma2,
        alternative = input$variance_alternative,
        alpha = input$variance_alpha
      )
    } else {
      req(input$variance_group_var)
      perform_variance_test(
        data = processed_data$current,
        variable = input$variance_var,
        test_type = "two_sample",
        group_var = input$variance_group_var,
        alternative = input$variance_alternative,
        alpha = input$variance_alpha
      )
    }
  }, error = function(e) {
    showNotification(paste("Error dalam uji variance:", e$message), type = "error")
    return(NULL)
  })
  
  analysis_results$variance <- result
  return(result)
})

# --- 4. OUTPUT HASIL ---
output$variance_test_result <- renderPrint({
  req(variance_results())
  result <- variance_results()
  
  cat("=== HASIL UJI VARIANCE ===\n\n")
  cat("Uji:", result$test_type, "\n")
  
  if (result$test_type == "Chi-square test for variance") {
    cat("H₀: σ² =", result$hypothesized_var, "\n")
    cat("H₁: σ²", switch(result$alternative, 
                         "two.sided" = "≠", 
                         "greater" = ">", 
                         "less" = "<"), result$hypothesized_var, "\n\n")
    
    cat("Statistik Chi-square:", round(result$statistic, 4), "\n")
    cat("Derajat bebas:", result$df, "\n")
    cat("Sample variance:", round(result$sample_var, 4), "\n")
    cat("Sample size:", result$n, "\n")
    
  } else {
    cat("H₀: σ₁² = σ₂² (variance sama)\n")
    cat("H₁: σ₁²", switch(result$alternative, 
                          "two.sided" = "≠", 
                          "greater" = ">", 
                          "less" = "<"), "σ₂²\n\n")
    
    cat("Statistik F:", round(result$statistic, 4), "\n")
    cat("Derajat bebas:", result$df1, ",", result$df2, "\n")
    cat("Variance", result$group1, ":", round(result$var1, 4), "\n")
    cat("Variance", result$group2, ":", round(result$var2, 4), "\n")
    cat("Sample sizes:", result$n1, ",", result$n2, "\n")
  }
  
  cat("P-value:", round(result$p_value, 6), "\n")
  cat("Confidence interval (", (1-result$alpha)*100, "%):", 
      "[", round(result$conf_int[1], 4), ",", round(result$conf_int[2], 4), "]\n")
})

output$variance_interpretation <- renderText({
  req(variance_results())
  result <- variance_results()
  
  interpretation <- paste0(
    "=== INTERPRETASI HASIL ===\n\n",
    "Tingkat signifikansi: α = ", result$alpha, "\n",
    "P-value: ", round(result$p_value, 6), "\n\n"
  )
  
  if (result$p_value < result$alpha) {
    interpretation <- paste0(interpretation,
                             "KEPUTUSAN: Tolak H₀\n",
                             "KESIMPULAN: Terdapat perbedaan variance yang signifikan secara statistik.\n\n"
    )
  } else {
    interpretation <- paste0(interpretation,
                             "KEPUTUSAN: Gagal tolak H₀\n", 
                             "KESIMPULAN: Tidak terdapat cukup bukti untuk menyatakan adanya perbedaan variance yang signifikan.\n\n"
    )
  }
  
  if (result$test_type == "Chi-square test for variance") {
    interpretation <- paste0(interpretation,
                             "Confidence interval untuk variance populasi: [", 
                             round(result$conf_int[1], 4), ", ", round(result$conf_int[2], 4), "]\n",
                             "Sample variance: ", round(result$sample_var, 4), "\n"
    )
  } else {
    interpretation <- paste0(interpretation,
                             "Confidence interval untuk ratio variance (σ₁²/σ₂²): [", 
                             round(result$conf_int[1], 4), ", ", round(result$conf_int[2], 4), "]\n",
                             "Observed ratio: ", round(result$statistic, 4), "\n"
    )
  }
  
  return(interpretation)
})

# --- 5. STATISTIK DESKRIPTIF ---
output$variance_descriptive_stats <- DT::renderDataTable({
  req(variance_results(), input$variance_var)
  
  data <- processed_data$current
  
  if (input$variance_test_type == "one_sample") {
    values <- data[[input$variance_var]][!is.na(data[[input$variance_var]])]
    stats_df <- data.frame(
      Statistik = c("N", "Mean", "Variance", "Std Dev", "Min", "Max"),
      Nilai = c(
        length(values),
        round(mean(values), 4),
        round(var(values), 4),
        round(sd(values), 4),
        round(min(values), 4),
        round(max(values), 4)
      )
    )
  } else {
    req(input$variance_group_var)
    complete_data <- data[complete.cases(data[c(input$variance_var, input$variance_group_var)]), ]
    groups <- unique(complete_data[[input$variance_group_var]])
    
    group1_data <- complete_data[[input$variance_var]][complete_data[[input$variance_group_var]] == groups[1]]
    group2_data <- complete_data[[input$variance_var]][complete_data[[input$variance_group_var]] == groups[2]]
    
    stats_df <- data.frame(
      Statistik = c("N", "Mean", "Variance", "Std Dev", "Min", "Max"),
      Group1 = c(
        length(group1_data),
        round(mean(group1_data), 4),
        round(var(group1_data), 4),
        round(sd(group1_data), 4),
        round(min(group1_data), 4),
        round(max(group1_data), 4)
      ),
      Group2 = c(
        length(group2_data),
        round(mean(group2_data), 4),
        round(var(group2_data), 4),
        round(sd(group2_data), 4),
        round(min(group2_data), 4),
        round(max(group2_data), 4)
      )
    )
    
    names(stats_df)[2:3] <- paste0(c("Group_", "Group_"), groups)
  }
  
  DT::datatable(stats_df, options = list(dom = 't'), rownames = FALSE)
})

# --- 6. VISUALISASI ---
output$variance_boxplot <- renderPlot({
  req(processed_data$current, input$variance_var)
  
  data <- processed_data$current
  
  if (input$variance_test_type == "one_sample") {
    values <- data[[input$variance_var]][!is.na(data[[input$variance_var]])]
    boxplot(values, main = paste("Boxplot untuk", input$variance_var),
            ylab = input$variance_var, col = "lightblue")
  } else {
    req(input$variance_group_var)
    complete_data <- data[complete.cases(data[c(input$variance_var, input$variance_group_var)]), ]
    
    boxplot(complete_data[[input$variance_var]] ~ complete_data[[input$variance_group_var]],
            main = paste("Boxplot", input$variance_var, "berdasarkan", input$variance_group_var),
            xlab = input$variance_group_var,
            ylab = input$variance_var,
            col = c("lightblue", "lightcoral"))
  }
})

output$variance_histogram <- renderPlot({
  req(processed_data$current, input$variance_var)
  
  data <- processed_data$current
  
  if (input$variance_test_type == "one_sample") {
    values <- data[[input$variance_var]][!is.na(data[[input$variance_var]])]
    hist(values, main = paste("Histogram untuk", input$variance_var),
         xlab = input$variance_var, col = "lightblue", breaks = 20)
  } else {
    req(input$variance_group_var)
    complete_data <- data[complete.cases(data[c(input$variance_var, input$variance_group_var)]), ]
    groups <- unique(complete_data[[input$variance_group_var]])
    
    par(mfrow = c(1, 2))
    for (i in 1:2) {
      group_data <- complete_data[[input$variance_var]][complete_data[[input$variance_group_var]] == groups[i]]
      hist(group_data, 
           main = paste("Histogram", groups[i]),
           xlab = input$variance_var,
           col = c("lightblue", "lightcoral")[i],
           breaks = 15)
    }
    par(mfrow = c(1, 1))
  }
})

# --- 7. DOWNLOAD HASIL ---
output$download_variance_result <- downloadHandler(
  filename = function() {
    paste("hasil-uji-variance-", Sys.Date(), ".", input$variance_format, sep = "")
  },
  content = function(file) {
    req(analysis_results$variance)
    
    result <- analysis_results$variance
    text_content <- capture.output({
      cat("=== HASIL UJI VARIANCE ===\n\n")
      cat("Uji:", result$test_type, "\n")
      
      if (result$test_type == "Chi-square test for variance") {
        cat("Statistik Chi-square:", round(result$statistic, 4), "\n")
        cat("Derajat bebas:", result$df, "\n")
        cat("Sample variance:", round(result$sample_var, 4), "\n")
      } else {
        cat("Statistik F:", round(result$statistic, 4), "\n")
        cat("Derajat bebas:", result$df1, ",", result$df2, "\n")
        cat("Variance", result$group1, ":", round(result$var1, 4), "\n")
        cat("Variance", result$group2, ":", round(result$var2, 4), "\n")
      }
      
      cat("P-value:", round(result$p_value, 6), "\n")
      
      if (result$p_value < result$alpha) {
        cat("Kesimpulan: Tolak H₀ - Terdapat perbedaan variance yang signifikan\n")
      } else {
        cat("Kesimpulan: Gagal tolak H₀ - Tidak ada perbedaan variance yang signifikan\n")
      }
    })
    
    rmarkdown::render(
      input = "text_report.Rmd",
      output_file = file,
      output_format = if(input$variance_format == "pdf") "pdf_document" else "word_document",
      params = list(
        report_title = "Hasil Uji Variance",
        text_output = paste(text_content, collapse = "\n")
      ),
      envir = new.env(parent = globalenv())
    )
  }
)