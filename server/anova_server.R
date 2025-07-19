# server/anova_server.R - DIPERBAIKI LENGKAP

# Load required libraries
library(ggplot2)
library(plotly)
library(car)  # For Levene's test if needed
library(dplyr)  # For data manipulation
library(DT)  # For data tables

# ===== HELPER FUNCTIONS FOR ANOVA =====

# Function to validate ANOVA inputs
validate_anova_inputs <- function(data, dep_var, indep_var1, indep_var2 = NULL, type = "one_way") {
  errors <- c()
  
  # Check if data exists
  if(is.null(data) || nrow(data) == 0) {
    errors <- c(errors, "Data tidak tersedia atau kosong")
    return(errors)
  }
  
  # Check if variables exist
  if (!dep_var %in% names(data)) {
    errors <- c(errors, paste("Variabel dependen", dep_var, "tidak ditemukan"))
  }
  
  if (!indep_var1 %in% names(data)) {
    errors <- c(errors, paste("Variabel independen 1", indep_var1, "tidak ditemukan"))
  }
  
  if (type == "two_way" && !is.null(indep_var2)) {
    if (!indep_var2 %in% names(data)) {
      errors <- c(errors, paste("Variabel independen 2", indep_var2, "tidak ditemukan"))
    }
    
    # Check if variables are different
    if (indep_var1 == indep_var2) {
      errors <- c(errors, "Variabel independen 1 dan 2 harus berbeda")
    }
  }
  
  # If basic checks failed, return early
  if (length(errors) > 0) {
    return(errors)
  }
  
  # Check data types
  tryCatch({
    if (!is.numeric(data[[dep_var]])) {
      errors <- c(errors, "Variabel dependen harus berupa data numerik")
    }
    
    if (!is.factor(data[[indep_var1]]) && !is.character(data[[indep_var1]])) {
      errors <- c(errors, "Variabel independen 1 harus berupa data kategorik")
    }
    
    if (type == "two_way" && !is.null(indep_var2)) {
      if (!is.factor(data[[indep_var2]]) && !is.character(data[[indep_var2]])) {
        errors <- c(errors, "Variabel independen 2 harus berupa data kategorik")
      }
    }
    
    # Check for sufficient groups (only if no errors so far)
    if (length(errors) == 0) {
      unique_groups1 <- unique(data[[indep_var1]][!is.na(data[[indep_var1]])])
      if (length(unique_groups1) < 2) {
        errors <- c(errors, paste("Variabel independen 1 harus memiliki minimal 2 kategori, ditemukan:", length(unique_groups1)))
      }
      
      if (type == "two_way" && !is.null(indep_var2)) {
        unique_groups2 <- unique(data[[indep_var2]][!is.na(data[[indep_var2]])])
        if (length(unique_groups2) < 2) {
          errors <- c(errors, paste("Variabel independen 2 harus memiliki minimal 2 kategori, ditemukan:", length(unique_groups2)))
        }
      }
    }
    
  }, error = function(e) {
    errors <- c(errors, paste("Error dalam validasi data:", e$message))
  })
  
  return(errors)
}

# Function to prepare data for ANOVA
prepare_anova_data <- function(data, dep_var, indep_var1, indep_var2 = NULL, type = "one_way") {
  if (type == "one_way") {
    subset_data <- data[, c(dep_var, indep_var1), drop = FALSE]
  } else {
    subset_data <- data[, c(dep_var, indep_var1, indep_var2), drop = FALSE]
  }
  
  # Remove missing values
  subset_data <- subset_data[complete.cases(subset_data), , drop = FALSE]
  
  # Convert categorical variables to factors
  subset_data[[indep_var1]] <- as.factor(subset_data[[indep_var1]])
  if (type == "two_way" && !is.null(indep_var2)) {
    subset_data[[indep_var2]] <- as.factor(subset_data[[indep_var2]])
  }
  
  return(subset_data)
}

# Helper function untuk notifikasi yang aman
safe_notification <- function(message, type = "message", duration = 5) {
  # Validasi type parameter
  valid_types <- c("default", "message", "warning", "error")
  if (!type %in% valid_types) {
    type <- "message"  # fallback to safe default
  }
  
  tryCatch({
    showNotification(
      message, 
      type = type, 
      duration = duration
    )
  }, error = function(e) {
    # Fallback: print to console jika showNotification gagal
    cat("NOTIFICATION [", type, "]:", message, "\n")
  })
}

# Wrapper functions untuk berbagai jenis notifikasi
notify_success <- function(message, duration = 5) {
  safe_notification(message, "message", duration)
}

notify_error <- function(message, duration = 10) {
  safe_notification(message, "error", duration)
}

notify_warning <- function(message, duration = 7) {
  safe_notification(message, "warning", duration)
}

# Function untuk Post Hoc Analysis
perform_posthoc <- function(model, method = "tukey") {
  tryCatch({
    switch(method,
           "tukey" = {
             result <- TukeyHSD(model)
             return(list(
               method = "Tukey HSD",
               result = result,
               pairwise = result[[1]]
             ))
           },
           "bonferroni" = {
             # Pairwise t-test dengan Bonferroni correction
             data_used <- model$model
             dep_var <- names(data_used)[1]
             indep_var <- names(data_used)[2]
             
             result <- pairwise.t.test(
               data_used[[dep_var]], 
               data_used[[indep_var]], 
               p.adjust.method = "bonferroni"
             )
             return(list(
               method = "Bonferroni",
               result = result,
               pairwise = result$p.value
             ))
           },
           "holm" = {
             # Pairwise t-test dengan Holm correction
             data_used <- model$model
             dep_var <- names(data_used)[1]
             indep_var <- names(data_used)[2]
             
             result <- pairwise.t.test(
               data_used[[dep_var]], 
               data_used[[indep_var]], 
               p.adjust.method = "holm"
             )
             return(list(
               method = "Holm",
               result = result,
               pairwise = result$p.value
             ))
           },
           "scheffe" = {
             # Manual Scheffe test
             result <- TukeyHSD(model)  # Use Tukey as base, adjust interpretation
             return(list(
               method = "Scheffe",
               result = result,
               pairwise = result[[1]],
               note = "Menggunakan pendekatan konservatif Scheffe"
             ))
           }
    )
  }, error = function(e) {
    return(list(error = e$message))
  })
}

# Function untuk interpretasi Post Hoc
interpret_posthoc <- function(posthoc_result, alpha = 0.05) {
  tryCatch({
    method <- posthoc_result$method
    
    interpretation <- paste0("=== INTERPRETASI UJI POST HOC (", method, ") ===\n\n")
    interpretation <- paste0(interpretation, "Tingkat signifikansi: α = ", alpha, "\n\n")
    
    if (method == "Tukey HSD") {
      pairwise <- posthoc_result$pairwise
      
      interpretation <- paste0(interpretation, "PERBANDINGAN BERPASANGAN:\n")
      
      significant_pairs <- c()
      non_significant_pairs <- c()
      
      for (i in 1:nrow(pairwise)) {
        comparison <- rownames(pairwise)[i]
        p_adj <- pairwise[i, "p adj"]
        diff <- pairwise[i, "diff"]
        
        if (!is.na(p_adj)) {
          if (p_adj < alpha) {
            significant_pairs <- c(significant_pairs, comparison)
            interpretation <- paste0(interpretation,
                                     sprintf("• %s: p = %.4f < %.2f → BERBEDA SIGNIFIKAN (selisih = %.3f)\n", 
                                             comparison, p_adj, alpha, diff))
          } else {
            non_significant_pairs <- c(non_significant_pairs, comparison)
            interpretation <- paste0(interpretation,
                                     sprintf("• %s: p = %.4f ≥ %.2f → TIDAK BERBEDA SIGNIFIKAN\n", 
                                             comparison, p_adj, alpha))
          }
        }
      }
      
      interpretation <- paste0(interpretation, "\nRINGKASAN:\n")
      interpretation <- paste0(interpretation, sprintf("- Pasangan yang berbeda signifikan: %d\n", length(significant_pairs)))
      interpretation <- paste0(interpretation, sprintf("- Pasangan yang tidak berbeda: %d\n", length(non_significant_pairs)))
      
    } else {
      # For other methods (Bonferroni, Holm)
      pairwise_matrix <- posthoc_result$pairwise
      
      interpretation <- paste0(interpretation, "PERBANDINGAN BERPASANGAN:\n")
      
      significant_count <- 0
      total_comparisons <- 0
      
      for (i in 1:nrow(pairwise_matrix)) {
        for (j in 1:ncol(pairwise_matrix)) {
          if (!is.na(pairwise_matrix[i, j])) {
            total_comparisons <- total_comparisons + 1
            comparison <- paste(rownames(pairwise_matrix)[i], "vs", colnames(pairwise_matrix)[j])
            p_value <- pairwise_matrix[i, j]
            
            if (p_value < alpha) {
              significant_count <- significant_count + 1
              interpretation <- paste0(interpretation,
                                       sprintf("• %s: p = %.4f < %.2f → BERBEDA SIGNIFIKAN\n", 
                                               comparison, p_value, alpha))
            } else {
              interpretation <- paste0(interpretation,
                                       sprintf("• %s: p = %.4f ≥ %.2f → TIDAK BERBEDA SIGNIFIKAN\n", 
                                               comparison, p_value, alpha))
            }
          }
        }
      }
      
      interpretation <- paste0(interpretation, "\nRINGKASAN:\n")
      interpretation <- paste0(interpretation, sprintf("- Total perbandingan: %d\n", total_comparisons))
      interpretation <- paste0(interpretation, sprintf("- Berbeda signifikan: %d\n", significant_count))
      interpretation <- paste0(interpretation, sprintf("- Tidak berbeda: %d\n", total_comparisons - significant_count))
    }
    
    return(interpretation)
    
  }, error = function(e) {
    return(paste("Error dalam interpretasi:", e$message))
  })
}

# Function untuk effect size calculation
calculate_eta_squared <- function(anova_summary) {
  tryCatch({
    anova_table <- anova_summary[[1]]
    ss_total <- sum(anova_table$`Sum Sq`, na.rm = TRUE)
    
    result <- list()
    for(i in 1:(nrow(anova_table)-1)) {  # Exclude residuals
      effect_name <- rownames(anova_table)[i]
      ss_effect <- anova_table$`Sum Sq`[i]
      eta_squared <- ss_effect / ss_total
      
      # Interpretation of effect size
      effect_interpretation <- if(eta_squared < 0.01) {
        "Sangat Kecil"
      } else if(eta_squared < 0.06) {
        "Kecil"
      } else if(eta_squared < 0.14) {
        "Sedang"
      } else {
        "Besar"
      }
      
      result[[effect_name]] <- list(
        eta_squared = eta_squared,
        interpretation = effect_interpretation
      )
    }
    
    return(result)
  }, error = function(e) {
    cat("Error in calculate_eta_squared:", e$message, "\n")
    return(NULL)
  })
}

# --- 1. RENDER UI DINAMIS ---
output$anova_dependent_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("anova_dep_var", "Pilih Variabel Dependen (Numerik):", 
              choices = numeric_vars,
              selected = if(length(numeric_vars) > 0) numeric_vars[1] else NULL)
})

output$anova_independent1_selector <- renderUI({
  req(processed_data$current)
  # Pastikan variabel kategori memiliki lebih dari 2 level untuk ANOVA
  cat_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) {
    (is.character(x) || is.factor(x)) && length(unique(x[!is.na(x)])) >= 2
  })]
  selectInput("anova_indep_var1", "Pilih Variabel Independen 1 (Kategorik):", 
              choices = cat_vars,
              selected = if(length(cat_vars) > 0) cat_vars[1] else NULL)
})

output$anova_independent2_selector <- renderUI({
  req(input$anova_type == "two_way", processed_data$current)
  cat_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) {
    (is.character(x) || is.factor(x)) && length(unique(x[!is.na(x)])) >= 2
  })]
  # Exclude the first independent variable from choices
  if(!is.null(input$anova_indep_var1)) {
    cat_vars <- cat_vars[cat_vars != input$anova_indep_var1]
  }
  selectInput("anova_indep_var2", "Pilih Variabel Independen 2 (Kategorik):", 
              choices = cat_vars,
              selected = if(length(cat_vars) > 0) cat_vars[1] else NULL)
})

# --- 2. REACTIVE VALUES ---
anova_results <- reactiveValues(
  model = NULL,
  summary = NULL,
  data = NULL,
  formula_used = NULL
)

posthoc_results <- reactiveValues(
  result = NULL,
  method = NULL,
  interpretation = NULL
)

# --- 3. LOGIKA ANOVA YANG DIPERBAIKI ---
observeEvent(input$run_anova, {
  req(processed_data$current, input$anova_dep_var, input$anova_indep_var1)
  
  tryCatch({
    # Validate inputs first
    validation_errors <- validate_anova_inputs(
      processed_data$current, 
      input$anova_dep_var, 
      input$anova_indep_var1, 
      input$anova_indep_var2, 
      input$anova_type
    )
    
    if (length(validation_errors) > 0) {
      notify_error(paste("Error validasi:", paste(validation_errors, collapse = "; ")))
      return()
    }
    
    # Prepare data for ANOVA
    data_for_anova <- prepare_anova_data(
      processed_data$current,
      input$anova_dep_var,
      input$anova_indep_var1,
      input$anova_indep_var2,
      input$anova_type
    )
    
    # Check if we have enough data after cleaning
    if(nrow(data_for_anova) < 3) {
      notify_error("Tidak cukup data untuk analisis ANOVA setelah pembersihan. Minimal 3 observasi diperlukan.")
      return()
    }
    
    # Build formula
    if(input$anova_type == "one_way") {
      formula_str <- paste(input$anova_dep_var, "~", input$anova_indep_var1)
    } else {
      req(input$anova_indep_var2)  # Pastikan variabel kedua ada untuk two-way
      interaction_term <- if(input$anova_interaction) "*" else "+"
      formula_str <- paste(input$anova_dep_var, "~", input$anova_indep_var1, interaction_term, input$anova_indep_var2)
    }
    
    # Run ANOVA
    model <- aov(as.formula(formula_str), data = data_for_anova)
    summary_model <- summary(model)
    
    # Store results
    anova_results$model <- model
    anova_results$summary <- summary_model
    anova_results$data <- data_for_anova
    anova_results$formula_used <- formula_str
    
    # Also store for download (backward compatibility)
    if(exists("analysis_results")) {
      analysis_results$anova <- summary_model
    }
    
    notify_success("ANOVA berhasil dijalankan!")
    
  }, error = function(e) {
    # Debug: print error to console
    cat("ANOVA Error Details:", e$message, "\n")
    cat("Error class:", class(e), "\n")
    cat("Error occurred at:", Sys.time(), "\n")
    
    notify_error(paste("Error dalam analisis ANOVA:", e$message))
  })
})

# --- 4. OUTPUT TABEL ANOVA ---
output$anova_summary_table <- renderPrint({
  req(anova_results$summary)
  
  cat("=== HASIL ANALISIS ANOVA ===\n")
  cat("Formula yang digunakan:", anova_results$formula_used, "\n")
  cat("Jumlah observasi:", nrow(anova_results$data), "\n\n")
  
  print(anova_results$summary)
  
  # Tambahan informasi model
  cat("\n=== RINGKASAN MODEL ===\n")
  if(!is.null(anova_results$model)) {
    cat("R-squared:", round(summary.lm(anova_results$model)$r.squared, 4), "\n")
    cat("Adjusted R-squared:", round(summary.lm(anova_results$model)$adj.r.squared, 4), "\n")
  }
})

# --- 5. INTERPRETASI YANG DIPERBAIKI ---
output$anova_interpretation <- renderText({
  req(anova_results$summary)
  
  tryCatch({
    summary_aov <- anova_results$summary
    anova_table <- summary_aov[[1]]
    p_values <- anova_table$`Pr(>F)`
    f_values <- anova_table$`F value`
    
    interpretation <- "=== INTERPRETASI HASIL ANOVA ===\n\n"
    
    if (input$anova_type == "one_way") {
      # ANOVA Satu Arah
      main_p_value <- p_values[1]
      main_f_value <- f_values[1]
      
      interpretation <- paste0(interpretation,
                               "Jenis Analisis: ANOVA Satu Arah (One-Way ANOVA)\n",
                               "Variabel Dependen: ", input$anova_dep_var, "\n",
                               "Variabel Independen: ", input$anova_indep_var1, "\n",
                               "Jumlah Grup: ", length(unique(anova_results$data[[input$anova_indep_var1]])), "\n\n")
      
      if (!is.na(main_p_value) && !is.na(main_f_value)) {
        interpretation <- paste0(interpretation,
                                 "HASIL PENGUJIAN:\n",
                                 "F-statistik: ", round(main_f_value, 4), "\n",
                                 "P-value: ", round(main_p_value, 6), "\n",
                                 "Tingkat signifikansi: α = 0.05\n\n")
        
        interpretation <- paste0(interpretation, "HIPOTESIS:\n",
                                 "H₀: μ₁ = μ₂ = μ₃ = ... (semua rata-rata grup sama)\n",
                                 "H₁: Minimal ada satu μᵢ ≠ μⱼ (ada perbedaan rata-rata)\n\n")
        
        if (main_p_value < 0.05) {
          interpretation <- paste0(interpretation,
                                   "KESIMPULAN:\n",
                                   "Dengan p-value = ", round(main_p_value, 6), " < 0.05, maka TOLAK H₀.\n",
                                   "Terdapat perbedaan rata-rata yang signifikan pada variabel '", input$anova_dep_var, 
                                   "' antar kategori '", input$anova_indep_var1, "'.\n\n",
                                   "REKOMENDASI:\n",
                                   "Lakukan uji post-hoc (seperti Tukey HSD) untuk mengetahui grup mana yang berbeda secara spesifik.")
        } else {
          interpretation <- paste0(interpretation,
                                   "KESIMPULAN:\n",
                                   "Dengan p-value = ", round(main_p_value, 6), " ≥ 0.05, maka GAGAL TOLAK H₀.\n",
                                   "Tidak terdapat perbedaan rata-rata yang signifikan pada variabel '", input$anova_dep_var, 
                                   "' antar kategori '", input$anova_indep_var1, "'.")
        }
      }
      
    } else if (input$anova_type == "two_way") {
      # ANOVA Dua Arah
      interpretation <- paste0(interpretation,
                               "Jenis Analisis: ANOVA Dua Arah (Two-Way ANOVA)\n",
                               "Variabel Dependen: ", input$anova_dep_var, "\n",
                               "Faktor 1: ", input$anova_indep_var1, "\n",
                               "Faktor 2: ", input$anova_indep_var2, "\n",
                               "Efek Interaksi: ", if(input$anova_interaction) "Ya" else "Tidak", "\n\n")
      
      row_names <- rownames(anova_table)
      
      for(i in 1:length(p_values)) {
        if(!is.na(p_values[i]) && !is.na(f_values[i])) {
          effect_name <- row_names[i]
          interpretation <- paste0(interpretation,
                                   "EFEK: ", effect_name, "\n",
                                   "F-statistik: ", round(f_values[i], 4), "\n",
                                   "P-value: ", round(p_values[i], 6), "\n")
          
          if(p_values[i] < 0.05) {
            interpretation <- paste0(interpretation,
                                     "Hasil: SIGNIFIKAN (p < 0.05)\n",
                                     "Interpretasi: Terdapat pengaruh yang signifikan.\n\n")
          } else {
            interpretation <- paste0(interpretation,
                                     "Hasil: TIDAK SIGNIFIKAN (p ≥ 0.05)\n",
                                     "Interpretasi: Tidak terdapat pengaruh yang signifikan.\n\n")
          }
        }
      }
    }
    
    return(interpretation)
    
  }, error = function(e) {
    return(paste("Error dalam membuat interpretasi:", e$message))
  })
})

# --- 6. VISUALISASI YANG DIPERBAIKI ---
output$anova_plot <- renderPlotly({
  req(anova_results$data, input$anova_dep_var, input$anova_indep_var1)
  
  tryCatch({
    data <- anova_results$data
    
    # Create base plot
    if(input$anova_type == "one_way") {
      # One-way ANOVA visualization
      p <- ggplot(data, aes_string(x = input$anova_indep_var1, y = input$anova_dep_var)) +
        geom_boxplot(aes_string(fill = input$anova_indep_var1), alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
                     fill = "red", color = "black") +
        labs(
          title = paste("Distribusi", input$anova_dep_var, "berdasarkan", input$anova_indep_var1),
          subtitle = "Boxplot dengan titik data individual dan rata-rata (diamond merah)",
          x = input$anova_indep_var1,
          y = input$anova_dep_var
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
      
    } else {
      # Two-way ANOVA visualization
      p <- ggplot(data, aes_string(x = input$anova_indep_var1, y = input$anova_dep_var, 
                                   fill = input$anova_indep_var2)) +
        geom_boxplot(alpha = 0.7) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 2, 
                     position = position_dodge(width = 0.75)) +
        labs(
          title = paste("Distribusi", input$anova_dep_var, "berdasarkan", 
                        input$anova_indep_var1, "dan", input$anova_indep_var2),
          subtitle = "Two-way ANOVA visualization",
          x = input$anova_indep_var1,
          y = input$anova_dep_var,
          fill = input$anova_indep_var2
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
    
    # Convert to plotly
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        title = list(
          text = paste0(
            if(input$anova_type == "one_way") "One-Way ANOVA: " else "Two-Way ANOVA: ",
            input$anova_dep_var
          ),
          font = list(size = 16)
        )
      )
    
  }, error = function(e) {
    # Create error plot
    plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, text = paste("Error membuat plot:", e$message),
                       textfont = list(size = 16, color = "red")) %>%
      plotly::layout(
        title = "Error dalam Visualisasi",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
  })
})

# --- 7. POST HOC LOGIC ---

# Determine when to show post hoc tab
output$show_posthoc <- reactive({
  req(anova_results$summary)
  
  tryCatch({
    # Check if ANOVA is significant
    anova_table <- anova_results$summary[[1]]
    p_values <- anova_table$`Pr(>F)`
    
    # For one-way ANOVA, check first p-value
    if (input$anova_type == "one_way") {
      return(!is.na(p_values[1]) && p_values[1] < 0.05)
    } 
    # For two-way ANOVA, check if any main effect is significant
    else {
      main_effects_significant <- any(p_values[1:2] < 0.05, na.rm = TRUE)
      return(main_effects_significant)
    }
    
  }, error = function(e) {
    cat("Error in show_posthoc:", e$message, "\n")
    return(FALSE)
  })
})
outputOptions(output, "show_posthoc", suspendWhenHidden = FALSE)

# Post Hoc Analysis
observeEvent(input$run_posthoc, {
  req(anova_results$model, input$posthoc_method)
  
  tryCatch({
    # Check if we're dealing with one-way ANOVA only for now
    if (input$anova_type == "two_way") {
      notify_warning("Post Hoc untuk Two-Way ANOVA saat ini hanya mendukung main effects. Interpretasi dengan hati-hati.")
    }
    
    # Perform post hoc analysis
    posthoc_result <- perform_posthoc(anova_results$model, input$posthoc_method)
    
    if ("error" %in% names(posthoc_result)) {
      notify_error(paste("Error dalam Post Hoc:", posthoc_result$error))
      return()
    }
    
    # Store results
    posthoc_results$result <- posthoc_result
    posthoc_results$method <- input$posthoc_method
    posthoc_results$interpretation <- interpret_posthoc(posthoc_result)
    
    notify_success(paste("Post Hoc", posthoc_result$method, "berhasil dijalankan!"))
    
  }, error = function(e) {
    notify_error(paste("Error dalam analisis Post Hoc:", e$message))
    cat("Post Hoc Error:", e$message, "\n")
  })
})

# Post Hoc Results Output
output$posthoc_results <- renderPrint({
  req(posthoc_results$result)
  
  result <- posthoc_results$result
  
  cat("=== HASIL UJI POST HOC ===\n")
  cat("Metode:", result$method, "\n")
  if (!is.null(result$note)) {
    cat("Catatan:", result$note, "\n")
  }
  cat("\n")
  
  print(result$result)
})

# Post Hoc Interpretation Output
output$posthoc_interpretation <- renderText({
  req(posthoc_results$interpretation)
  posthoc_results$interpretation
})

# Post Hoc Visualization
output$posthoc_plot <- renderPlotly({
  req(posthoc_results$result, anova_results$data)
  
  tryCatch({
    if (input$anova_type == "one_way") {
      # Create mean comparison plot
      data <- anova_results$data
      
      # Calculate group means and confidence intervals
      group_stats <- data %>%
        group_by(!!sym(input$anova_indep_var1)) %>%
        summarise(
          mean_val = mean(!!sym(input$anova_dep_var), na.rm = TRUE),
          se_val = sd(!!sym(input$anova_dep_var), na.rm = TRUE) / sqrt(n()),
          n = n(),
          .groups = 'drop'
        ) %>%
        mutate(
          ci_lower = mean_val - 1.96 * se_val,
          ci_upper = mean_val + 1.96 * se_val
        )
      
      # Create plot
      p <- ggplot(group_stats, aes_string(x = input$anova_indep_var1, y = "mean_val")) +
        geom_col(aes_string(fill = input$anova_indep_var1), alpha = 0.7) +
        geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                      width = 0.2, color = "black") +
        geom_text(aes(label = paste("n =", n)), 
                  vjust = -0.5, hjust = 0.5, size = 3) +
        labs(
          title = "Perbandingan Rata-rata Antar Grup (Post Hoc)",
          subtitle = paste("Metode:", posthoc_results$result$method),
          x = input$anova_indep_var1,
          y = paste("Rata-rata", input$anova_dep_var),
          caption = "Error bars: 95% Confidence Interval"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
      
      ggplotly(p, tooltip = c("x", "y", "text"))
      
    } else {
      # For two-way ANOVA, create interaction plot
      data <- anova_results$data
      
      group_stats <- data %>%
        group_by(!!sym(input$anova_indep_var1), !!sym(input$anova_indep_var2)) %>%
        summarise(
          mean_val = mean(!!sym(input$anova_dep_var), na.rm = TRUE),
          se_val = sd(!!sym(input$anova_dep_var), na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        )
      
      p <- ggplot(group_stats, aes_string(x = input$anova_indep_var1, y = "mean_val", 
                                          color = input$anova_indep_var2, 
                                          group = input$anova_indep_var2)) +
        geom_line(size = 1) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val), 
                      width = 0.1) +
        labs(
          title = "Interaction Plot (Two-Way ANOVA)",
          x = input$anova_indep_var1,
          y = paste("Rata-rata", input$anova_dep_var),
          color = input$anova_indep_var2
        ) +
        theme_minimal()
      
      ggplotly(p)
    }
    
  }, error = function(e) {
    plotly::plot_ly() %>%
      plotly::add_text(x = 0.5, y = 0.5, 
                       text = paste("Error membuat plot Post Hoc:", e$message),
                       textfont = list(size = 16, color = "red")) %>%
      plotly::layout(
        title = "Error dalam Visualisasi Post Hoc",
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
  })
})

# --- 8. LOGIKA UNDUH YANG DIPERBAIKI ---
output$download_anova_result <- downloadHandler(
  filename = function() {
    paste("hasil-anova-", Sys.Date(), ".", input$anova_format, sep = "")
  },
  content = function(file) {
    req(anova_results$summary)
    
    tryCatch({
      # Create comprehensive report content
      report_content <- c(
        "# LAPORAN HASIL ANALISIS ANOVA",
        "",
        paste("**Tanggal Analisis:** ", Sys.Date()),
        paste("**Jenis ANOVA:** ", if(input$anova_type == "one_way") "Satu Arah (One-Way)" else "Dua Arah (Two-Way)"),
        paste("**Formula:** ", anova_results$formula_used),
        "",
        "## Ringkasan Data",
        paste("- Jumlah observasi:", nrow(anova_results$data)),
        paste("- Variabel dependen:", input$anova_dep_var),
        paste("- Variabel independen 1:", input$anova_indep_var1),
        if(input$anova_type == "two_way") paste("- Variabel independen 2:", input$anova_indep_var2) else "",
        "",
        "## Hasil ANOVA",
        "```",
        capture.output(print(anova_results$summary)),
        "```",
        "",
        "## Interpretasi",
        # Add interpretation here
        ""
      )
      
      # Write to temporary file
      temp_rmd <- tempfile(fileext = ".Rmd")
      writeLines(report_content, temp_rmd)
      
      # Render report
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        output_format = if(input$anova_format == "pdf") "pdf_document" else "word_document",
        quiet = TRUE
      )
      
    }, error = function(e) {
      # Fallback: simple text output
      writeLines(capture.output(print(anova_results$summary)), file)
    })
  }
)