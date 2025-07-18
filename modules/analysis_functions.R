# Load required libraries dengan error handling
required_packages <- c("car", "nortest", "stats")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Warning: Package", pkg, "not available. Some functions may use fallbacks.\n")
  }
}

# Fungsi untuk statistik deskriptif
calculate_descriptive_stats <- function(data, variables = NULL) {
  if (is.null(variables)) {
    numeric_vars <- sapply(data, is.numeric)
    variables <- names(data)[numeric_vars]
  }
  
  results <- list()
  
  for (var in variables) {
    if (var %in% names(data) && is.numeric(data[[var]])) {
      values <- data[[var]][!is.na(data[[var]])]
      
      if (length(values) > 0) {
        results[[var]] <- list(
          n = length(values),
          mean = mean(values),
          median = median(values),
          sd = sd(values),
          var = var(values),
          min = min(values),
          max = max(values),
          q1 = quantile(values, 0.25),
          q3 = quantile(values, 0.75),
          iqr = IQR(values),
          skewness = calculate_skewness(values),
          kurtosis = calculate_kurtosis(values)
        )
      } else {
        results[[var]] <- list(
          n = 0, mean = NA, median = NA, sd = NA, var = NA,
          min = NA, max = NA, q1 = NA, q3 = NA, iqr = NA,
          skewness = NA, kurtosis = NA
        )
      }
    }
  }
  
  return(results)
}

# Fungsi untuk menghitung skewness
calculate_skewness <- function(x) {
  if (length(x) < 3) return(NA)
  
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  if (sd_x == 0) return(NA)
  
  skew <- sum((x - mean_x)^3) / (n * sd_x^3)
  return(skew)
}

# Fungsi untuk menghitung kurtosis
calculate_kurtosis <- function(x) {
  if (length(x) < 4) return(NA)
  
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  if (sd_x == 0) return(NA)
  
  kurt <- sum((x - mean_x)^4) / (n * sd_x^4) - 3
  return(kurt)
}

# Fungsi untuk uji normalitas
test_normality <- function(data, variable, method = "shapiro") {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  values <- data[[variable]][!is.na(data[[variable]])]
  
  if (length(values) < 3) {
    return(list(
      test = method,
      statistic = NA,
      p_value = NA,
      interpretation = "Data tidak cukup untuk uji normalitas (minimal 3 observasi)"
    ))
  }
  
  result <- tryCatch({
    switch(method,
           "shapiro" = {
             if (length(values) > 5000) {
               # Untuk data besar, gunakan sampel
               sample_values <- sample(values, 5000)
               test_result <- shapiro.test(sample_values)
             } else {
               test_result <- shapiro.test(values)
             }
             list(
               test = "Shapiro-Wilk",
               statistic = test_result$statistic,
               p_value = test_result$p.value
             )
           },
           "anderson" = {
             if (requireNamespace("nortest", quietly = TRUE)) {
               test_result <- nortest::ad.test(values)
               list(
                 test = "Anderson-Darling",
                 statistic = test_result$statistic,
                 p_value = test_result$p.value
               )
             } else {
               stop("Package 'nortest' tidak tersedia")
             }
           },
           "kolmogorov" = {
             test_result <- ks.test(values, "pnorm", mean(values), sd(values))
             list(
               test = "Kolmogorov-Smirnov",
               statistic = test_result$statistic,
               p_value = test_result$p.value
             )
           }
    )
  }, error = function(e) {
    return(list(
      test = method,
      statistic = NA,
      p_value = NA,
      interpretation = paste("Error dalam uji normalitas:", e$message)
    ))
  })
  
  # Interpretasi
  if (!is.na(result$p_value)) {
    alpha <- 0.05
    if (result$p_value > alpha) {
      interpretation <- paste0("Data berdistribusi normal (p-value = ", 
                               round(result$p_value, 4), " > ", alpha, ")")
    } else {
      interpretation <- paste0("Data tidak berdistribusi normal (p-value = ", 
                               round(result$p_value, 4), " ≤ ", alpha, ")")
    }
    result$interpretation <- interpretation
  }
  
  return(result)
}

# FUNGSI UJI HOMOGENITAS YANG DIPERBAIKI
test_homogeneity <- function(data, variable, group_var, method = "levene") {
  if (!variable %in% names(data) || !group_var %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  # Hapus missing values dengan lebih hati-hati
  complete_cases_idx <- complete.cases(data[c(variable, group_var)])
  complete_data <- data[complete_cases_idx, ]
  
  # Validasi data setelah cleaning
  if (nrow(complete_data) < 3) {
    return(list(
      test = method,
      statistic = NA,
      p_value = NA,
      interpretation = "Data tidak cukup untuk uji homogenitas (minimal 3 observasi)"
    ))
  }
  
  # Pastikan variabel grup memiliki minimal 2 level
  group_levels <- unique(complete_data[[group_var]])
  group_levels <- group_levels[!is.na(group_levels)]
  
  if (length(group_levels) < 2) {
    return(list(
      test = method,
      statistic = NA,
      p_value = NA,
      interpretation = "Variabel grup harus memiliki minimal 2 kategori yang berbeda"
    ))
  }
  
  # Pastikan setiap grup memiliki minimal 2 observasi
  group_counts <- table(complete_data[[group_var]])
  if (any(group_counts < 2)) {
    return(list(
      test = method,
      statistic = NA,
      p_value = NA,
      interpretation = "Setiap grup harus memiliki minimal 2 observasi"
    ))
  }
  
  # Konversi ke factor jika belum
  complete_data[[group_var]] <- as.factor(complete_data[[group_var]])
  
  # Pastikan variabel numerik benar-benar numerik
  complete_data[[variable]] <- as.numeric(complete_data[[variable]])
  
  # Hapus lagi jika ada NA yang muncul setelah konversi
  final_complete_idx <- complete.cases(complete_data[c(variable, group_var)])
  final_data <- complete_data[final_complete_idx, ]
  
  if (nrow(final_data) < 3) {
    return(list(
      test = method,
      statistic = NA,
      p_value = NA,
      interpretation = "Data tidak cukup setelah pembersihan untuk uji homogenitas"
    ))
  }
  
  # Buat formula
  formula_str <- paste(variable, "~", group_var)
  formula_obj <- as.formula(formula_str)
  
  # Tambahkan error handling yang lebih spesifik
  result <- tryCatch({
    switch(method,
           "levene" = {
             if (requireNamespace("car", quietly = TRUE)) {
               test_result <- car::leveneTest(formula_obj, data = final_data, center = median)
               list(
                 test = "Levene Test",
                 statistic = test_result$`F value`[1],
                 p_value = test_result$`Pr(>F)`[1],
                 df1 = test_result$Df[1],
                 df2 = test_result$Df[2]
               )
             } else {
               stop("Package 'car' tidak tersedia")
             }
           },
           "bartlett" = {
             test_result <- bartlett.test(formula_obj, data = final_data)
             list(
               test = "Bartlett Test",
               statistic = test_result$statistic,
               p_value = test_result$p.value,
               df = test_result$parameter
             )
           }
    )
  }, error = function(e) {
    return(list(
      test = method,
      statistic = NA,
      p_value = NA,
      interpretation = paste("Error dalam perhitungan:", e$message)
    ))
  })
  
  # Validasi hasil sebelum interpretasi
  if (is.na(result$p_value) || is.null(result$p_value)) {
    result$interpretation <- "Uji gagal dilakukan. Periksa data dan variabel yang dipilih."
    return(result)
  }
  
  # Interpretasi
  alpha <- 0.05
  if (result$p_value > alpha) {
    interpretation <- paste0("Variansi homogen (p-value = ", 
                             round(result$p_value, 4), " > ", alpha, ")")
  } else {
    interpretation <- paste0("Variansi tidak homogen (p-value = ", 
                             round(result$p_value, 4), " ≤ ", alpha, ")")
  }
  
  result$interpretation <- interpretation
  result$n_total <- nrow(final_data)
  result$n_groups <- length(unique(final_data[[group_var]]))
  
  return(result)
}

# Fungsi untuk t-test
perform_ttest <- function(data, variable, group_var = NULL, mu = 0, alternative = "two.sided") {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  if (is.null(group_var)) {
    # One sample t-test
    values <- data[[variable]][!is.na(data[[variable]])]
    
    if (length(values) < 2) {
      stop("Data tidak cukup untuk uji t (minimal 2 observasi)")
    }
    
    test_result <- t.test(values, mu = mu, alternative = alternative)
    
    interpretation <- paste0(
      "Uji t satu sampel untuk variabel ", variable, ":\n",
      "H0: μ = ", mu, "\n",
      "H1: μ ", switch(alternative, 
                       "two.sided" = "≠", 
                       "greater" = ">", 
                       "less" = "<"), " ", mu, "\n",
      "t = ", round(test_result$statistic, 4), "\n",
      "df = ", test_result$parameter, "\n",
      "p-value = ", round(test_result$p.value, 4), "\n",
      if (test_result$p.value < 0.05) {
        "Kesimpulan: Tolak H0, terdapat perbedaan signifikan"
      } else {
        "Kesimpulan: Gagal tolak H0, tidak terdapat perbedaan signifikan"
      }
    )
  } else {
    # Two sample t-test
    if (!group_var %in% names(data)) {
      stop("Variabel grup tidak ditemukan dalam data")
    }
    
    complete_data <- data[complete.cases(data[c(variable, group_var)]), ]
    groups <- unique(complete_data[[group_var]])
    
    if (length(groups) != 2) {
      stop("Variabel grup harus memiliki tepat 2 kategori")
    }
    
    group1 <- complete_data[[variable]][complete_data[[group_var]] == groups[1]]
    group2 <- complete_data[[variable]][complete_data[[group_var]] == groups[2]]
    
    if (length(group1) < 2 || length(group2) < 2) {
      stop("Setiap grup harus memiliki minimal 2 observasi")
    }
    
    test_result <- t.test(group1, group2, alternative = alternative)
    
    interpretation <- paste0(
      "Uji t dua sampel untuk variabel ", variable, " berdasarkan ", group_var, ":\n",
      "Grup 1 (", groups[1], "): n = ", length(group1), ", mean = ", round(mean(group1), 4), "\n",
      "Grup 2 (", groups[2], "): n = ", length(group2), ", mean = ", round(mean(group2), 4), "\n",
      "t = ", round(test_result$statistic, 4), "\n",
      "df = ", round(test_result$parameter, 2), "\n",
      "p-value = ", round(test_result$p.value, 4), "\n",
      if (test_result$p.value < 0.05) {
        "Kesimpulan: Tolak H0, terdapat perbedaan signifikan antara kedua grup"
      } else {
        "Kesimpulan: Gagal tolak H0, tidak terdapat perbedaan signifikan antara kedua grup"
      }
    )
  }
  
  return(list(
    test_result = test_result,
    interpretation = interpretation
  ))
}

# Fungsi untuk ANOVA
perform_anova <- function(data, dependent_var, independent_vars, type = "one_way") {
  if (!dependent_var %in% names(data)) {
    stop("Variabel dependen tidak ditemukan dalam data")
  }
  
  if (type == "one_way") {
    if (length(independent_vars) != 1 || !independent_vars[1] %in% names(data)) {
      stop("Untuk ANOVA satu arah, diperlukan satu variabel independen")
    }
    
    formula_str <- paste(dependent_var, "~", independent_vars[1])
    formula_obj <- as.formula(formula_str)
    
    complete_data <- data[complete.cases(data[c(dependent_var, independent_vars)]), ]
    
    if (nrow(complete_data) < 3) {
      stop("Data tidak cukup untuk ANOVA (minimal 3 observasi)")
    }
    
    # Check if group variable has enough levels
    group_levels <- unique(complete_data[[independent_vars[1]]])
    if (length(group_levels) < 2) {
      stop("Variabel grup harus memiliki minimal 2 level")
    }
    
    anova_result <- aov(formula_obj, data = complete_data)
    summary_result <- summary(anova_result)
    
    interpretation <- paste0(
      "ANOVA Satu Arah:\n",
      "Variabel Dependen: ", dependent_var, "\n",
      "Variabel Independen: ", independent_vars[1], "\n",
      "F = ", round(summary_result[[1]]$`F value`[1], 4), "\n",
      "df = ", summary_result[[1]]$Df[1], ", ", summary_result[[1]]$Df[2], "\n",
      "p-value = ", round(summary_result[[1]]$`Pr(>F)`[1], 4), "\n",
      if (summary_result[[1]]$`Pr(>F)`[1] < 0.05) {
        "Kesimpulan: Tolak H0, terdapat perbedaan signifikan antar grup"
      } else {
        "Kesimpulan: Gagal tolak H0, tidak terdapat perbedaan signifikan antar grup"
      }
    )
    
  } else if (type == "two_way") {
    if (length(independent_vars) != 2 || !all(independent_vars %in% names(data))) {
      stop("Untuk ANOVA dua arah, diperlukan dua variabel independen")
    }
    
    formula_str <- paste(dependent_var, "~", independent_vars[1], "*", independent_vars[2])
    formula_obj <- as.formula(formula_str)
    
    complete_data <- data[complete.cases(data[c(dependent_var, independent_vars)]), ]
    
    if (nrow(complete_data) < 6) {
      stop("Data tidak cukup untuk ANOVA dua arah (minimal 6 observasi)")
    }
    
    anova_result <- aov(formula_obj, data = complete_data)
    summary_result <- summary(anova_result)
    
    interpretation <- paste0(
      "ANOVA Dua Arah:\n",
      "Variabel Dependen: ", dependent_var, "\n",
      "Variabel Independen: ", paste(independent_vars, collapse = ", "), "\n",
      "Hasil uji F untuk setiap efek:\n"
    )
    
    for (i in 1:nrow(summary_result[[1]])) {
      if (!is.na(summary_result[[1]]$`F value`[i])) {
        interpretation <- paste0(interpretation,
                                 rownames(summary_result[[1]])[i], ": F = ", 
                                 round(summary_result[[1]]$`F value`[i], 4),
                                 ", p-value = ", round(summary_result[[1]]$`Pr(>F)`[i], 4), "\n"
        )
      }
    }
  }
  
  return(list(
    anova_result = anova_result,
    summary_result = summary_result,
    interpretation = interpretation
  ))
}

# Fungsi transformasi data
transform_data <- function(data, variable, method = "log") {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  values <- data[[variable]]
  na_mask <- is.na(values)
  
  tryCatch({
    transformed <- switch(method,
                          "log" = {
                            if (any(values <= 0, na.rm = TRUE)) {
                              log(values + 1)  # log(x + 1) transformation
                            } else {
                              log(values)
                            }
                          },
                          "sqrt" = {
                            if (any(values < 0, na.rm = TRUE)) {
                              stop("Transformasi akar kuadrat tidak dapat diterapkan pada nilai negatif")
                            }
                            sqrt(values)
                          },
                          "square" = values^2,
                          "standardize" = {
                            mean_val <- mean(values, na.rm = TRUE)
                            sd_val <- sd(values, na.rm = TRUE)
                            if (sd_val == 0) {
                              stop("Tidak dapat standardisasi: standar deviasi = 0")
                            }
                            (values - mean_val) / sd_val
                          },
                          "normalize" = {
                            min_val <- min(values, na.rm = TRUE)
                            max_val <- max(values, na.rm = TRUE)
                            if (min_val == max_val) {
                              stop("Tidak dapat normalisasi: semua nilai sama")
                            }
                            (values - min_val) / (max_val - min_val)
                          },
                          values
    )
    
    transformed[na_mask] <- NA
    return(transformed)
    
  }, error = function(e) {
    stop(paste("Error dalam transformasi:", e$message))
  })
}

# Fungsi kategorisasi
categorize_variable <- function(data, variable, method = "quantile", breaks = 3) {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  values <- data[[variable]]
  clean_values <- values[!is.na(values)]
  
  if (length(clean_values) == 0) {
    stop("Tidak ada data valid untuk dikategorisasi")
  }
  
  if (length(unique(clean_values)) < breaks) {
    stop(paste("Jumlah nilai unik kurang dari jumlah kategori yang diminta"))
  }
  
  tryCatch({
    if (method == "quantile") {
      quantiles <- quantile(clean_values, probs = seq(0, 1, length.out = breaks + 1), na.rm = TRUE)
      # Check for duplicate quantiles
      if (length(unique(quantiles)) < length(quantiles)) {
        method <- "equal"  # fallback to equal intervals
      } else {
        categories <- cut(values, breaks = quantiles, include.lowest = TRUE, 
                          labels = paste0("Q", 1:breaks))
        return(categories)
      }
    }
    
    if (method == "equal") {
      min_val <- min(clean_values)
      max_val <- max(clean_values)
      if (min_val == max_val) {
        stop("Variabel memiliki nilai konstan")
      }
      range_extend <- (max_val - min_val) * 0.001
      breaks_seq <- seq(min_val - range_extend, max_val + range_extend, length.out = breaks + 1)
      categories <- cut(values, breaks = breaks_seq, include.lowest = TRUE,
                        labels = paste0("Cat", 1:breaks))
      return(categories)
    }
    
  }, error = function(e) {
    stop(paste("Error dalam kategorisasi:", e$message))
  })
}

# Fungsi untuk mendapatkan variabel kategori yang valid
get_valid_categorical_variables <- function(data, min_levels = 2, max_levels = 20, min_obs_per_level = 2) {
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }
  
  valid_vars <- character(0)
  
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    
    if (!is.character(col_data) && !is.factor(col_data)) {
      next
    }
    
    clean_data <- col_data[!is.na(col_data)]
    
    if (length(clean_data) == 0) {
      next
    }
    
    unique_levels <- unique(clean_data)
    n_levels <- length(unique_levels)
    
    if (n_levels < min_levels || n_levels > max_levels) {
      next
    }
    
    level_counts <- table(clean_data)
    if (any(level_counts < min_obs_per_level)) {
      next
    }
    
    valid_vars <- c(valid_vars, col_name)
  }
  
  return(valid_vars)
}

# Fungsi helper untuk validasi data
validate_data_for_analysis <- function(data, variables, min_obs = 3) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(valid = FALSE, message = "Data kosong"))
  }
  
  missing_vars <- variables[!variables %in% names(data)]
  if (length(missing_vars) > 0) {
    return(list(valid = FALSE, message = paste("Variabel tidak ditemukan:", paste(missing_vars, collapse = ", "))))
  }
  
  complete_cases <- complete.cases(data[variables])
  n_complete <- sum(complete_cases)
  
  if (n_complete < min_obs) {
    return(list(valid = FALSE, message = paste("Data tidak cukup. Diperlukan minimal", min_obs, "observasi lengkap, tersedia", n_complete)))
  }
  
  return(list(valid = TRUE, message = "Data valid", n_complete = n_complete))
}

# Fungsi untuk membersihkan data
clean_data_for_analysis <- function(data, variables) {
  complete_cases <- complete.cases(data[variables])
  cleaned_data <- data[complete_cases, ]
  
  return(list(
    data = cleaned_data,
    n_original = nrow(data),
    n_cleaned = nrow(cleaned_data),
    n_removed = nrow(data) - nrow(cleaned_data)
  ))
}

cat("✓ Analysis functions loaded successfully\n")