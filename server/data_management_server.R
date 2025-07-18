# Inisialisasi reactive values yang stabil
processed_data <- reactiveValues(
  original = NULL,
  current = NULL,
  operations = list(),
  outliers = integer(0),
  operation_log = character(0)
)

# Observer untuk inisialisasi data - DIPERBAIKI
observe({
  data <- sovi_data()
  if (!is.null(data)) {
    cat("Initializing processed data...\n")
    processed_data$original <- data
    processed_data$current <- data
    processed_data$operation_log <- c("Data dimuat: 511 observasi, 24 variabel")
  }
})

# Update pilihan variabel - DIPERBAIKI dengan error handling
observe({
  data <- processed_data$current
  if (!is.null(data) && ncol(data) > 0) {
    tryCatch({
      # Variabel numerik
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      all_vars <- names(data)
      
      # Update choices dengan error handling
      if (length(numeric_vars) > 0) {
        updateSelectInput(session, "transform_var", choices = numeric_vars)
        updateSelectInput(session, "categorize_var", choices = numeric_vars)
        updateSelectInput(session, "outlier_var", choices = numeric_vars)
        updateSelectInput(session, "viz_var", choices = numeric_vars)
      }
      
      if (length(all_vars) > 0) {
        updateSelectInput(session, "filter_var", choices = all_vars)
      }
    }, error = function(e) {
      cat("Error updating choices:", e$message, "\n")
    })
  }
})

detect_outliers <- function(x, method = "iqr", iqr_mult = 1.5, z_thresh = 3) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 3) return(integer(0))
  
  if (method == "iqr") {
    Q1 <- quantile(x_clean, 0.25)
    Q3 <- quantile(x_clean, 0.75)
    IQR_val <- Q3 - Q1
    lower <- Q1 - iqr_mult * IQR_val
    upper <- Q3 + iqr_mult * IQR_val
    return(which(x < lower | x > upper))
  } else if (method == "zscore") {
    z_scores <- abs((x - mean(x_clean)) / sd(x_clean))
    return(which(z_scores > z_thresh))
  } else if (method == "modified_zscore") {
    median_x <- median(x_clean)
    mad_x <- mad(x_clean)
    if (mad_x == 0) return(integer(0))
    modified_z <- 0.6745 * (x - median_x) / mad_x
    return(which(abs(modified_z) > 3.5))
  }
  return(integer(0))
}

impute_values <- function(x, outlier_indices, method = "median") {
  x_imputed <- x
  if (length(outlier_indices) > 0) {
    valid_values <- x[-outlier_indices]
    valid_values <- valid_values[!is.na(valid_values)]
    
    if (length(valid_values) == 0) return(x)
    
    replacement <- switch(method,
                          "mean" = mean(valid_values),
                          "median" = median(valid_values),
                          "mode" = {
                            tbl <- table(valid_values)
                            as.numeric(names(tbl)[which.max(tbl)])
                          },
                          median(valid_values)
    )
    
    x_imputed[outlier_indices] <- replacement
  }
  return(x_imputed)
}

output$data_info_summary <- renderText({
  data <- processed_data$current
  if (!is.null(data)) {
    n_obs <- nrow(data)
    n_vars <- ncol(data)
    n_numeric <- sum(sapply(data, is.numeric))
    n_categorical <- sum(sapply(data, function(x) is.character(x) || is.factor(x)))
    missing_pct <- round(100 * sum(is.na(data)) / (n_obs * n_vars), 2)
    
    paste(
      "=== STATUS DATA ===",
      paste("Observasi:", format(n_obs, big.mark = ",")),
      paste("Variabel:", n_vars, "(", n_numeric, "numerik,", n_categorical, "kategori)"),
      paste("Missing values:", missing_pct, "%"),
      paste("Operasi diterapkan:", length(processed_data$operations)),
      sep = "\n"
    )
  } else {
    "Data belum tersedia"
  }
})

# Value boxes - DIPERBAIKI
output$vbox_rows <- renderValueBox({
  valueBox(
    value = if (!is.null(processed_data$current)) format(nrow(processed_data$current), big.mark = ",") else "0",
    subtitle = "Baris Data",
    icon = icon("table"),
    color = "blue"
  )
})

output$vbox_cols <- renderValueBox({
  valueBox(
    value = if (!is.null(processed_data$current)) ncol(processed_data$current) else "0",
    subtitle = "Kolom Data", 
    icon = icon("columns"),
    color = "green"
  )
})

output$vbox_missing <- renderValueBox({
  valueBox(
    value = if (!is.null(processed_data$current)) {
      paste0(round(100 * sum(is.na(processed_data$current)) / (nrow(processed_data$current) * ncol(processed_data$current)), 1), "%")
    } else "0%",
    subtitle = "Missing Values",
    icon = icon("exclamation-triangle"),
    color = "yellow"
  )
})

output$vbox_outliers <- renderValueBox({
  valueBox(
    value = if (!is.null(processed_data$outliers)) length(processed_data$outliers) else 0,
    subtitle = "Outliers Terdeteksi",
    icon = icon("search"),
    color = "red"
  )
})

# Tabel preview data
output$data_preview_table <- DT::renderDataTable({
  data <- processed_data$current
  if (!is.null(data)) {
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'frtip'
      ),
      rownames = FALSE
    )
  }
})

observeEvent(input$btn_transform, {
  req(processed_data$current, input$transform_var, input$transform_method)
  
  tryCatch({
    data <- processed_data$current
    var_name <- input$transform_var
    method <- input$transform_method
    suffix <- if (!is.null(input$transform_suffix) && input$transform_suffix != "") {
      input$transform_suffix
    } else {
      method
    }
    
    values <- data[[var_name]]
    
    # Validasi data
    if (all(is.na(values))) {
      stop("Variabel tidak memiliki data valid")
    }
    
    # Transformasi dengan validasi
    transformed <- switch(method,
                          "log" = {
                            if (any(values <= 0, na.rm = TRUE)) {
                              log(values + 1)
                            } else {
                              log(values)
                            }
                          },
                          "sqrt" = {
                            if (any(values < 0, na.rm = TRUE)) {
                              stop("Tidak dapat menggunakan sqrt pada nilai negatif")
                            }
                            sqrt(values)
                          },
                          "square" = values^2,
                          "standardize" = {
                            mean_val <- mean(values, na.rm = TRUE)
                            sd_val <- sd(values, na.rm = TRUE)
                            if (sd_val == 0) stop("Standar deviasi = 0, tidak dapat standardisasi")
                            (values - mean_val) / sd_val
                          },
                          "normalize" = {
                            min_val <- min(values, na.rm = TRUE)
                            max_val <- max(values, na.rm = TRUE)
                            if (min_val == max_val) stop("Semua nilai sama, tidak dapat normalisasi")
                            (values - min_val) / (max_val - min_val)
                          }
    )
    
    # Buat nama variabel baru
    new_var_name <- paste0(var_name, "_", suffix)
    
    # Tambahkan ke data
    processed_data$current[[new_var_name]] <- transformed
    
    # Log operasi
    log_entry <- paste("Transformasi", method, "diterapkan pada", var_name, "→", new_var_name)
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    processed_data$operations[[new_var_name]] <- list(
      type = "transform",
      original_var = var_name,
      method = method,
      timestamp = Sys.time()
    )
    
    # Update UI - DIPERBAIKI menggunakan showNotification yang benar
    showNotification("Transformasi berhasil diterapkan!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
  })
})

observeEvent(input$btn_categorize, {
  req(processed_data$current, input$categorize_var, input$categorize_method)
  
  tryCatch({
    data <- processed_data$current
    var_name <- input$categorize_var
    method <- input$categorize_method
    
    values <- data[[var_name]]
    clean_values <- values[!is.na(values)]
    
    if (length(clean_values) < 2) {
      stop("Data tidak cukup untuk kategorisasi")
    }
    
    # Tentukan breakpoints
    if (method == "custom") {
      req(input$custom_breaks)
      breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
      breaks <- c(-Inf, sort(breaks), Inf)
    } else {
      n_breaks <- input$categorize_breaks
      if (method == "quantile") {
        breaks <- quantile(clean_values, probs = seq(0, 1, length.out = n_breaks + 1), na.rm = TRUE)
        # Check for duplicate quantiles
        if (length(unique(breaks)) < length(breaks)) {
          method <- "equal"
          breaks <- seq(min(clean_values), max(clean_values), length.out = n_breaks + 1)
        }
      } else { # equal
        breaks <- seq(min(clean_values), max(clean_values), length.out = n_breaks + 1)
      }
    }
    
    # Tentukan labels
    if (!is.null(input$categorize_labels) && input$categorize_labels != "") {
      labels <- trimws(unlist(strsplit(input$categorize_labels, ",")))
      if (length(labels) != (length(breaks) - 1)) {
        labels <- paste0("Cat_", 1:(length(breaks) - 1))
      }
    } else {
      labels <- paste0("Cat_", 1:(length(breaks) - 1))
    }
    
    # Kategorisasi
    categorized <- cut(values, breaks = breaks, labels = labels, include.lowest = TRUE)
    
    # Nama variabel baru
    new_var_name <- paste0(var_name, "_cat")
    
    # Tambahkan ke data
    processed_data$current[[new_var_name]] <- categorized
    
    # Log operasi
    log_entry <- paste("Kategorisasi", method, "diterapkan pada", var_name, "→", new_var_name)
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    processed_data$operations[[new_var_name]] <- list(
      type = "categorize",
      original_var = var_name,
      method = method,
      breaks = breaks,
      labels = labels,
      timestamp = Sys.time()
    )
    
    showNotification("Kategorisasi berhasil diterapkan!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
  })
})

observeEvent(input$btn_detect_outliers, {
  req(processed_data$current, input$outlier_var, input$outlier_method)
  
  tryCatch({
    data <- processed_data$current
    var_name <- input$outlier_var
    method <- input$outlier_method
    
    values <- data[[var_name]]
    
    # Deteksi outlier berdasarkan metode
    outlier_indices <- switch(method,
                              "iqr" = detect_outliers(values, "iqr", input$iqr_multiplier %||% 1.5),
                              "zscore" = detect_outliers(values, "zscore", input$zscore_threshold %||% 3),
                              "modified_zscore" = detect_outliers(values, "modified_zscore")
    )
    
    # Simpan hasil deteksi
    processed_data$outliers <- outlier_indices
    
    # Log operasi
    log_entry <- paste("Deteksi outlier dengan metode", method, "pada", var_name, ":", length(outlier_indices), "outlier ditemukan")
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    
    showNotification(paste(length(outlier_indices), "outlier terdeteksi!"), type = "warning")
    
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
  })
})

observeEvent(input$btn_impute, {
  req(processed_data$current, input$outlier_var, input$impute_method)
  
  if (is.null(processed_data$outliers) || length(processed_data$outliers) == 0) {
    showNotification("Tidak ada outlier yang terdeteksi. Lakukan deteksi outlier terlebih dahulu.", type = "warning")
    return()
  }
  
  tryCatch({
    data <- processed_data$current
    var_name <- input$outlier_var
    method <- input$impute_method
    outlier_indices <- processed_data$outliers
    
    values <- data[[var_name]]
    
    if (method == "remove") {
      # Hapus baris dengan outlier
      processed_data$current <- processed_data$current[-outlier_indices, ]
      log_entry <- paste("Hapus", length(outlier_indices), "outlier dari", var_name)
    } else {
      # Imputasi
      imputed_values <- impute_values(values, outlier_indices, method)
      processed_data$current[[var_name]] <- imputed_values
      
      log_entry <- paste("Imputasi", length(outlier_indices), "outlier pada", var_name, "dengan metode", method)
    }
    
    # Log operasi
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    processed_data$operations[[paste0(var_name, "_imputed")]] <- list(
      type = "impute",
      original_var = var_name,
      method = method,
      n_outliers = length(outlier_indices),
      timestamp = Sys.time()
    )
    
    # Reset outliers
    processed_data$outliers <- integer(0)
    
    showNotification("Imputasi berhasil diterapkan!", type = "message")
    
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
  })
})

output$dynamic_filter_controls <- renderUI({
  req(input$filter_var)
  
  data <- processed_data$current
  if (is.null(data)) return(NULL)
  
  var_name <- input$filter_var
  
  if (is.numeric(data[[var_name]])) {
    values <- data[[var_name]][!is.na(data[[var_name]])]
    if (length(values) == 0) return(p("Tidak ada data valid"))
    
    sliderInput("filter_range", 
                paste("Range untuk", var_name),
                min = min(values), 
                max = max(values),
                value = c(min(values), max(values)),
                step = (max(values) - min(values)) / 100)
  } else {
    unique_vals <- unique(data[[var_name]][!is.na(data[[var_name]])])
    if (length(unique_vals) == 0) return(p("Tidak ada data valid"))
    
    checkboxGroupInput("filter_categories",
                       paste("Kategori untuk", var_name),
                       choices = unique_vals,
                       selected = unique_vals)
  }
})

observeEvent(input$btn_filter, {
  req(processed_data$current, input$filter_var)
  
  tryCatch({
    data <- processed_data$current
    var_name <- input$filter_var
    
    if (is.numeric(data[[var_name]]) && !is.null(input$filter_range)) {
      # Filter numerik
      min_val <- input$filter_range[1]
      max_val <- input$filter_range[2]
      
      filtered_data <- data[!is.na(data[[var_name]]) & 
                              data[[var_name]] >= min_val & 
                              data[[var_name]] <= max_val, ]
      
      log_entry <- paste("Filter numerik pada", var_name, ": range [", min_val, ",", max_val, "]")
      
    } else if (!is.null(input$filter_categories)) {
      # Filter kategori
      selected_cats <- input$filter_categories
      
      filtered_data <- data[!is.na(data[[var_name]]) & 
                              data[[var_name]] %in% selected_cats, ]
      
      log_entry <- paste("Filter kategori pada", var_name, ":", length(selected_cats), "kategori dipilih")
    } else {
      stop("Filter tidak dapat diterapkan")
    }
    
    # Validasi hasil filter
    if (nrow(filtered_data) == 0) {
      stop("Filter menghasilkan data kosong")
    }
    
    # Update data
    n_before <- nrow(processed_data$current)
    processed_data$current <- filtered_data
    n_after <- nrow(filtered_data)
    
    # Log operasi
    log_entry <- paste(log_entry, "-", n_before, "→", n_after, "baris")
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    
    showNotification(paste("Filter diterapkan:", n_after, "baris tersisa"), type = "message")
    
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
  })
})

# Reset filter - DIPERBAIKI
observeEvent(input$btn_reset_filter, {
  if (!is.null(processed_data$original)) {
    processed_data$current <- processed_data$original
    processed_data$operation_log <- c(processed_data$operation_log, "Filter direset")
    showNotification("Filter telah direset", type = "message")
  }
})

observeEvent(input$btn_reset_all, {
  showModal(modalDialog(
    title = "Konfirmasi Reset",
    "Apakah Anda yakin ingin menghapus semua perubahan dan kembali ke data asli?",
    footer = tagList(
      modalButton("Batal"),
      actionButton("confirm_reset", "Ya, Reset", class = "btn-danger")
    )
  ))
})

observeEvent(input$confirm_reset, {
  tryCatch({
    if (!is.null(processed_data$original)) {
      processed_data$current <- processed_data$original
      processed_data$operations <- list()
      processed_data$outliers <- integer(0)
      processed_data$operation_log <- c("Data direset ke kondisi awal")
      
      removeModal()
      showNotification("Semua data telah direset!", type = "message")
    }
  }, error = function(e) {
    removeModal()
    showNotification(paste("Error reset:", e$message), type = "error")
  })
})

output$descriptive_stats_table <- DT::renderDataTable({
  data <- processed_data$current
  if (!is.null(data)) {
    tryCatch({
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      
      if (length(numeric_vars) > 0) {
        stats_df <- data.frame(
          Variabel = character(),
          N = numeric(),
          Mean = numeric(),
          Median = numeric(),
          SD = numeric(),
          Min = numeric(),
          Max = numeric(),
          Missing = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (var in numeric_vars) {
          values <- data[[var]]
          valid_values <- values[!is.na(values)]
          
          if (length(valid_values) > 0) {
            stats_df <- rbind(stats_df, data.frame(
              Variabel = var,
              N = length(valid_values),
              Mean = round(mean(valid_values), 3),
              Median = round(median(valid_values), 3),
              SD = round(sd(valid_values), 3),
              Min = round(min(valid_values), 3),
              Max = round(max(valid_values), 3),
              Missing = sum(is.na(values)),
              stringsAsFactors = FALSE
            ))
          }
        }
        
        DT::datatable(
          stats_df,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'frtip'
          ),
          rownames = FALSE
        )
      }
    }, error = function(e) {
      NULL
    })
  }
})

output$visualization_plot <- renderPlot({
  req(processed_data$current, input$viz_var, input$viz_type)
  
  tryCatch({
    data <- processed_data$current
    var_name <- input$viz_var
    values <- data[[var_name]][!is.na(data[[var_name]])]
    
    if (length(values) == 0) {
      plot(1, 1, type = "n", main = "Tidak ada data untuk divisualisasikan")
      return()
    }
    
    switch(input$viz_type,
           "hist" = {
             hist(values, 
                  main = paste("Histogram", var_name),
                  xlab = var_name,
                  ylab = "Frekuensi",
                  col = "lightblue",
                  border = "white",
                  breaks = min(30, length(unique(values))))
           },
           "box" = {
             boxplot(values,
                     main = paste("Boxplot", var_name),
                     ylab = var_name,
                     col = "lightgreen",
                     border = "darkgreen")
           },
           "qq" = {
             qqnorm(values, main = paste("Q-Q Plot", var_name))
             qqline(values, col = "red", lwd = 2)
           }
    )
  }, error = function(e) {
    plot(1, 1, type = "n", main = paste("Error:", e$message))
  })
})

output$operation_log <- renderText({
  if (length(processed_data$operation_log) > 0) {
    paste(
      "=== LOG OPERASI DATA ===\n",
      paste(processed_data$operation_log, collapse = "\n"),
      sep = "\n"
    )
  } else {
    "Belum ada operasi yang dilakukan."
  }
})

output$operation_interpretation <- renderText({
  operations <- processed_data$operations
  
  if (length(operations) == 0) {
    return("=== INTERPRETASI OPERASI ===\n\nBelum ada operasi yang dilakukan.\n\nGUNAKAN FITUR:\n1. TRANSFORMASI: Perbaiki distribusi data yang skew\n2. KATEGORISASI: Ubah data kontinyu menjadi kategori\n3. OUTLIER: Deteksi dan tangani data ekstrem\n4. FILTER: Fokus pada subset data tertentu")
  }
  
  interpretation <- "=== INTERPRETASI OPERASI ===\n\n"
  
  # Analisis berdasarkan jenis operasi
  transform_ops <- operations[sapply(operations, function(x) x$type == "transform")]
  categorize_ops <- operations[sapply(operations, function(x) x$type == "categorize")]
  impute_ops <- operations[sapply(operations, function(x) x$type == "impute")]
  
  if (length(transform_ops) > 0) {
    interpretation <- paste0(interpretation, "TRANSFORMASI DITERAPKAN:\n")
    for (op_name in names(transform_ops)) {
      op <- transform_ops[[op_name]]
      method_desc <- switch(op$method,
                            "log" = "Logaritma (mengurangi skewness, stabilisasi variansi)",
                            "sqrt" = "Akar kuadrat (mengurangi heteroskedastisitas)",
                            "square" = "Kuadrat (memperkuat sinyal, meningkatkan variansi)",
                            "standardize" = "Z-score (mean=0, sd=1, untuk perbandingan skala)",
                            "normalize" = "Min-Max (range 0-1, untuk algoritma sensitif skala)",
                            op$method
      )
      interpretation <- paste0(interpretation, "- ", op_name, ": ", method_desc, "\n")
    }
    interpretation <- paste0(interpretation, "\n")
  }
  
  if (length(categorize_ops) > 0) {
    interpretation <- paste0(interpretation, "KATEGORISASI DITERAPKAN:\n")
    for (op_name in names(categorize_ops)) {
      op <- categorize_ops[[op_name]]
      method_desc <- switch(op$method,
                            "quantile" = "berdasarkan persentil (distribusi seimbang)",
                            "equal" = "berdasarkan interval sama (range seragam)",
                            "custom" = "berdasarkan breakpoint khusus",
                            op$method
      )
      interpretation <- paste0(interpretation, "- ", op_name, ": ", length(op$labels), " kategori ", method_desc, "\n")
    }
    interpretation <- paste0(interpretation, "\n")
  }
  
  if (length(impute_ops) > 0) {
    interpretation <- paste0(interpretation, "IMPUTASI OUTLIER:\n")
    for (op_name in names(impute_ops)) {
      op <- impute_ops[[op_name]]
      interpretation <- paste0(interpretation, "- ", op$original_var, ": ", op$n_outliers, " outlier ditangani dengan ", op$method, "\n")
    }
    interpretation <- paste0(interpretation, "\n")
  }
  
  # Rekomendasi
  interpretation <- paste0(interpretation, "REKOMENDASI ANALISIS LANJUTAN:\n")
  
  if (length(transform_ops) > 0) {
    interpretation <- paste0(interpretation, "✓ Uji normalitas pada variabel yang ditransformasi\n")
  }
  
  if (length(categorize_ops) > 0) {
    interpretation <- paste0(interpretation, "✓ Gunakan uji chi-square untuk analisis kategori\n")
  }
  
  if (length(impute_ops) > 0) {
    interpretation <- paste0(interpretation, "✓ Bandingkan hasil analisis sebelum dan sesudah imputasi\n")
  }
  
  interpretation <- paste0(interpretation, "✓ Periksa korelasi antar variabel setelah preprocessing\n")
  interpretation <- paste0(interpretation, "✓ Validasi hasil dengan domain expert\n")
  
  return(interpretation)
})

output$btn_export_data <- downloadHandler(
  filename = function() {
    paste0("processed_data_", Sys.Date(), ".csv")
  },
  content = function(file) {
    if (!is.null(processed_data$current)) {
      write.csv(processed_data$current, file, row.names = FALSE)
    }
  }
)

cat("✓ Fixed data management server loaded successfully\n")