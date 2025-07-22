# modules/visualization_functions.R - Versi Aman dari Error

# Load required libraries dengan error handling
safe_library <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    warning(paste("Package", package_name, "not available"))
    return(FALSE)
  }
  library(package_name, character.only = TRUE)
  return(TRUE)
}

# Load libraries
required_libs <- c("ggplot2", "plotly", "dplyr", "reshape2")
for(lib in required_libs) {
  safe_library(lib)
}

# Safely load e1071 for skewness/kurtosis
has_e1071 <- safe_library("e1071")

# Fungsi untuk membuat histogram dengan error handling
create_histogram <- function(data, variable, bins = 30, fill_color = "steelblue") {
  # Validasi input
  if(missing(data) || missing(variable)) {
    stop("Data dan variable harus disediakan")
  }
  
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  # Remove missing values
  clean_data <- data[[variable]]
  clean_data <- clean_data[!is.na(clean_data)]
  
  if(length(clean_data) == 0) {
    stop("Tidak ada data valid untuk histogram")
  }
  
  # Calculate statistics for annotations
  mean_val <- mean(clean_data)
  median_val <- median(clean_data)
  
  tryCatch({
    p <- ggplot(data.frame(x = clean_data), aes(x = x)) +
      geom_histogram(bins = bins, fill = fill_color, alpha = 0.7, color = "white") +
      geom_vline(aes(xintercept = mean_val), color = "red", linetype = "dashed", linewidth = 1) +
      geom_vline(aes(xintercept = median_val), color = "blue", linetype = "dashed", linewidth = 1) +
      annotate("text", x = mean_val, y = Inf, label = paste("Mean:", round(mean_val, 2)), 
               vjust = 2, hjust = -0.1, color = "red", size = 3.5) +
      annotate("text", x = median_val, y = Inf, label = paste("Median:", round(median_val, 2)), 
               vjust = 3.5, hjust = -0.1, color = "blue", size = 3.5) +
      labs(
        title = paste("Distribusi", variable),
        subtitle = paste("n =", length(clean_data), "| Missing:", sum(is.na(data[[variable]]))),
        x = variable,
        y = "Frekuensi"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray60"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
    
    return(p)
  }, error = function(e) {
    # Fallback: simple histogram
    hist_data <- data.frame(x = clean_data)
    p <- ggplot(hist_data, aes(x = x)) +
      geom_histogram(bins = bins, fill = fill_color, alpha = 0.7) +
      labs(title = paste("Distribusi", variable), x = variable, y = "Frekuensi") +
      theme_minimal()
    return(p)
  })
}

# Fungsi untuk membuat boxplot dengan error handling
create_boxplot <- function(data, variable, group_var = NULL, fill_color = "lightblue") {
  # Validasi input
  if(missing(data) || missing(variable)) {
    stop("Data dan variable harus disediakan")
  }
  
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  # Clean data
  if (!is.null(group_var) && group_var %in% names(data)) {
    # Grouped boxplot
    clean_data <- data[!is.na(data[[variable]]) & !is.na(data[[group_var]]), ]
    
    if(nrow(clean_data) == 0) {
      stop("Tidak ada data valid untuk boxplot")
    }
    
    tryCatch({
      p <- ggplot(clean_data, aes_string(x = group_var, y = variable, fill = group_var)) +
        geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
        labs(
          title = paste("Distribusi", variable, "berdasarkan", group_var),
          subtitle = "♦ = Mean, ● = Outliers",
          x = group_var,
          y = variable
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray60"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        )
      return(p)
    }, error = function(e) {
      # Fallback: simple grouped boxplot
      p <- ggplot(clean_data, aes_string(x = group_var, y = variable)) +
        geom_boxplot(fill = fill_color) +
        labs(title = paste("Boxplot", variable), x = group_var, y = variable) +
        theme_minimal()
      return(p)
    })
  } else {
    # Single boxplot
    clean_data <- data[[variable]]
    clean_data <- clean_data[!is.na(clean_data)]
    
    if(length(clean_data) == 0) {
      stop("Tidak ada data valid untuk boxplot")
    }
    
    # Calculate outliers
    q1 <- quantile(clean_data, 0.25)
    q3 <- quantile(clean_data, 0.75)
    iqr <- q3 - q1
    outliers_count <- sum(clean_data < (q1 - 1.5 * iqr) | clean_data > (q3 + 1.5 * iqr))
    
    tryCatch({
      plot_data <- data.frame(y = clean_data, x = "")
      
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_boxplot(fill = fill_color, alpha = 0.7, outlier.color = "red", outlier.size = 2) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
        labs(
          title = paste("Distribusi", variable),
          subtitle = paste("Outliers:", outliers_count, "| ♦ = Mean"),
          x = "",
          y = variable
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray60"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      return(p)
    }, error = function(e) {
      # Fallback: simple boxplot
      plot_data <- data.frame(y = clean_data, x = "")
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_boxplot(fill = fill_color) +
        labs(title = paste("Boxplot", variable), x = "", y = variable) +
        theme_minimal()
      return(p)
    })
  }
}

# Fungsi untuk membuat scatter plot dengan error handling
create_scatterplot <- function(data, x_var, y_var, color_var = NULL) {
  # Validasi input
  if(missing(data) || missing(x_var) || missing(y_var)) {
    stop("Data, x_var, dan y_var harus disediakan")
  }
  
  if (!x_var %in% names(data) || !y_var %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  # Clean data
  if (!is.null(color_var) && color_var %in% names(data)) {
    clean_data <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]) & !is.na(data[[color_var]]), ]
  } else {
    clean_data <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]), ]
  }
  
  if(nrow(clean_data) == 0) {
    stop("Tidak ada data valid untuk scatter plot")
  }
  
  # Calculate correlation
  correlation <- tryCatch(
    cor(clean_data[[x_var]], clean_data[[y_var]], use = "complete.obs"),
    error = function(e) NA
  )
  
  if (!is.null(color_var) && color_var %in% names(data)) {
    # Colored scatter plot
    tryCatch({
      p <- ggplot(clean_data, aes_string(x = x_var, y = y_var, color = color_var)) +
        geom_point(alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
        labs(
          title = paste("Hubungan", y_var, "vs", x_var, "berdasarkan", color_var),
          subtitle = if(!is.na(correlation)) paste("Korelasi Pearson:", round(correlation, 3)) else "Korelasi tidak dapat dihitung",
          x = x_var,
          y = y_var,
          color = color_var
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray60"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
      return(p)
    }, error = function(e) {
      # Fallback: simple colored scatter
      p <- ggplot(clean_data, aes_string(x = x_var, y = y_var, color = color_var)) +
        geom_point(alpha = 0.6) +
        labs(title = paste("Scatter Plot:", y_var, "vs", x_var), x = x_var, y = y_var) +
        theme_minimal()
      return(p)
    })
  } else {
    # Simple scatter plot
    tryCatch({
      p <- ggplot(clean_data, aes_string(x = x_var, y = y_var)) +
        geom_point(alpha = 0.6, color = "steelblue", size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
        labs(
          title = paste("Hubungan", y_var, "vs", x_var),
          subtitle = if(!is.na(correlation)) paste("Korelasi Pearson:", round(correlation, 3)) else "Korelasi tidak dapat dihitung",
          x = x_var,
          y = y_var
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray60"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        )
      
      # Add correlation annotation if available
      if(!is.na(correlation)) {
        p <- p + annotate("text", x = Inf, y = Inf, 
                          label = paste("r =", round(correlation, 3)), 
                          hjust = 1.1, vjust = 2, size = 5, color = "red", fontface = "bold")
      }
      
      return(p)
    }, error = function(e) {
      # Fallback: simple scatter
      p <- ggplot(clean_data, aes_string(x = x_var, y = y_var)) +
        geom_point(alpha = 0.6, color = "steelblue") +
        labs(title = paste("Scatter Plot:", y_var, "vs", x_var), x = x_var, y = y_var) +
        theme_minimal()
      return(p)
    })
  }
}

# Fungsi untuk membuat correlation heatmap dengan error handling
create_correlation_heatmap <- function(data, variables = NULL, method = "pearson") {
  # Validasi input
  if(missing(data)) {
    stop("Data harus disediakan")
  }
  
  if(is.null(data) || nrow(data) == 0) {
    stop("Data kosong atau null")
  }
  
  # Determine numeric variables
  if (is.null(variables)) {
    numeric_vars <- sapply(data, is.numeric)
    variables <- names(data)[numeric_vars]
  }
  
  # Filter hanya variabel numerik yang ada dalam data
  variables <- variables[variables %in% names(data)]
  
  if(length(variables) == 0) {
    stop("Tidak ada variabel numerik dalam data")
  }
  
  cor_data <- data[variables]
  
  # Hapus kolom dengan variance 0 atau semua NA
  valid_vars <- sapply(cor_data, function(x) {
    clean_x <- x[!is.na(x)]
    length(clean_x) > 1 && var(clean_x) > 0
  })
  
  cor_data <- cor_data[valid_vars]
  
  if (ncol(cor_data) < 2) {
    stop("Minimal 2 variabel numerik diperlukan untuk correlation heatmap")
  }
  
  # Hitung korelasi dengan error handling
  cor_matrix <- tryCatch({
    cor(cor_data, use = "complete.obs", method = method)
  }, error = function(e) {
    # Fallback: try with different method
    tryCatch({
      cor(cor_data, use = "pairwise.complete.obs", method = "pearson")
    }, error = function(e2) {
      stop("Tidak dapat menghitung korelasi")
    })
  })
  
  if(any(is.na(cor_matrix))) {
    warning("Beberapa korelasi tidak dapat dihitung")
  }
  
  # Konversi ke format long untuk ggplot
  tryCatch({
    # Use melt from reshape2 if available, otherwise manual conversion
    if(requireNamespace("reshape2", quietly = TRUE)) {
      cor_melted <- reshape2::melt(cor_matrix, na.rm = FALSE)
      names(cor_melted) <- c("Var1", "Var2", "value")
    } else {
      # Manual melting
      cor_melted <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_melted$value <- as.vector(cor_matrix)
    }
    
    # Tambahkan kategori kekuatan korelasi
    cor_melted$strength <- cut(abs(cor_melted$value), 
                               breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
                               labels = c("Sangat Lemah", "Lemah", "Sedang", "Kuat", "Sangat Kuat"),
                               include.lowest = TRUE)
    
    # Create enhanced heatmap
    p <- ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = round(value, 2)), 
                color = ifelse(abs(cor_melted$value) > 0.5, "white", "black"), 
                size = 3, fontface = "bold") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = paste("Korelasi\n", tools::toTitleCase(method)),
                           breaks = c(-1, -0.5, 0, 0.5, 1),
                           labels = c("-1.0\n(Negatif Kuat)", "-0.5", "0", "0.5", "1.0\n(Positif Kuat)")) +
      labs(
        title = paste("Heatmap Korelasi", tools::toTitleCase(method)),
        subtitle = paste("Analisis", ncol(cor_data), "variabel numerik"),
        x = "Variabel",
        y = "Variabel",
        caption = "Warna: Biru = Negatif, Putih = Netral, Merah = Positif"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray60"),
        plot.caption = element_text(size = 10, color = "gray60"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid = element_blank()
      ) +
      coord_fixed() # Membuat sel menjadi persegi
    
    return(p)
    
  }, error = function(e) {
    # Fallback: simple heatmap
    cor_df <- as.data.frame(as.table(cor_matrix))
    names(cor_df) <- c("Var1", "Var2", "value")
    
    p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Heatmap") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    return(p)
  })
}

# Fungsi helper untuk interpretasi korelasi
interpret_correlation <- function(cor_value) {
  if(is.na(cor_value)) return("Tidak dapat dihitung")
  
  abs_cor <- abs(cor_value)
  
  strength <- if(abs_cor >= 0.8) "sangat kuat" else
    if(abs_cor >= 0.6) "kuat" else
      if(abs_cor >= 0.4) "sedang" else
        if(abs_cor >= 0.2) "lemah" else
          "sangat lemah"
  
  direction <- if(cor_value > 0) "positif" else "negatif"
  
  return(paste("Korelasi", direction, strength))
}

# Fungsi untuk analisis distribusi dengan error handling
analyze_distribution <- function(data, variable) {
  if(missing(data) || missing(variable)) {
    stop("Data dan variable harus disediakan")
  }
  
  if(!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  values <- data[[variable]]
  clean_values <- values[!is.na(values)]
  
  if(length(clean_values) == 0) {
    return(list(error = "Tidak ada data valid"))
  }
  
  analysis <- list(
    n = length(clean_values),
    missing = sum(is.na(values)),
    mean = tryCatch(mean(clean_values), error = function(e) NA),
    median = tryCatch(median(clean_values), error = function(e) NA),
    sd = tryCatch(sd(clean_values), error = function(e) NA),
    min = tryCatch(min(clean_values), error = function(e) NA),
    max = tryCatch(max(clean_values), error = function(e) NA),
    q1 = tryCatch(quantile(clean_values, 0.25), error = function(e) NA),
    q3 = tryCatch(quantile(clean_values, 0.75), error = function(e) NA)
  )
  
  # CV calculation
  if(!is.na(analysis$mean) && !is.na(analysis$sd) && analysis$mean != 0) {
    analysis$cv <- analysis$sd / analysis$mean * 100
  } else {
    analysis$cv <- NA
  }
  
  # Skewness and Kurtosis (only if e1071 is available)
  if(has_e1071) {
    analysis$skewness <- tryCatch(e1071::skewness(clean_values), error = function(e) NA)
    analysis$kurtosis <- tryCatch(e1071::kurtosis(clean_values), error = function(e) NA)
  } else {
    analysis$skewness <- NA
    analysis$kurtosis <- NA
  }
  
  # Outliers
  if(!is.na(analysis$q1) && !is.na(analysis$q3)) {
    iqr <- analysis$q3 - analysis$q1
    analysis$outliers <- sum(clean_values < (analysis$q1 - 1.5 * iqr) | 
                               clean_values > (analysis$q3 + 1.5 * iqr))
  } else {
    analysis$outliers <- NA
  }
  
  # Normalitas
  if(length(clean_values) >= 3 && length(clean_values) <= 5000) {
    analysis$shapiro_p <- tryCatch(shapiro.test(clean_values)$p.value, error = function(e) NA)
    analysis$normal <- if(!is.na(analysis$shapiro_p)) analysis$shapiro_p > 0.05 else NA
  } else {
    analysis$shapiro_p <- NA
    analysis$normal <- NA
  }
  
  return(analysis)
}

# Fungsi untuk membuat Q-Q plot dengan error handling yang baik
create_qqplot <- function(data, variable) {
  # Validasi input
  if(missing(data) || missing(variable)) {
    stop("Data dan variable harus disediakan")
  }
  
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  # Remove missing values
  clean_data <- data[[variable]]
  clean_data <- clean_data[!is.na(clean_data)]
  
  if(length(clean_data) == 0) {
    stop("Tidak ada data valid untuk Q-Q plot")
  }
  
  if(length(clean_data) < 3) {
    stop("Data terlalu sedikit untuk Q-Q plot (minimal 3 observasi)")
  }
  
  tryCatch({
    # Create Q-Q plot using ggplot2
    qq_data <- data.frame(
      sample = clean_data
    )
    
    # Calculate theoretical quantiles
    n <- length(clean_data)
    theoretical_quantiles <- qnorm((1:n - 0.5) / n)
    empirical_quantiles <- sort(clean_data)
    
    # Standardize empirical quantiles
    empirical_standardized <- (empirical_quantiles - mean(empirical_quantiles)) / sd(empirical_quantiles)
    
    plot_data <- data.frame(
      theoretical = theoretical_quantiles,
      sample = empirical_standardized
    )
    
    # Calculate R-squared for line fit
    cor_coef <- cor(theoretical_quantiles, empirical_standardized)
    r_squared <- cor_coef^2
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = theoretical, y = sample)) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
      labs(
        title = paste("Q-Q Plot:", variable),
        subtitle = paste("Uji Normalitas Visual | R² =", round(r_squared, 3), "| n =", n),
        x = "Theoretical Quantiles (Normal Distribution)",
        y = "Standardized Sample Quantiles",
        caption = "Garis merah: distribusi normal teoretis"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray60"),
        plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank()
      )
    
    # Add interpretation text
    interpretation <- if(r_squared > 0.95) {
      "Interpretasi: Data mengikuti distribusi normal dengan baik"
    } else if(r_squared > 0.90) {
      "Interpretasi: Data mendekati distribusi normal"
    } else {
      "Interpretasi: Data menyimpang dari distribusi normal"
    }
    
    p <- p + annotate("text", x = -Inf, y = Inf, 
                      label = interpretation, 
                      hjust = -0.05, vjust = 1.5, 
                      size = 3.5, color = "darkred", fontface = "bold")
    
    return(p)
    
  }, error = function(e) {
    # Fallback: Base R Q-Q plot
    warning("ggplot2 error, menggunakan base R plot")
    
    # Use base R for simple Q-Q plot
    par(mfrow = c(1, 1))
    qqnorm(clean_data, main = paste("Q-Q Plot:", variable), 
           xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
    qqline(clean_data, col = "red", lwd = 2)
    
    # Return a simple ggplot as placeholder
    simple_plot <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
      geom_text(label = "Q-Q Plot dibuat dengan base R", size = 5) +
      labs(title = paste("Q-Q Plot:", variable)) +
      theme_void()
    
    return(simple_plot)
  })
}
