library(ggplot2)
library(plotly)
library(RColorBrewer)

# Fungsi untuk membuat histogram
create_histogram <- function(data, variable, bins = 30, color = "steelblue") {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  p <- ggplot(data, aes_string(x = variable)) +
    geom_histogram(bins = bins, fill = color, alpha = 0.7, color = "white") +
    geom_density(aes(y = after_stat(density) * nrow(data) * (max(data[[variable]], na.rm = TRUE) - min(data[[variable]], na.rm = TRUE)) / bins), 
                 color = "red", size = 1) +
    labs(
      title = paste("Histogram", variable),
      x = variable,
      y = "Frekuensi"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  return(p)
}

# Fungsi untuk membuat boxplot
create_boxplot <- function(data, variable, group_var = NULL, color = "steelblue") {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  if (is.null(group_var)) {
    p <- ggplot(data, aes_string(y = variable)) +
      geom_boxplot(fill = color, alpha = 0.7) +
      labs(
        title = paste("Boxplot", variable),
        y = variable
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  } else {
    if (!group_var %in% names(data)) {
      stop("Variabel grup tidak ditemukan dalam data")
    }
    
    p <- ggplot(data, aes_string(x = group_var, y = variable, fill = group_var)) +
      geom_boxplot(alpha = 0.7) +
      labs(
        title = paste("Boxplot", variable, "berdasarkan", group_var),
        x = group_var,
        y = variable
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none"
      )
  }
  
  return(p)
}

# Fungsi untuk membuat scatter plot
create_scatterplot <- function(data, x_var, y_var, color_var = NULL) {
  if (!x_var %in% names(data) || !y_var %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  if (is.null(color_var)) {
    p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(
        title = paste("Scatter Plot:", y_var, "vs", x_var),
        x = x_var,
        y = y_var
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  } else {
    if (!color_var %in% names(data)) {
      stop("Variabel warna tidak ditemukan dalam data")
    }
    
    p <- ggplot(data, aes_string(x = x_var, y = y_var, color = color_var)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(
        title = paste("Scatter Plot:", y_var, "vs", x_var, "berdasarkan", color_var),
        x = x_var,
        y = y_var,
        color = color_var
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  }
  
  return(p)
}

# Fungsi untuk membuat correlation heatmap
create_correlation_heatmap <- function(data, variables = NULL, method = "pearson") {
  if (is.null(variables)) {
    numeric_vars <- sapply(data, is.numeric)
    variables <- names(data)[numeric_vars]
  }
  
  # Filter hanya variabel numerik yang ada dalam data
  variables <- variables[variables %in% names(data)]
  cor_data <- data[variables]
  
  # Hapus kolom dengan variance 0 atau missing values
  cor_data <- cor_data[sapply(cor_data, function(x) var(x, na.rm = TRUE) > 0)]
  
  if (ncol(cor_data) < 2) {
    stop("Tidak cukup variabel numerik untuk membuat correlation heatmap")
  }
  
  # Hitung korelasi
  cor_matrix <- cor(cor_data, use = "complete.obs", method = method)
  
  # Konversi ke format long untuk ggplot
  cor_melted <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
  cor_melted$value <- as.vector(cor_matrix)
  
  p <- ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c(-1, 1), space = "Lab",
                        name = paste("Korelasi\n", tools::toTitleCase(method))) +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    labs(
      title = paste("Heatmap Korelasi", tools::toTitleCase(method)),
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(size = 10)
    )
  
  return(p)
}

# Fungsi untuk membuat Q-Q plot
create_qqplot <- function(data, variable) {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  values <- data[[variable]][!is.na(data[[variable]])]
  
  p <- ggplot(data.frame(sample = values), aes(sample = sample)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    labs(
      title = paste("Q-Q Plot untuk", variable),
      x = "Theoretical Quantiles",
      y = "Sample Quantiles"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  return(p)
}

# Fungsi untuk membuat bar plot
create_barplot <- function(data, variable, fill_color = "steelblue") {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  # Hitung frekuensi
  freq_data <- data %>%
    count(!!sym(variable)) %>%
    arrange(desc(n))
  
  p <- ggplot(freq_data, aes_string(x = variable, y = "n")) +
    geom_bar(stat = "identity", fill = fill_color, alpha = 0.7) +
    geom_text(aes(label = n), vjust = -0.3) +
    labs(
      title = paste("Bar Plot untuk", variable),
      x = variable,
      y = "Frekuensi"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}
