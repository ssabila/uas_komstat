# server/visualization_server.R

# Selektor variabel utama (untuk histogram dan boxplot)
output$vis_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  
  selectInput("vis_var", "Variabel:", 
              choices = numeric_vars,
              selected = if(length(numeric_vars) > 0) numeric_vars[1] else NULL)
})

# Selektor variabel grup (untuk histogram dan boxplot)
output$vis_group_selector <- renderUI({
  req(processed_data$current)
  categorical_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) is.character(x) || is.factor(x))]
  
  selectInput("vis_group", "Grup (Opsional):", 
              choices = c("Tidak Ada", categorical_vars),
              selected = "Tidak Ada")
})

# Selektor variabel X khusus untuk scatter plot
output$vis_x_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  
  selectInput("vis_x_var", "Variabel X:", 
              choices = numeric_vars,
              selected = if(length(numeric_vars) > 0) numeric_vars[1] else NULL)
})

# Selektor variabel Y khusus untuk scatter plot
output$vis_y_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  
  selectInput("vis_y_var", "Variabel Y:", 
              choices = numeric_vars,
              selected = if(length(numeric_vars) > 1) numeric_vars[2] else NULL)
})

# Selektor variabel warna untuk scatter plot
output$vis_color_selector <- renderUI({
  req(processed_data$current)
  categorical_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) is.character(x) || is.factor(x))]
  
  selectInput("vis_color_var", "Warna:", 
              choices = c("Tidak Ada" = "", categorical_vars),
              selected = "")
})

# Render plot utama
output$main_plot <- renderPlotly({
  req(processed_data$current)
  
  tryCatch({
    data <- processed_data$current
    
    # Validasi data
    if(is.null(data) || nrow(data) == 0) {
      return(plotly::plot_ly() %>% 
               plotly::add_text(x = 0.5, y = 0.5, text = "Data tidak tersedia",
                                textfont = list(size = 16, color = "red")))
    }
    
    # Tentukan jenis plot berdasarkan input atau default
    plot_type <- if(is.null(input$vis_plot_type)) "Histogram" else input$vis_plot_type
    
    p <- switch(plot_type,
                "Histogram" = {
                  var_name <- if(is.null(input$vis_var)) {
                    numeric_vars <- names(data)[sapply(data, is.numeric)]
                    if(length(numeric_vars) > 0) numeric_vars[1] else NULL
                  } else {
                    input$vis_var
                  }
                  
                  if(is.null(var_name) || !var_name %in% names(data)) {
                    return(plotly::plot_ly() %>% 
                             plotly::add_text(x = 0.5, y = 0.5, text = "Variabel tidak tersedia",
                                              textfont = list(size = 16, color = "red")))
                  }
                  
                  create_histogram(data, var_name)
                },
                
                "Boxplot" = {
                  var_name <- if(is.null(input$vis_var)) {
                    numeric_vars <- names(data)[sapply(data, is.numeric)]
                    if(length(numeric_vars) > 0) numeric_vars[1] else NULL
                  } else {
                    input$vis_var
                  }
                  
                  if(is.null(var_name) || !var_name %in% names(data)) {
                    return(plotly::plot_ly() %>% 
                             plotly::add_text(x = 0.5, y = 0.5, text = "Variabel tidak tersedia",
                                              textfont = list(size = 16, color = "red")))
                  }
                  
                  group_var <- if(is.null(input$vis_group) || input$vis_group == "Tidak Ada") NULL else input$vis_group
                  create_boxplot(data, var_name, group_var = group_var)
                },
                
                "Scatter Plot" = {
                  # Default variables jika input belum tersedia
                  numeric_vars <- names(data)[sapply(data, is.numeric)]
                  
                  x_var <- if(is.null(input$vis_x_var)) {
                    if(length(numeric_vars) > 0) numeric_vars[1] else NULL
                  } else {
                    input$vis_x_var
                  }
                  
                  y_var <- if(is.null(input$vis_y_var)) {
                    if(length(numeric_vars) > 1) numeric_vars[2] else if(length(numeric_vars) > 0) numeric_vars[1] else NULL
                  } else {
                    input$vis_y_var
                  }
                  
                  # Validasi variabel
                  if(is.null(x_var) || is.null(y_var) || !x_var %in% names(data) || !y_var %in% names(data)) {
                    return(plotly::plot_ly() %>% 
                             plotly::add_text(x = 0.5, y = 0.5, text = "Variabel tidak tersedia",
                                              textfont = list(size = 16, color = "red")))
                  }
                  
                  # Cek apakah variabel X dan Y sama
                  if(x_var == y_var) {
                    return(plotly::plot_ly() %>% 
                             plotly::add_text(x = 0.5, y = 0.5, text = "Pilih variabel X dan Y yang berbeda",
                                              textfont = list(size = 16, color = "orange")))
                  }
                  
                  # Tentukan variabel warna
                  color_var <- if(is.null(input$vis_color_var) || input$vis_color_var == "") {
                    NULL
                  } else {
                    input$vis_color_var
                  }
                  
                  # Tentukan apakah menampilkan trend line
                  show_trend <- if(is.null(input$vis_show_trend)) TRUE else input$vis_show_trend
                  
                  # Buat scatter plot
                  create_scatterplot(data, 
                                     x_var = x_var, 
                                     y_var = y_var, 
                                     color_var = color_var,
                                     show_trend = show_trend)
                }
    )
    
    if(is.null(p)) {
      return(plotly::plot_ly() %>% 
               plotly::add_text(x = 0.5, y = 0.5, text = "Error membuat plot",
                                textfont = list(size = 16, color = "red")))
    }
    
    # Konversi ke plotly
    ggplotly(p, tooltip = "all") %>%
      plotly::layout(
        title = list(font = list(size = 16)),
        margin = list(t = 80, b = 60, l = 60, r = 60)
      )
    
  }, error = function(e) {
    showNotification(paste("Error creating plot:", e$message), type = "error")
    return(plotly::plot_ly() %>% 
             plotly::add_text(x = 0.5, y = 0.5, text = paste("Error:", e$message),
                              textfont = list(size = 14, color = "red")))
  })
})

# Interpretasi sebaran data
output$data_interpretation <- renderText({
  req(processed_data$current)
  
  tryCatch({
    data <- processed_data$current
    plot_type <- if(is.null(input$vis_plot_type)) "Histogram" else input$vis_plot_type
    
    if(plot_type == "Scatter Plot") {
      # Default variables jika input belum tersedia
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      
      x_var <- if(is.null(input$vis_x_var)) {
        if(length(numeric_vars) > 0) numeric_vars[1] else NULL
      } else {
        input$vis_x_var
      }
      
      y_var <- if(is.null(input$vis_y_var)) {
        if(length(numeric_vars) > 1) numeric_vars[2] else if(length(numeric_vars) > 0) numeric_vars[1] else NULL
      } else {
        input$vis_y_var
      }
      
      if(is.null(x_var) || is.null(y_var) || !x_var %in% names(data) || !y_var %in% names(data)) {
        return("<p style='color: red;'>Variabel tidak ditemukan dalam data.</p>")
      }
      
      if(x_var == y_var) {
        return("<p style='color: orange;'>Pilih variabel X dan Y yang berbeda untuk analisis yang bermakna.</p>")
      }
      
      # Analisis korelasi untuk scatter plot
      clean_data <- data[!is.na(data[[x_var]]) & !is.na(data[[y_var]]), ]
      
      if(nrow(clean_data) < 3) {
        return("<p style='color: red;'>Data tidak cukup untuk analisis korelasi (minimal 3 observasi).</p>")
      }
      
      correlation <- cor(clean_data[[x_var]], clean_data[[y_var]], use = "complete.obs")
      
      # Interpretasi kekuatan korelasi
      strength <- if(abs(correlation) >= 0.8) {
        "sangat kuat"
      } else if(abs(correlation) >= 0.6) {
        "kuat"
      } else if(abs(correlation) >= 0.4) {
        "sedang"
      } else if(abs(correlation) >= 0.2) {
        "lemah"
      } else {
        "sangat lemah"
      }
      
      direction <- if(correlation > 0) "positif" else "negatif"
      
      interpretation <- paste0(
        "<h5><i class='fa fa-chart-line'></i> Analisis Scatter Plot</h5>",
        "<p><strong>Variabel X:</strong> ", x_var, "</p>",
        "<p><strong>Variabel Y:</strong> ", y_var, "</p>",
        "<p><strong>Korelasi Pearson:</strong> ", round(correlation, 4), "</p>",
        "<p><strong>Interpretasi:</strong> Terdapat hubungan <span style='color: ",
        if(abs(correlation) >= 0.4) "green" else "orange", ";'><strong>", strength, "</strong></span> ",
        "dan bersifat <strong>", direction, "</strong> antara kedua variabel.</p>",
        "<p><strong>Jumlah observasi valid:</strong> ", nrow(clean_data), "</p>"
      )
      
      # Tambahan interpretasi praktis
      if(abs(correlation) >= 0.6) {
        interpretation <- paste0(interpretation,
                                 "<div style='background: #d4edda; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
                                 "<i class='fa fa-lightbulb'></i> <strong>Insight:</strong> ",
                                 "Korelasi yang kuat menunjukkan bahwa kedua variabel memiliki hubungan yang signifikan. ",
                                 if(correlation > 0) {
                                   paste0("Ketika ", x_var, " meningkat, ", y_var, " cenderung meningkat juga.")
                                 } else {
                                   paste0("Ketika ", x_var, " meningkat, ", y_var, " cenderung menurun.")
                                 },
                                 "</div>"
        )
      }
      
      return(interpretation)
      
    } else {
      # Interpretasi untuk histogram dan boxplot
      var_name <- if(is.null(input$vis_var)) {
        numeric_vars <- names(data)[sapply(data, is.numeric)]
        if(length(numeric_vars) > 0) numeric_vars[1] else NULL
      } else {
        input$vis_var
      }
      
      if(is.null(var_name) || !var_name %in% names(data)) {
        return("<p style='color: red;'>Variabel tidak ditemukan dalam data.</p>")
      }
      
      # Analisis statistik deskriptif
      analysis <- analyze_variable(data, var_name)
      
      interpretation <- paste0(
        "<h5><i class='fa fa-chart-bar'></i> Analisis ", plot_type, "</h5>",
        "<p><strong>Variabel:</strong> ", var_name, "</p>",
        "<p><strong>Jumlah observasi:</strong> ", analysis$n, "</p>",
        "<p><strong>Mean:</strong> ", round(analysis$mean, 2), "</p>",
        "<p><strong>Median:</strong> ", round(analysis$median, 2), "</p>",
        "<p><strong>Standar Deviasi:</strong> ", round(analysis$sd, 2), "</p>"
      )
      
      if(!is.na(analysis$cv)) {
        interpretation <- paste0(interpretation,
                                 "<p><strong>Coefficient of Variation:</strong> ", round(analysis$cv, 2), "%</p>"
        )
      }
      
      if(!is.na(analysis$outliers)) {
        interpretation <- paste0(interpretation,
                                 "<p><strong>Outliers:</strong> ", analysis$outliers, " observasi</p>"
        )
      }
      
      # Interpretasi normalitas
      if(!is.na(analysis$normal)) {
        interpretation <- paste0(interpretation,
                                 "<p><strong>Normalitas (Shapiro-Wilk):</strong> ",
                                 if(analysis$normal) "Data terdistribusi normal" else "Data tidak terdistribusi normal",
                                 " (p = ", round(analysis$shapiro_p, 4), ")</p>"
        )
      }
      
      return(interpretation)
    }
    
  }, error = function(e) {
    return(paste0("<p style='color: red;'>Error dalam interpretasi: ", e$message, "</p>"))
  })
})


# Render heatmap korelasi
output$correlation_heatmap <- renderPlotly({
  req(processed_data$current)
  
  tryCatch({
    data <- processed_data$current
    
    # Validasi data
    if(is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Periksa variabel numerik
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    if(length(numeric_vars) < 2) {
      showNotification("Minimal 2 variabel numerik diperlukan untuk heatmap korelasi", type = "warning")
      return(NULL)
    }
    
    p <- create_correlation_heatmap(data)
    if(is.null(p)) return(NULL)
    ggplotly(p)
    
  }, error = function(e) {
    showNotification(paste("Error creating correlation heatmap:", e$message), type = "error")
    return(NULL)
  })
})

# Interpretasi korelasi
output$correlation_interpretation <- renderUI({
  req(processed_data$current)
  
  # Ambil hanya variabel numerik
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  cor_data <- processed_data$current[numeric_vars]
  
  # Hapus kolom dengan variance 0
  cor_data <- cor_data[sapply(cor_data, function(x) var(x, na.rm = TRUE) > 0)]
  
  if(ncol(cor_data) < 2) {
    return(HTML("<p>Tidak cukup variabel numerik untuk analisis korelasi.</p>"))
  }
  
  # Hitung matriks korelasi
  cor_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
  
  # Analisis korelasi
  # Ekstrak korelasi (exclude diagonal)
  cor_values <- cor_matrix[upper.tri(cor_matrix)]
  cor_pairs <- which(upper.tri(cor_matrix), arr.ind = TRUE)
  
  # Buat dataframe untuk analisis
  cor_analysis <- data.frame(
    var1 = rownames(cor_matrix)[cor_pairs[,1]],
    var2 = colnames(cor_matrix)[cor_pairs[,2]],
    correlation = cor_values,
    abs_correlation = abs(cor_values)
  )
  
  # Urutkan berdasarkan korelasi absolut
  cor_analysis <- cor_analysis[order(cor_analysis$abs_correlation, decreasing = TRUE),]
  
  # Identifikasi korelasi terkuat dan terlemah
  strongest_pos <- cor_analysis[which.max(cor_analysis$correlation),]
  strongest_neg <- cor_analysis[which.min(cor_analysis$correlation),]
  strongest_abs <- cor_analysis[1,]
  weakest_abs <- cor_analysis[nrow(cor_analysis),]
  
  # Kategorisasi kekuatan korelasi
  very_strong <- sum(cor_analysis$abs_correlation >= 0.8)
  strong <- sum(cor_analysis$abs_correlation >= 0.6 & cor_analysis$abs_correlation < 0.8)
  moderate <- sum(cor_analysis$abs_correlation >= 0.4 & cor_analysis$abs_correlation < 0.6)
  weak <- sum(cor_analysis$abs_correlation >= 0.2 & cor_analysis$abs_correlation < 0.4)
  very_weak <- sum(cor_analysis$abs_correlation < 0.2)
  
  
  # Generate HTML output
  HTML(paste0(
    "<div style='padding: 10px;'>",
    "<h4 style='color: #2c3e50; margin-bottom: 15px;'>Analisis Korelasi Lengkap</h4>",
    
    # Ringkasan umum
    "<div style='background: #e8f6f3; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
    "<strong>Ringkasan Korelasi:</strong><br>",
    "• Total pasangan variabel: ", nrow(cor_analysis), "<br>",
    "• Korelasi sangat kuat (≥0.8): ", very_strong, "<br>",
    "• Korelasi kuat (0.6-0.8): ", strong, "<br>",
    "• Korelasi sedang (0.4-0.6): ", moderate, "<br>",
    "• Korelasi lemah (0.2-0.4): ", weak, "<br>",
    "• Korelasi sangat lemah (<0.2): ", very_weak,
    "</div>",
    
    # Korelasi positif dan negatif terkuat
    "<div style='background: #e8f8f5; padding: 10px; border-radius: 5px; margin-bottom: 10px;'>",
    "<strong>Ekstrem Korelasi:</strong><br>",
    "• <span style='color: #27ae60;'>Positif terkuat:</span> ", strongest_pos$var1, " ↔ ", strongest_pos$var2, " (", round(strongest_pos$correlation, 3), ")<br>",
    "• <span style='color: #e74c3c;'>Negatif terkuat:</span> ", strongest_neg$var1, " ↔ ", strongest_neg$var2, " (", round(strongest_neg$correlation, 3), ")",
    "</div>",
    
    # Interpretasi dan rekomendasi
    "<div style='background: #fff3cd; padding: 10px; border-radius: 5px;'>",
    "<strong>Interpretasi:</strong><br>",
    if(very_strong > 0) {
      "• <span style='color: #856404;'>Perhatian:</span> Ada korelasi sangat kuat yang mungkin menunjukkan multikolinearitas<br>"
    } else "",
    if(strong + very_strong >= 3) {
      "• Banyak variabel berkorelasi kuat - pertimbangkan analisis faktor<br>"
    } else "",
    "• Gunakan korelasi >0.6 untuk identifikasi redundansi variabel<br>",
    "• Korelasi <0.3 menunjukkan variabel relatif independen<br>",
    "• Pertimbangkan transformasi untuk korelasi non-linear",
    "</div>",
    "</div>"
  ))
})