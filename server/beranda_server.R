# server/beranda_server.R - Versi yang Diperbaiki

# Output untuk total districts
output$total_districts <- renderText({
  data <- sovi_data()
  if (!is.null(data)) {
    nrow(data)
  } else {
    "Loading..."
  }
})

# Output untuk total population
output$total_population <- renderText({
  data <- sovi_data()
  if (!is.null(data) && "POPULATION" %in% names(data)) {
    total_pop <- sum(data$POPULATION, na.rm = TRUE)
    paste(round(total_pop / 1000000, 1), "juta")
  } else {
    "Loading..."
  }
})

# Output untuk total provinsi
output$total_provinces <- renderText({
  data <- sovi_data()
  if (!is.null(data) && "PROVINCE_NAME" %in% names(data)) {
    length(unique(data$PROVINCE_NAME[!is.na(data$PROVINCE_NAME)]))
  } else {
    "Loading..."
  }
})

# Output untuk rata-rata kemiskinan
output$average_poverty <- renderText({
  data <- sovi_data()
  if (!is.null(data) && "POVERTY" %in% names(data)) {
    avg_poverty <- mean(data$POVERTY, na.rm = TRUE)
    paste0(round(avg_poverty, 1), "%")
  } else {
    "Loading..."
  }
})

# Output untuk tabel informasi variabel
output$variable_info_table <- DT::renderDataTable({
  metadata <- get_variable_metadata()
  
  # Konversi metadata ke data frame
  var_info <- data.frame(
    Variabel = names(metadata),
    Nama = sapply(metadata, function(x) x$name),
    Deskripsi = sapply(metadata, function(x) x$description),
    Tipe = sapply(metadata, function(x) x$type),
    Satuan = sapply(metadata, function(x) ifelse(is.null(x$unit), "-", x$unit)),
    stringsAsFactors = FALSE
  )
  
  DT::datatable(
    var_info,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'frtip'
    ),
    rownames = FALSE
  )
}, server = FALSE)

# PERBAIKAN: Output untuk pie chart distribusi populasi menggunakan renderPlotly
output$population_pie_chart <- renderPlotly({
  data <- sovi_data()
  if (!is.null(data) && "POPULATION" %in% names(data) && "REGION" %in% names(data)) {
    tryCatch({
      # Hapus missing values
      clean_data <- data[!is.na(data$POPULATION) & !is.na(data$REGION), ]
      
      if (nrow(clean_data) > 0) {
        # Hitung total populasi per region
        region_pop <- aggregate(clean_data$POPULATION,
                                by = list(Region = clean_data$REGION),
                                FUN = sum, na.rm = TRUE)
        names(region_pop) <- c("Region", "Total_Population")
        region_pop$Population_Million <- region_pop$Total_Population / 1000000
        region_pop$Percentage <- round((region_pop$Total_Population / sum(region_pop$Total_Population)) * 100, 1)
        
        # Buat pie chart dengan plotly
        plot_ly(
          data = region_pop,
          labels = ~Region,
          values = ~Population_Million,
          type = 'pie',
          textinfo = 'label+percent',
          textposition = 'auto',
          hovertemplate = paste(
            '<b>%{label}</b><br>',
            'Populasi: %{value:.1f} juta<br>',
            'Persentase: %{percent}<br>',
            '<extra></extra>'
          ),
          marker = list(
            colors = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2'),
            line = list(color = '#FFFFFF', width = 2)
          )
        ) %>%
          layout(
            title = list(
              text = "Distribusi Populasi per Wilayah Indonesia",
              font = list(size = 16, family = "Arial", color = "#2c3e50")
            ),
            showlegend = TRUE,
            legend = list(
              orientation = "h",
              x = 0.5,
              xanchor = 'center',
              y = -0.1
            ),
            margin = list(l = 50, r = 50, t = 80, b = 100),
            paper_bgcolor = 'rgba(0,0,0,0)',
            plot_bgcolor = 'rgba(0,0,0,0)'
          )
      } else {
        # Fallback untuk data kosong
        plot_ly() %>%
          add_annotations(
            text = "Data tidak tersedia",
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, color = "#7f8c8d")
          ) %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            paper_bgcolor = 'rgba(0,0,0,0)',
            plot_bgcolor = 'rgba(0,0,0,0)'
          )
      }
    }, error = function(e) {
      # Error handling
      plot_ly() %>%
        add_annotations(
          text = paste("Error:", e$message),
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 14, color = "#e74c3c")
        ) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    })
  } else {
    # Loading state
    plot_ly() %>%
      add_annotations(
        text = "Data sedang dimuat...",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, color = "#3498db")
      ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        paper_bgcolor = 'rgba(0,0,0,0)',
        plot_bgcolor = 'rgba(0,0,0,0)'
      )
  }
})

# BARU: Output untuk statistik cepat - VERSI LENGKAP DENGAN BOX YANG DIPERPANJANG
output$quick_stats <- renderText({
  data <- sovi_data()
  if (!is.null(data)) {
    tryCatch({
      stats_text <- "=== STATISTIK CEPAT ===\n\n"
      
      # Total kabupaten/kota
      stats_text <- paste0(stats_text, sprintf("📍 Total Kabupaten/Kota: %d\n", nrow(data)))
      
      # Total provinsi
      if ("PROVINCE_NAME" %in% names(data)) {
        n_provinces <- length(unique(data$PROVINCE_NAME[!is.na(data$PROVINCE_NAME)]))
        stats_text <- paste0(stats_text, sprintf("🗺️  Total Provinsi: %d\n", n_provinces))
      }
      
      # Total populasi
      if ("POPULATION" %in% names(data)) {
        total_pop <- sum(data$POPULATION, na.rm = TRUE)
        stats_text <- paste0(stats_text, sprintf("👥 Total Populasi: %.1f juta jiwa\n", total_pop / 1000000))
      }
      
      # Rata-rata kemiskinan
      if ("POVERTY" %in% names(data)) {
        avg_poverty <- mean(data$POVERTY, na.rm = TRUE)
        stats_text <- paste0(stats_text, sprintf("💰 Rata-rata Kemiskinan: %.1f%%\n", avg_poverty))
      }
      
      stats_text <- paste0(stats_text, "\n=== DISTRIBUSI WILAYAH ===\n")
      
      # Distribusi per region
      if ("REGION" %in% names(data)) {
        region_counts <- table(data$REGION[!is.na(data$REGION)])
        stats_text <- paste0(stats_text, "\n🌏 Per Wilayah:\n")
        for (region in names(region_counts)) {
          percentage <- round((region_counts[region] / nrow(data)) * 100, 1)
          stats_text <- paste0(stats_text, sprintf("  • %s: %d kabupaten/kota (%.1f%%)\n",
                                                   region, region_counts[region], percentage))
        }
      }
      
      # Distribusi per pulau (jika ada)
      if ("ISLAND_GROUP" %in% names(data)) {
        island_counts <- table(data$ISLAND_GROUP[!is.na(data$ISLAND_GROUP)])
        island_counts <- sort(island_counts, decreasing = TRUE)
        stats_text <- paste0(stats_text, "\n🏝️  Top 3 Kelompok Pulau:\n")
        top_islands <- head(island_counts, 3)
        for (i in 1:length(top_islands)) {
          percentage <- round((top_islands[i] / nrow(data)) * 100, 1)
          stats_text <- paste0(stats_text, sprintf("  %d. %s: %d kabupaten/kota (%.1f%%)\n",
                                                   i, names(top_islands)[i], top_islands[i], percentage))
        }
      }
      
      # Statistik variabel utama
      stats_text <- paste0(stats_text, "\n=== VARIABEL UTAMA ===\n")
      
      numeric_vars <- c("POVERTY", "POPULATION", "UNEMPLOYMENT", "NO_DIPLOMA", "AGED_65_PLUS",
                        "AGED_17_UNDER", "DISABLED", "SINGLE_PARENT", "MINORITY", "CROWDING",
                        "NO_VEHICLE", "GROUP_QUARTERS", "MOBILE_HOMES", "MULTI_UNIT",
                        "MEDIAN_HOME_VALUE", "MEDIAN_RENT", "MEDIAN_HH_INCOME", "PER_CAPITA_INCOME",
                        "NO_PLUMBING")
      
      available_vars <- intersect(numeric_vars, names(data))
      if (length(available_vars) > 0) {
        for (var in head(available_vars, 6)) { # Tampilkan 6 variabel karena box lebih panjang
          if (var %in% names(data)) {
            var_mean <- mean(data[[var]], na.rm = TRUE)
            var_name <- switch(var,
                               "POVERTY" = "Kemiskinan",
                               "POPULATION" = "Populasi",
                               "UNEMPLOYMENT" = "Pengangguran",
                               "NO_DIPLOMA" = "Tanpa Diploma",
                               "AGED_65_PLUS" = "Usia 65+",
                               "AGED_17_UNDER" = "Usia <17",
                               var
            )
            
            unit <- if(var == "POPULATION") " jiwa" else if(var %in% c("POVERTY", "UNEMPLOYMENT", "NO_DIPLOMA", "AGED_65_PLUS", "AGED_17_UNDER")) "%" else ""
            
            if (var == "POPULATION") {
              stats_text <- paste0(stats_text, sprintf("  • %s: %.0f%s\n", var_name, var_mean, unit))
            } else {
              stats_text <- paste0(stats_text, sprintf("  • %s: %.1f%s\n", var_name, var_mean, unit))
            }
          }
        }
      }
      
      stats_text <- paste0(stats_text, sprintf("\n📊 Terakhir diperbarui: %s", format(Sys.time(), "%Y-%m-%d %H:%M")))
      
      return(stats_text)
      
    }, error = function(e) {
      return(paste("Error generating statistics:", e$message))
    })
  } else {
    return("Data sedang dimuat...")
  }
})

# --- PERBAIKAN PADA TABEL TOP 5 POPULASI ---
output$top_population_table <- DT::renderDataTable({
  data <- sovi_data()
  if (!is.null(data) && "POPULATION" %in% names(data)) {
    tryCatch({
      ranking_data <- data[!is.na(data$POPULATION), ]
      cols_to_select <- intersect(c("DISTRICT_NAME", "PROVINCE_NAME", "POPULATION"), names(ranking_data))
      
      if (length(cols_to_select) >= 2) {
        top_data <- ranking_data[order(ranking_data$POPULATION, decreasing = TRUE), cols_to_select]
        top_data <- head(top_data, 5)
        top_data$POPULATION <- paste0(round(top_data$POPULATION / 1000000, 2), "M")
        
        # Logika penamaan ulang kolom yang lebih aman
        new_names <- c()
        if ("DISTRICT_NAME" %in% names(top_data)) new_names <- c(new_names, "Kabupaten/Kota")
        if ("PROVINCE_NAME" %in% names(top_data)) new_names <- c(new_names, "Provinsi")
        if ("POPULATION" %in% names(top_data)) new_names <- c(new_names, "Populasi")
        names(top_data) <- new_names
        
        DT::datatable(
          top_data,
          options = list(pageLength = 5, dom = 't', ordering = FALSE, scrollX = FALSE, scrollY = FALSE),
          rownames = FALSE, class = 'cell-border stripe', style = 'bootstrap4'
        ) %>%
          DT::formatStyle(columns = names(top_data), fontSize = '14px', lineHeight = '1.4')
      } else {
        DT::datatable(data.frame(Status = "Data tidak cukup untuk ranking populasi"), options = list(dom = 't'), rownames = FALSE)
      }
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)), options = list(dom = 't'), rownames = FALSE)
    })
  } else {
    DT::datatable(data.frame(Status = "Data sedang dimuat..."), options = list(dom = 't'), rownames = FALSE)
  }
}, server = FALSE)

# --- PERBAIKAN PADA TABEL TOP 5 KEMISKINAN ---
output$top_poverty_table <- DT::renderDataTable({
  data <- sovi_data()
  if (!is.null(data) && "POVERTY" %in% names(data)) {
    tryCatch({
      ranking_data <- data[!is.na(data$POVERTY), ]
      cols_to_select <- intersect(c("DISTRICT_NAME", "PROVINCE_NAME", "POVERTY"), names(ranking_data))
      
      if (length(cols_to_select) >= 2) {
        top_data <- ranking_data[order(ranking_data$POVERTY, decreasing = TRUE), cols_to_select]
        top_data <- head(top_data, 5)
        top_data$POVERTY <- paste0(round(top_data$POVERTY, 1), "%")
        
        # Logika penamaan ulang kolom yang lebih aman
        new_names <- c()
        if ("DISTRICT_NAME" %in% names(top_data)) new_names <- c(new_names, "Kabupaten/Kota")
        if ("PROVINCE_NAME" %in% names(top_data)) new_names <- c(new_names, "Provinsi")
        if ("POVERTY" %in% names(top_data)) new_names <- c(new_names, "Kemiskinan")
        names(top_data) <- new_names
        
        DT::datatable(
          top_data,
          options = list(pageLength = 5, dom = 't', ordering = FALSE, scrollX = FALSE, scrollY = FALSE),
          rownames = FALSE, class = 'cell-border stripe', style = 'bootstrap4'
        ) %>%
          DT::formatStyle(columns = names(top_data), fontSize = '14px', lineHeight = '1.4')
      } else {
        DT::datatable(data.frame(Status = "Data tidak cukup untuk ranking kemiskinan"), options = list(dom = 't'), rownames = FALSE)
      }
    }, error = function(e) {
      DT::datatable(data.frame(Error = paste("Error:", e$message)), options = list(dom = 't'), rownames = FALSE)
    })
  } else {
    DT::datatable(data.frame(Status = "Data sedang dimuat..."), options = list(dom = 't'), rownames = FALSE)
  }
}, server = FALSE)


# Handler untuk tombol "Panduan Pengguna"
observeEvent(input$goto_guide, {
  updateTabItems(session, "sidebar", "user_guide")
})

# Output untuk ringkasan geografis (jika diperlukan)
output$geographic_summary <- renderText({
  data <- sovi_data()
  if (!is.null(data)) {
    if (exists("get_geographic_summary")) {
      get_geographic_summary(data)
    } else {
      "Informasi geografis sedang dimuat..."
    }
  } else {
    "Data belum dimuat"
  }
})