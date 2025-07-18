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

# Output untuk rata-rata kemiskinan - BARU: MENGGANTIKAN ISLAND DISTRIBUTION
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

# Output untuk ringkasan geografis
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

# Output untuk distribusi kemiskinan per pulau
output$poverty_by_island <- renderPlot({
  data <- sovi_data()
  if (!is.null(data) && "POVERTY" %in% names(data) && "ISLAND_GROUP" %in% names(data)) {
    tryCatch({
      # Hapus missing values
      clean_data <- data[!is.na(data$POVERTY) & !is.na(data$ISLAND_GROUP), ]
      
      if (nrow(clean_data) > 0) {
        # Hitung rata-rata kemiskinan per pulau
        island_poverty <- aggregate(clean_data$POVERTY, 
                                    by = list(Island = clean_data$ISLAND_GROUP), 
                                    FUN = mean, na.rm = TRUE)
        names(island_poverty) <- c("Island", "Avg_Poverty")
        island_poverty <- island_poverty[order(island_poverty$Avg_Poverty, decreasing = TRUE), ]
        
        # Buat plot
        ggplot(island_poverty, aes(x = reorder(Island, Avg_Poverty), y = Avg_Poverty)) +
          geom_col(fill = "steelblue", alpha = 0.7) +
          geom_text(aes(label = paste0(round(Avg_Poverty, 1), "%")), 
                    hjust = -0.1, size = 3) +
          coord_flip() +
          labs(
            title = "Rata-rata Tingkat Kemiskinan per Kelompok Pulau",
            x = "Kelompok Pulau",
            y = "Rata-rata Kemiskinan (%)"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            axis.text = element_text(size = 10)
          )
      } else {
        ggplot() + 
          annotate("text", x = 0, y = 0, label = "Data tidak tersedia", size = 5) +
          theme_void()
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0, y = 0, label = "Error creating plot", size = 5) +
        theme_void()
    })
  } else {
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "Data sedang dimuat...", size = 5) +
      theme_void()
  }
})

# Output untuk distribusi kemiskinan per pulau - DIKEMBALIKAN
output$poverty_by_island <- renderPlot({
  data <- sovi_data()
  if (!is.null(data) && "POVERTY" %in% names(data) && "ISLAND_GROUP" %in% names(data)) {
    tryCatch({
      # Hapus missing values
      clean_data <- data[!is.na(data$POVERTY) & !is.na(data$ISLAND_GROUP), ]
      
      if (nrow(clean_data) > 0) {
        # Hitung rata-rata kemiskinan per pulau
        island_poverty <- aggregate(clean_data$POVERTY, 
                                    by = list(Island = clean_data$ISLAND_GROUP), 
                                    FUN = mean, na.rm = TRUE)
        names(island_poverty) <- c("Island", "Avg_Poverty")
        island_poverty <- island_poverty[order(island_poverty$Avg_Poverty, decreasing = TRUE), ]
        
        # Buat plot
        ggplot(island_poverty, aes(x = reorder(Island, Avg_Poverty), y = Avg_Poverty)) +
          geom_col(fill = "steelblue", alpha = 0.7) +
          geom_text(aes(label = paste0(round(Avg_Poverty, 1), "%")), 
                    hjust = -0.1, size = 3) +
          coord_flip() +
          labs(
            title = "Rata-rata Tingkat Kemiskinan per Kelompok Pulau",
            x = "Kelompok Pulau",
            y = "Rata-rata Kemiskinan (%)"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            axis.text = element_text(size = 10)
          )
      } else {
        ggplot() + 
          annotate("text", x = 0, y = 0, label = "Data tidak tersedia", size = 5) +
          theme_void()
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0, y = 0, label = "Error creating plot", size = 5) +
        theme_void()
    })
  } else {
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "Data sedang dimuat...", size = 5) +
      theme_void()
  }
})

# Output untuk distribusi populasi per region
output$population_by_region <- renderPlot({
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
        
        # Buat plot pie
        ggplot(region_pop, aes(x = "", y = Population_Million, fill = Region)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          geom_text(aes(label = paste0(Region, "\n", round(Population_Million, 1), "M")), 
                    position = position_stack(vjust = 0.5), size = 3) +
          labs(
            title = "Distribusi Populasi per Wilayah Indonesia",
            fill = "Wilayah"
          ) +
          theme_void() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
            legend.position = "bottom"
          ) +
          scale_fill_brewer(type = "qual", palette = "Set2")
      } else {
        ggplot() + 
          annotate("text", x = 0, y = 0, label = "Data tidak tersedia", size = 5) +
          theme_void()
      }
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0, y = 0, label = "Error creating plot", size = 5) +
        theme_void()
    })
  } else {
    ggplot() + 
      annotate("text", x = 0, y = 0, label = "Data sedang dimuat...", size = 5) +
      theme_void()
  }
})

# Handler untuk tombol "Panduan Pengguna"
observeEvent(input$goto_guide, {
  updateTabItems(session, "sidebar", "user_guide")
})