indonesia_geojson <- reactive({
  geojson_path <- "Administrasi_Kabupaten_ind.geojson" 
  
  if (!file.exists(geojson_path)) {
    showNotification(
      paste("KRITIS: File peta tidak ditemukan:", geojson_path),
      type = "error", 
      duration = NULL
    )
    return(NULL)
  }
  
  geojson_data <- tryCatch({
    # Gunakan geojsonio langsung (lebih kompatibel dengan leaflet)
    sp_data <- geojsonio::geojson_read(geojson_path, what = "sp")
    
    # Pastikan CRS
    if (is.na(sp::proj4string(sp_data))) {
      sp::proj4string(sp_data) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    }
    
    cat("GeoJSON loaded successfully with", length(sp_data), "features\n")
    cat("CRS:", sp::proj4string(sp_data), "\n")
    
    return(sp_data)
    
  }, error = function(e) {
    showNotification(paste("Gagal membaca file GeoJSON:", e$message), type = "error", duration = NULL)
    return(NULL)
  })
  
  return(geojson_data)
})

# 2. UI PEMILIH VARIABEL
output$map_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  numeric_vars <- numeric_vars[!numeric_vars %in% c("DISTRICTCODE")]
  
  selectInput("map_var", "Pilih Variabel untuk Dipetakan:", 
              choices = numeric_vars, 
              selected = if("POVERTY" %in% numeric_vars) "POVERTY" else numeric_vars[1])
})

# 2b. REACTIVE VALUES UNTUK MENYIMPAN DATA KLIK
map_click_data <- reactiveValues(
  selected_district = NULL,
  district_data = NULL,
  show_stats = FALSE
)

# 3. LOGIKA UNTUK MERENDER PETA - PENDEKATAN SIMPLE
output$distribution_map <- renderLeaflet({
  req(processed_data$current, indonesia_geojson(), input$map_var)
  
  sovi_data_current <- processed_data$current
  geojson <- indonesia_geojson()
  variable_to_map <- input$map_var
  
  if (is.null(geojson)) {
    return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 4))
  }
  
  cat("=== SIMPLE DEBUG ===\n")
  cat("Data SOVI rows:", nrow(sovi_data_current), "\n")
  cat("GeoJSON class:", class(geojson), "\n")
  cat("GeoJSON@data columns:", paste(names(geojson@data), collapse = ", "), "\n")
  
  # Cari kolom ID
  id_col <- NULL
  for (col in c("ID", "KODE", "CODE", "id", "kode")) {
    if (col %in% names(geojson@data)) {
      id_col <- col
      break
    }
  }
  
  if (is.null(id_col)) {
    return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 4))
  }
  
  cat("Using ID column:", id_col, "\n")
  
  # Standarisasi format
  sovi_data_current$DISTRICTCODE <- as.character(sovi_data_current$DISTRICTCODE)
  geojson@data[[id_col]] <- as.character(geojson@data[[id_col]])
  
  # Merge menggunakan sp
  merged_data <- sp::merge(geojson, sovi_data_current, 
                           by.x = id_col, by.y = "DISTRICTCODE", 
                           all.x = FALSE)
  
  cat("Merged SP data rows:", nrow(merged_data@data), "\n")
  
  if (nrow(merged_data@data) == 0) {
    return(leaflet() %>% 
             addTiles() %>% 
             setView(lng = 118, lat = -2, zoom = 4) %>%
             addMarkers(lng = 118, lat = -2, popup = "Tidak ada data yang cocok"))
  }
  
  # Ekstrak nilai variabel
  map_values <- as.numeric(merged_data@data[[variable_to_map]])
  
  if (all(is.na(map_values))) {
    return(leaflet() %>% addTiles() %>% setView(lng = 118, lat = -2, zoom = 4))
  }
  
  # Buat palette sederhana
  pal <- colorNumeric(
    palette = c("#FFF7EC", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000"),
    domain = range(map_values, na.rm = TRUE),
    na.color = "#808080"
  )
  
  cat("Palette domain:", range(map_values, na.rm = TRUE), "\n")
  
  # Labels sederhana
  labels <- sprintf(
    "<strong>%s</strong><br/>%s: %.2f",
    merged_data@data[[id_col]],
    variable_to_map,
    map_values
  ) %>% lapply(htmltools::HTML)
  
  # RENDER PETA DENGAN CARA TRADISIONAL (PALING STABIL)
  leaflet_map <- leaflet(merged_data) %>%
    addTiles() %>%
    setView(lng = 118, lat = -2, zoom = 5) %>%
    addPolygons(
      fillColor = ~pal(merged_data@data[[variable_to_map]]),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      # TAMBAHAN: Layer ID untuk identifikasi saat diklik
      layerId = ~merged_data@data[[id_col]]
    ) %>%
    addLegend(
      pal = pal, 
      values = ~merged_data@data[[variable_to_map]], 
      opacity = 0.7, 
      title = variable_to_map,
      position = "bottomright"
    )
  
  cat("Map rendered successfully\n")
  return(leaflet_map)
})

# 4. OBSERVER UNTUK MENANGKAP KLIK PADA PETA
observe({
  req(input$distribution_map_shape_click)
  
  click <- input$distribution_map_shape_click
  clicked_id <- click$id
  
  cat("Clicked district ID:", clicked_id, "\n")
  
  # Ambil data untuk district yang diklik
  sovi_data_current <- processed_data$current
  district_info <- sovi_data_current[sovi_data_current$DISTRICTCODE == clicked_id, ]
  
  if (nrow(district_info) > 0) {
    map_click_data$selected_district <- clicked_id
    map_click_data$district_data <- district_info
    map_click_data$show_stats <- TRUE
    
    cat("District data loaded for:", clicked_id, "\n")
  }
})

# 5. OUTPUT UNTUK INFORMASI DISTRICT YANG DIKLIK
output$district_info_panel <- renderUI({
  if (!map_click_data$show_stats || is.null(map_click_data$district_data)) {
    return(
      div(
        style = "text-align: center; padding: 20px; color: #666;",
        icon("mouse-pointer", style = "font-size: 2em; color: #3498db;"),
        h4("Klik pada peta untuk melihat statistik"),
        p("Pilih kabupaten/kota di peta untuk menampilkan informasi detail")
      )
    )
  }
  
  district_data <- map_click_data$district_data
  district_id <- map_click_data$selected_district
  
  # Ambil semua data untuk perbandingan
  all_data <- processed_data$current
  
  # Siapkan statistik
  numeric_vars <- names(all_data)[sapply(all_data, is.numeric)]
  numeric_vars <- numeric_vars[!numeric_vars %in% c("DISTRICTCODE")]
  
  # Panel informasi district
  div(
    # Header
    div(
      style = "background: linear-gradient(135deg, #B30000 0%, #FDD49E 100%); 
               color: white; padding: 15px; border-radius: 8px 8px 0 0; margin-bottom: 15px;",
      h3(style = "margin: 0; text-align: center;", 
         paste("Kabupaten/Kota:", district_id)),
      p(style = "margin: 5px 0 0 0; text-align: center; font-size: 14px; opacity: 0.9;",
        "Statistik Deskriptif Kerentanan Sosial")
    ),
    
    # Konten statistik
    div(
      style = "padding: 0 15px;",
      
      # Variabel yang sedang dipetakan
      if (!is.null(input$map_var)) {
        current_var <- input$map_var
        current_value <- as.numeric(district_data[[current_var]])
        
        # Hitung persentil
        all_values <- as.numeric(all_data[[current_var]])
        percentile <- round(100 * sum(all_values <= current_value, na.rm = TRUE) / sum(!is.na(all_values)), 1)
        
        div(
          style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #007bff;",
          h4(style = "color: #007bff; margin-top: 0;", "Variabel Terpilih"),
          p(strong(current_var), ": ", 
            span(style = "font-size: 18px; color: #28a745; font-weight: bold;", 
                 round(current_value, 2))),
          p("Persentil: ", 
            span(style = paste0("color: ", if(percentile > 75) "#dc3545" else if(percentile > 50) "#ffc107" else "#28a745", "; font-weight: bold;"), 
                 paste0(percentile, "%"))),
          p(style = "font-size: 12px; color: #6c757d; margin: 0;",
            "Nilai ini berada di persentil ", percentile, " dari semua kabupaten/kota")
        )
      },
      
      # Tabel semua indikator
      h4("Semua Indikator Kerentanan"),
      div(
        style = "max-height: 400px; overflow-y: auto; border: 1px solid #dee2e6; border-radius: 8px;",
        DT::renderDataTable({
          # Siapkan data untuk tabel
          stats_table <- data.frame(
            Indikator = character(),
            Nilai = numeric(),
            Persentil = character(),
            Status = character(),
            stringsAsFactors = FALSE
          )
          
          for (var in numeric_vars) {
            value <- as.numeric(district_data[[var]])
            all_vals <- as.numeric(all_data[[var]])
            pct <- round(100 * sum(all_vals <= value, na.rm = TRUE) / sum(!is.na(all_vals)), 1)
            
            status <- if(pct > 75) "Tinggi" else if(pct > 50) "Sedang" else "Rendah"
            
            stats_table <- rbind(stats_table, data.frame(
              Indikator = var,
              Nilai = round(value, 2),
              Persentil = paste0(pct, "%"),
              Status = status,
              stringsAsFactors = FALSE
            ))
          }
          
          DT::datatable(
            stats_table,
            options = list(
              pageLength = 10,
              scrollY = "300px",
              scrollCollapse = TRUE,
              dom = 't',
              columnDefs = list(list(className = 'dt-center', targets = 1:3))
            ),
            rownames = FALSE,
            escape = FALSE
          ) %>%
            DT::formatStyle(
              'Status',
              backgroundColor = DT::styleEqual(
                c('Rendah', 'Sedang', 'Tinggi'),
                c('#d4edda', '#fff3cd', '#f8d7da')
              )
            )
        }, server = FALSE)
      ),
      
      # Rangkuman
      div(
        style = "margin-top: 20px; padding: 15px; background: #e9ecef; border-radius: 8px;",
        h5("Rangkuman"),
        p("Kabupaten/kota ini memiliki karakteristik kerentanan sosial yang bervariasi. 
          Gunakan informasi di atas untuk analisis lebih lanjut."),
        actionButton("close_stats", "Tutup", 
                     style = "background: #6c757d; color: white; border: none; padding: 8px 16px; border-radius: 4px;")
      )
    )
  )
})

# 6. OBSERVER UNTUK MENUTUP PANEL STATISTIK
observeEvent(input$close_stats, {
  map_click_data$show_stats <- FALSE
  map_click_data$selected_district <- NULL
  map_click_data$district_data <- NULL
})