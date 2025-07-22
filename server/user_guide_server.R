# server/user_guide_server.R

#  NAVIGASI CEPAT 
# Handler untuk tombol Data (Manajemen Data)
observeEvent(input$guide_nav_data, {
  updateTabItems(session, "sidebar", "data_management")
  showNotification("Navigasi ke Manajemen Data", type = "message", duration = 2)
})

# Handler untuk tombol Eksplorasi (Statistik Deskriptif)
observeEvent(input$guide_nav_explore, {
  updateTabItems(session, "sidebar", "descriptive")
  showNotification("Navigasi ke Eksplorasi Data", type = "message", duration = 2)
})

# Handler untuk tombol Asumsi (Uji Asumsi)
observeEvent(input$guide_nav_assumptions, {
  updateTabItems(session, "sidebar", "assumptions")
  showNotification("Navigasi ke Uji Asumsi", type = "message", duration = 2)
})

# Handler untuk tombol Inferensia (Statistik Inferensia - t-Test)
observeEvent(input$guide_nav_inference, {
  updateTabItems(session, "sidebar", "ttest")
  showNotification("Navigasi ke Statistik Inferensia", type = "message", duration = 2)
})

# Handler untuk tombol Regresi (Regresi Linear)
observeEvent(input$guide_nav_regression, {
  updateTabItems(session, "sidebar", "regression")
  showNotification("Navigasi ke Analisis Regresi", type = "message", duration = 2)
})

# Handler untuk tombol Download (Download Center)
observeEvent(input$guide_nav_download, {
  updateTabItems(session, "sidebar", "download")
  showNotification("Navigasi ke Download Center", type = "message", duration = 2)
})

#  KONTEN DINAMIS PANDUAN 
# Render konten dinamis berdasarkan tab yang aktif
output$guide_content <- renderUI({
  # Konten default atau dinamis bisa ditambahkan di sini
  # Untuk saat ini, konten sudah hard-coded di UI
  return(NULL)
})

#  NOTIFIKASI BANTUAN 
# Observer untuk memberikan tips kontekstual
observe({
  # Tips berdasarkan tab yang sedang aktif di sidebar
  current_tab <- input$sidebar
  
  if (!is.null(current_tab) && current_tab == "user_guide") {
    # Bisa ditambahkan tips khusus untuk tab panduan
  }
})

#  LOGGING NAVIGASI 
# Optional: Log navigasi untuk analisis penggunaan
navigation_log <- reactiveValues(
  history = character(0),
  timestamps = as.POSIXct(character(0))
)

# Function untuk log navigasi
log_navigation <- function(destination) {
  navigation_log$history <- c(navigation_log$history, destination)
  navigation_log$timestamps <- c(navigation_log$timestamps, Sys.time())
  
  # Keep only last 50 entries to prevent memory issues
  if (length(navigation_log$history) > 50) {
    navigation_log$history <- tail(navigation_log$history, 50)
    navigation_log$timestamps <- tail(navigation_log$timestamps, 50)
  }
}

# Update semua observeEvent untuk include logging
observeEvent(input$guide_nav_data, {
  log_navigation("data_management")
}, ignoreInit = TRUE)

observeEvent(input$guide_nav_explore, {
  log_navigation("descriptive")
}, ignoreInit = TRUE)

observeEvent(input$guide_nav_assumptions, {
  log_navigation("assumptions")
}, ignoreInit = TRUE)

observeEvent(input$guide_nav_inference, {
  log_navigation("ttest")
}, ignoreInit = TRUE)

observeEvent(input$guide_nav_regression, {
  log_navigation("regression")
}, ignoreInit = TRUE)

observeEvent(input$guide_nav_download, {
  log_navigation("download")
}, ignoreInit = TRUE)