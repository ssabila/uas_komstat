library(bs4Dash)
library(shiny)
library(DT)
library(plotly)
library(leaflet)
library(dplyr)
library(ggplot2)
library(readr)
library(htmltools)
library(shinyjs)
library(here)
library(e1071)
library(reshape2)
library(sf)
library(shinycustomloader)


cat("\n=== Loading Modules ===\n")

module_files <- c(
  "modules/data_module.R",
  "modules/analysis_functions.R", 
  "modules/visualization_functions.R"
)

cat("\n=== Checking Data Files ===\n")
data_files <- c("sovi_data.csv", "distance.csv")
for(data_file in data_files) {
  if(file.exists(data_file)) {
    cat("✓ Found:", data_file, "\n")
  } else {
    cat("✗ Missing:", data_file, "\n")
  }
}


server <- function(input, output, session) {
  useShinyjs()
  
  tryCatch({
    source("modules/data_module.R", local = TRUE)
    source("modules/analysis_functions.R", local = TRUE)
    source("modules/visualization_functions.R", local = TRUE)
  }, error = function(e) {
    cat("Error loading modules:", e$message, "\n")
    showNotification("Error loading modules. Some features may not work.", type = "error")
  })
  
  # Muat data saat aplikasi dimulai
  sovi_data <- reactive({
    tryCatch({
      data <- load_sovi_data()
      if (!is.null(data)) {
        # PERBAIKAN: Check jika fungsi tersedia sebelum memanggil
        if (exists("create_dummy_categorical_variables")) {
          data <- create_dummy_categorical_variables(data)
        } else {
          # Fallback: buat variabel kategori sederhana
          if ("POVERTY" %in% names(data)) {
            poverty_median <- median(data$POVERTY, na.rm = TRUE)
            data$POVERTY_LEVEL <- ifelse(data$POVERTY > poverty_median, "Tinggi", "Rendah")
          }
          
          if ("POPULATION" %in% names(data)) {
            pop_quartiles <- quantile(data$POPULATION, probs = c(0.33, 0.67), na.rm = TRUE)
            data$POPULATION_SIZE <- cut(data$POPULATION, 
                                        breaks = c(-Inf, pop_quartiles[1], pop_quartiles[2], Inf),
                                        labels = c("Kecil", "Sedang", "Besar"),
                                        include.lowest = TRUE)
          }
        }
      }
      return(data)
    }, error = function(e) {
      cat("Error loading SOVI data:", e$message, "\n")
      showNotification("Error loading data. Please check data files.", type = "error")
      return(NULL)
    })
  })
  
  distance_data <- reactive({
    tryCatch({
      load_distance_data()
    }, error = function(e) {
      cat("Error loading distance data:", e$message, "\n")
      return(NULL)
    })
  })
  
  # Inisialisasi reactiveValues untuk menyimpan semua hasil analisis
  analysis_results <- reactiveValues(
    descriptive = NULL,
    normality = NULL,
    homogeneity = NULL,
    ttest = NULL,
    anova = NULL,
    proportion = NULL,
    variance = NULL,  # BARU: Tambahkan variance
    regression = NULL
  )
  
  source_files <- list(
    "server/beranda_server.R",
    "server/user_guide_server.R",
    "server/data_management_server.R", 
    "server/descriptive_server.R",
    "server/visualization_server.R",
    "server/mapping_server.R",
    "server/assumptions_server.R",
    "server/ttest_server.R",
    "server/proportion_server.R",
    "server/variance_server.R", 
    "server/anova_server.R",
    "server/regression_server.R",
    "server/download_server.R"
  )
  
  for (file in source_files) {
    if (file.exists(file)) {
      tryCatch({
        source(file, local = TRUE)
        cat("✓ Loaded:", file, "\n")
      }, error = function(e) {
        cat("✗ Error loading", file, ":", e$message, "\n")
        showNotification(
          paste("Error loading", basename(file), ":", e$message),
          type = "warning",
          duration = 3
        )
      })
    } else {
      cat("✗ File not found:", file, "\n")
      showNotification(
        paste("File not found:", basename(file)),
        type = "warning",
        duration = 3
      )
    }
  }
  
  onStop(function() {
    cat("Shiny session stopped\n")
  })
}

ui <- bs4DashPage(
  # Header
  bs4DashNavbar(
    title = "SOVI Dashboard",
    titleWidth = 400
  ),
  
  # Sidebar
  bs4DashSidebar(
    width = 250,
    bs4SidebarMenu(
      id = "sidebar",
      bs4SidebarMenuItem("Beranda", tabName = "beranda", icon = icon("home")),
      bs4SidebarMenuItem("Panduan Pengguna", tabName = "user_guide", icon = icon("book")),
      bs4SidebarMenuItem("Manajemen Data", tabName = "data_management", icon = icon("database")),
      bs4SidebarMenuItem("Eksplorasi Data", tabName = "exploration", icon = icon("search"),
                         bs4SidebarMenuSubItem("Statistik Deskriptif", tabName = "descriptive"),
                         bs4SidebarMenuSubItem("Visualisasi", tabName = "visualization"),
                         bs4SidebarMenuSubItem("Peta Distribusi", tabName = "mapping")
      ),
      bs4SidebarMenuItem("Uji Asumsi", tabName = "assumptions", icon = icon("check-circle")),
      bs4SidebarMenuItem("Statistik Inferensia", tabName = "inference", icon = icon("chart-line"),
                         bs4SidebarMenuSubItem("Uji t-Test", tabName = "ttest"),
                         bs4SidebarMenuSubItem("Uji Proporsi", tabName = "proportion"),
                         bs4SidebarMenuSubItem("Uji Variance", tabName = "variance"),
                         bs4SidebarMenuSubItem("ANOVA", tabName = "anova")
      ),
      bs4SidebarMenuItem("Regresi Linear", tabName = "regression", icon = icon("line-chart")),
      bs4SidebarMenuItem("Download Center", tabName = "download", icon = icon("download"))
    )
  ),
  
  # Body
  bs4DashBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        /* Basic styling */
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
        .info-box {
          border-radius: 5px;
        }
        .small-box {
          border-radius: 5px;
        }
        
        /* Assumptions page specific styling */
        .assumptions-container {
          padding: 20px;
        }
        
        .assumptions-container .box {
          margin-bottom: 25px !important;
          overflow: visible !important;
        }
        
        .assumptions-container .col-sm-6 {
          padding-left: 10px;
          padding-right: 10px;
        }
        
        /* Plot containers */
        .plot-container {
          margin: 15px 0;
          padding: 15px;
          background: white;
          border-radius: 8px;
          border: 1px solid #dee2e6;
        }
        
        /* Download section styling */
        .download-section {
          margin-top: 20px;
          padding: 20px;
          background: #f8f9fa;
          border-radius: 8px;
          border-top: 3px solid #007bff;
        }
        
        /* Prevent text overflow */
        .verbatim-output {
          word-wrap: break-word;
          overflow-wrap: break-word;
          white-space: pre-wrap;
        }
        
        /* Radio button styling */
        .radio-container {
          margin: 10px 0;
        }
        
        .radio-container .radio {
          margin-bottom: 5px;
        }
        
        /* Button improvements */
        .btn-lg {
          padding: 12px 24px;
          font-size: 16px;
          border-radius: 6px;
        }
        
        /* Box height consistency */
        .equal-height-box {
          display: flex;
          flex-direction: column;
        }
        
        .equal-height-box .box-body {
          flex-grow: 1;
        }
        
        /* Responsive adjustments */
        @media (max-width: 768px) {
          .assumptions-container .col-sm-6 {
            margin-bottom: 30px;
            padding: 0 15px;
          }
          
          .assumptions-container .box {
            margin-bottom: 20px !important;
          }
        }
        
        /* Additional fixes for better spacing */
        .content-wrapper {
          min-height: 100vh;
        }
        
        .tab-content {
          padding: 20px;
        }
        
        /* Fix for sidebar and content alignment */
        .main-sidebar {
          position: fixed;
        }
        
        /* Ensure proper box spacing across all pages */
        .box + .box {
          margin-top: 20px;
        }
        
        /* Improved form styling */
        .form-group {
          margin-bottom: 15px;
        }
        
        .form-control {
          border-radius: 4px;
        }
        
        /* Better table styling */
        .dataTables_wrapper {
          margin-top: 20px;
        }
        
        /* Plot output improvements */
        .shiny-plot-output {
          border: 1px solid #ddd;
          border-radius: 4px;
        }
        
        /* Loading spinner improvements */
        .shiny-spinner {
          margin: 20px auto;
        }
      "))
    ),
    
    bs4TabItems(
      # Tab Beranda
      bs4TabItem(tabName = "beranda",
                 source("ui/beranda_ui.R", local = TRUE)$value
      ),
      
      # Tab Panduan Pengguna
      bs4TabItem(tabName = "user_guide",
                 source("ui/user_guide_ui.R", local = TRUE)$value
      ),
      
      # Tab Manajemen Data
      bs4TabItem(tabName = "data_management",
                 source("ui/data_management_ui.R", local = TRUE)$value
      ),
      
      # Tab Statistik Deskriptif
      bs4TabItem(tabName = "descriptive",
                 source("ui/descriptive_ui.R", local = TRUE)$value
      ),
      
      # Tab Visualisasi
      bs4TabItem(tabName = "visualization",
                 source("ui/visualization_ui.R", local = TRUE)$value
      ),
      
      # Tab Peta
      bs4TabItem(tabName = "mapping",
                 source("ui/mapping_ui.R", local = TRUE)$value
      ),
      
      # Tab Uji Asumsi
      bs4TabItem(tabName = "assumptions",
                 div(class = "assumptions-container",
                     source("ui/assumptions_ui.R", local = TRUE)$value)
      ),
      
      # Tab t-Test
      bs4TabItem(tabName = "ttest",
                 source("ui/ttest_ui.R", local = TRUE)$value
      ),
      
      # Tab Uji Proporsi
      bs4TabItem(tabName = "proportion",
                 source("ui/proportion_ui.R", local = TRUE)$value
      ),
      
      # Tab Uji Variance
      bs4TabItem(tabName = "variance",
                 source("ui/variance_ui.R", local = TRUE)$value
      ),
      
      # Tab ANOVA
      bs4TabItem(tabName = "anova",
                 source("ui/anova_ui.R", local = TRUE)$value
      ),
      
      # Tab Regresi
      bs4TabItem(tabName = "regression",
                 source("ui/regression_ui.R", local = TRUE)$value
      ),
      
      # Tab Download
      bs4TabItem(tabName = "download",
                 source("ui/download_ui.R", local = TRUE)$value
      )
    )
  )
)


shinyApp(ui = ui, server = server)