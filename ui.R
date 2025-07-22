ui <- dashboardPage(
  # Header
  dashboardHeader(
    title = "SOVI Dashboard",
    titleWidth = 400
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Panduan Pengguna", tabName = "user_guide", icon = icon("book")),
      menuItem("Manajemen Data", tabName = "data_management", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "exploration", icon = icon("search"),
               menuSubItem("Statistik Deskriptif", tabName = "descriptive"),
               menuSubItem("Visualisasi", tabName = "visualization"),
               menuSubItem("Peta Distribusi", tabName = "mapping")
      ),
      menuItem("Uji Asumsi", tabName = "assumptions", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inference", icon = icon("chart-line"),
               menuSubItem("Uji t-Test", tabName = "ttest"),
               menuSubItem("Uji Proporsi", tabName = "proportion"),
               menuSubItem("Uji Variance", tabName = "variance"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear", tabName = "regression", icon = icon("line-chart")),
      menuItem("Download Center", tabName = "download", icon = icon("download"))
    )
  ),
  
  # Body
  dashboardBody(
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
    
    tabItems(
      # Tab Beranda
      tabItem(tabName = "beranda",
              source("ui/beranda_ui.R", local = TRUE)$value
      ),
      
      # Tab Panduan Pengguna
      tabItem(tabName = "user_guide",
              source("ui/user_guide_ui.R", local = TRUE)$value
      ),
      
      # Tab Manajemen Data
      tabItem(tabName = "data_management",
              source("ui/data_management_ui.R", local = TRUE)$value
      ),
      
      # Tab Statistik Deskriptif
      tabItem(tabName = "descriptive",
              source("ui/descriptive_ui.R", local = TRUE)$value
      ),
      
      # Tab Visualisasi
      tabItem(tabName = "visualization",
              source("ui/visualization_ui.R", local = TRUE)$value
      ),
      
      # Tab Peta
      tabItem(tabName = "mapping",
              source("ui/mapping_ui.R", local = TRUE)$value
      ),
      
      # Tab Uji Asumsi
      tabItem(tabName = "assumptions",
              div(class = "assumptions-container",
                  source("ui/assumptions_ui.R", local = TRUE)$value)
      ),
      
      # Tab t-Test
      tabItem(tabName = "ttest",
              source("ui/ttest_ui.R", local = TRUE)$value
      ),
      
      # Tab Uji Proporsi
      tabItem(tabName = "proportion",
              source("ui/proportion_ui.R", local = TRUE)$value
      ),
      
      # Tab Uji Variance
      tabItem(tabName = "variance",
              source("ui/variance_ui.R", local = TRUE)$value
      ),
      
      # Tab ANOVA
      tabItem(tabName = "anova",
              source("ui/anova_ui.R", local = TRUE)$value
      ),
      
      # Tab Regresi
      tabItem(tabName = "regression",
              source("ui/regression_ui.R", local = TRUE)$value
      ),
      
      # Tab Download
      tabItem(tabName = "download",
              source("ui/download_ui.R", local = TRUE)$value
      )
    )
  )
)