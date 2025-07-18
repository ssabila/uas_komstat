ui <- dashboardPage(
  # Header
  dashboardHeader(
    title = "SOVI Dashboard: Social Vulnerability Insights Indonesia",
    titleWidth = 400
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Panduan Pengguna", tabName = "user_guide", icon = icon("book")), # BARU: Tab panduan
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
      "))
    ),
    
    tabItems(
      # Tab Beranda
      tabItem(tabName = "beranda",
              source("ui/beranda_ui.R", local = TRUE)$value
      ),
      
      # BARU: Tab Panduan Pengguna
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
              source("ui/assumptions_ui.R", local = TRUE)$value
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