# requirements.R - Library yang diperlukan untuk dashboard SOVI

# Pastikan library berikut sudah terinstall sebelum menjalankan dashboard

# Core libraries
required_packages <- c(
  "shiny",          # Framework web apps
  "shinydashboard", # Dashboard UI
  "shinyjs",        # JavaScript integration
  "DT",            # DataTables
  "plotly",        # Interactive plots - PENTING UNTUK PIE CHART
  "ggplot2",       # Static plots
  "dplyr",         # Data manipulation
  "htmltools",     # HTML utilities
  "htmlwidgets"    # Widget framework
)

# Function untuk install packages yang missing
install_missing_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages, dependencies = TRUE)
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Install missing packages
install_missing_packages(required_packages)

# Load all packages
invisible(sapply(required_packages, library, character.only = TRUE))

# Verify critical packages
if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("Package 'plotly' is required for pie chart visualization. Please install it using: install.packages('plotly')")
}

if (!requireNamespace("DT", quietly = TRUE)) {
  stop("Package 'DT' is required for data tables. Please install it using: install.packages('DT')")
}

cat("All required packages loaded successfully!\n")
cat("✓ plotly version:", packageVersion("plotly"), "\n")
cat("✓ ggplot2 version:", packageVersion("ggplot2"), "\n")
cat("✓ DT version:", packageVersion("DT"), "\n")