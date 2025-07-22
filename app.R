# Load required libraries
library(shiny)
library(shinydashboard)
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
library(bs4Dash)

source("requirements.R")

# PERBAIKAN: Cek dan install package yang diperlukan
required_packages <- c("shiny", "shinydashboard", "DT", "plotly", "leaflet", 
                       "dplyr", "ggplot2", "readr", "car", "nortest", "corrplot", 
                       "RColorBrewer", "stringr", "geojsonio", "sp", 
                       "leaflet.providers", "rmarkdown", "knitr")

missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
}

# Load semua library yang diperlukan
loaded_packages <- sapply(required_packages, function(pkg) {
  tryCatch({
    library(pkg, character.only = TRUE)
    return(TRUE)
  }, error = function(e) {
    cat("Failed to load package:", pkg, "-", e$message, "\n")
    return(FALSE)
  })
})

# Report loading status
cat("Package loading status:\n")
for(i in 1:length(loaded_packages)) {
  status <- if(loaded_packages[i]) "✓" else "✗"
  cat(status, names(loaded_packages)[i], "\n")
}

cat("\n=== Loading Modules ===\n")

module_files <- c(
  "modules/data_module.R",
  "modules/analysis_functions.R", 
  "modules/visualization_functions.R"
)

for(module in module_files) {
  if(file.exists(module)) {
    tryCatch({
      source(module)
      cat("✓ Loaded:", module, "\n")
    }, error = function(e) {
      cat("✗ Error loading", module, ":", e$message, "\n")
    })
  } else {
    cat("✗ File not found:", module, "\n")
  }
}

cat("\n=== Loading UI and Server ===\n")

if(file.exists("ui.R")) {
  tryCatch({
    source("ui.R")
    cat("✓ Loaded: ui.R\n")
  }, error = function(e) {
    cat("✗ Error loading ui.R:", e$message, "\n")
    stop("Cannot load UI")
  })
} else {
  stop("ui.R file not found")
}

if(file.exists("server.R")) {
  tryCatch({
    source("server.R")
    cat("✓ Loaded: server.R\n")
  }, error = function(e) {
    cat("✗ Error loading server.R:", e$message, "\n")
    stop("Cannot load Server")
  })
} else {
  stop("server.R file not found")
}

cat("\n=== Checking Data Files ===\n")
data_files <- c("sovi_data.csv", "distance.csv")
for(data_file in data_files) {
  if(file.exists(data_file)) {
    cat("✓ Found:", data_file, "\n")
  } else {
    cat("✗ Missing:", data_file, "\n")
  }
}

cat("\n=== Checking Template Files ===\n")
template_files <- c("text_report.Rmd", "laporan_lengkap.Rmd")
for(template in template_files) {
  if(file.exists(template)) {
    cat("✓ Found:", template, "\n")
  } else {
    cat("✗ Missing:", template, "\n")
  }
}

cat("\n=== Starting Shiny App ===\n")

# Jalankan aplikasi
shinyApp(ui = ui, server = server)