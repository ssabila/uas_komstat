library(shinyjs)

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