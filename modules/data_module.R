library(readr)
library(dplyr)

# Fungsi untuk memuat data SOVI
load_sovi_data <- function() {
  cat("Loading SOVI data...\n")
  tryCatch({
    if (!file.exists("sovi_data.csv")) {
      warning("File sovi_data.csv tidak ditemukan")
      return(NULL)
    }
    
    data <- read_csv("sovi_data.csv", show_col_types = FALSE)
    
    # Bersihkan nama kolom
    names(data) <- make.names(names(data))
    
    # Konversi tipe data yang sesuai
    numeric_cols <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                      "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE",
                      "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION")
    
    # Only convert columns that exist
    existing_numeric_cols <- intersect(numeric_cols, names(data))
    data[existing_numeric_cols] <- lapply(data[existing_numeric_cols], as.numeric)
    
    if ("DISTRICTCODE" %in% names(data)) {
      data$DISTRICTCODE <- as.character(data$DISTRICTCODE)
    }
    
    cat("✓ SOVI data loaded successfully:", nrow(data), "rows,", ncol(data), "columns\n")
    return(data)
  }, error = function(e) {
    cat("✗ Error loading SOVI data:", e$message, "\n")
    return(NULL)
  })
}

# Fungsi untuk memuat data jarak
load_distance_data <- function() {
  cat("Loading distance data...\n")
  tryCatch({
    if (!file.exists("distance.csv")) {
      warning("File distance.csv tidak ditemukan")
      return(NULL)
    }
    
    data <- read_csv("distance.csv", show_col_types = FALSE)
    cat("✓ Distance data loaded successfully\n")
    return(data)
  }, error = function(e) {
    cat("✗ Error loading distance data:", e$message, "\n")
    return(NULL)
  })
}

# FUNGSI UNTUK MEMBUAT VARIABEL KATEGORI DUMMY - VERSI LENGKAP
create_dummy_categorical_variables <- function(data) {
  cat("Creating dummy categorical variables...\n")
  
  if (is.null(data) || nrow(data) == 0) {
    cat("✗ No data to process\n")
    return(data)
  }
  
  tryCatch({
    # 1. Buat kategori kemiskinan berdasarkan median
    if ("POVERTY" %in% names(data) && !"POVERTY_LEVEL" %in% names(data)) {
      poverty_values <- data$POVERTY[!is.na(data$POVERTY)]
      if (length(poverty_values) > 0) {
        poverty_median <- median(poverty_values)
        data$POVERTY_LEVEL <- ifelse(is.na(data$POVERTY), NA,
                                     ifelse(data$POVERTY > poverty_median, "Tinggi", "Rendah"))
        cat("✓ Created POVERTY_LEVEL variable\n")
      }
    }
    
    # 2. Buat kategori ukuran populasi berdasarkan tertile
    if ("POPULATION" %in% names(data) && !"POPULATION_SIZE" %in% names(data)) {
      pop_values <- data$POPULATION[!is.na(data$POPULATION)]
      if (length(pop_values) > 0) {
        pop_tertiles <- quantile(pop_values, probs = c(0.33, 0.67), na.rm = TRUE)
        data$POPULATION_SIZE <- cut(data$POPULATION, 
                                    breaks = c(-Inf, pop_tertiles[1], pop_tertiles[2], Inf),
                                    labels = c("Kecil", "Sedang", "Besar"),
                                    include.lowest = TRUE)
        cat("✓ Created POPULATION_SIZE variable\n")
      }
    }
    
    # 3. Buat kategori pendidikan berdasarkan kuartil
    if ("LOWEDU" %in% names(data) && !"EDUCATION_CATEGORY" %in% names(data)) {
      edu_values <- data$LOWEDU[!is.na(data$LOWEDU)]
      if (length(edu_values) > 0) {
        edu_quartiles <- quantile(edu_values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        data$EDUCATION_CATEGORY <- cut(data$LOWEDU,
                                       breaks = c(-Inf, edu_quartiles[1], edu_quartiles[2], edu_quartiles[3], Inf),
                                       labels = c("Sangat_Baik", "Baik", "Cukup", "Kurang"),
                                       include.lowest = TRUE)
        cat("✓ Created EDUCATION_CATEGORY variable\n")
      }
    }
    
    # 4. BARU: Buat kategori provinsi berdasarkan 2 digit pertama DISTRICTCODE
    if ("DISTRICTCODE" %in% names(data) && !"PROVINCE_CODE" %in% names(data)) {
      province_codes <- create_province_categories(data$DISTRICTCODE)
      data$PROVINCE_CODE <- province_codes$province_code
      data$PROVINCE_NAME <- province_codes$province_name
      cat("✓ Created PROVINCE_CODE and PROVINCE_NAME variables\n")
    }
    
    # 5. BARU: Buat kategori pulau berdasarkan kode provinsi
    if ("DISTRICTCODE" %in% names(data) && !"ISLAND_GROUP" %in% names(data)) {
      data$ISLAND_GROUP <- create_island_categories(data$DISTRICTCODE)
      cat("✓ Created ISLAND_GROUP variable\n")
    }
    
    # 6. BARU: Buat kategori region (kombinasi pulau yang lebih sederhana)
    if ("DISTRICTCODE" %in% names(data) && !"REGION" %in% names(data)) {
      data$REGION <- create_region_categories(data$DISTRICTCODE)
      cat("✓ Created REGION variable\n")
    }
    
    cat("✓ All categorical variables created successfully\n")
    return(data)
    
  }, error = function(e) {
    cat("✗ Error creating dummy variables:", e$message, "\n")
    return(data)
  })
}

# FUNGSI UNTUK MAPPING KODE PROVINSI
create_province_categories <- function(district_codes) {
  # Ekstrak 2 digit pertama sebagai kode provinsi
  province_codes <- substr(as.character(district_codes), 1, 2)
  
  # Mapping kode provinsi ke nama provinsi (berdasarkan kode BPS)
  province_mapping <- list(
    "11" = "Aceh",
    "12" = "Sumatera Utara", 
    "13" = "Sumatera Barat",
    "14" = "Riau",
    "15" = "Jambi",
    "16" = "Sumatera Selatan",
    "17" = "Bengkulu",
    "18" = "Lampung",
    "19" = "Kepulauan Bangka Belitung",
    "21" = "Kepulauan Riau",
    "31" = "DKI Jakarta",
    "32" = "Jawa Barat",
    "33" = "Jawa Tengah",
    "34" = "DI Yogyakarta",
    "35" = "Jawa Timur",
    "36" = "Banten",
    "51" = "Bali",
    "52" = "Nusa Tenggara Barat",
    "53" = "Nusa Tenggara Timur",
    "61" = "Kalimantan Barat",
    "62" = "Kalimantan Tengah",
    "63" = "Kalimantan Selatan",
    "64" = "Kalimantan Timur",
    "65" = "Kalimantan Utara",
    "71" = "Sulawesi Utara",
    "72" = "Sulawesi Tengah", 
    "73" = "Sulawesi Selatan",
    "74" = "Sulawesi Tenggara",
    "75" = "Gorontalo",
    "76" = "Sulawesi Barat",
    "81" = "Maluku",
    "82" = "Maluku Utara",
    "91" = "Papua Barat",
    "94" = "Papua"
  )
  
  # Map kode ke nama provinsi
  province_names <- sapply(province_codes, function(code) {
    if (code %in% names(province_mapping)) {
      return(province_mapping[[code]])
    } else {
      return(paste0("Provinsi_", code))  # Fallback untuk kode yang tidak dikenali
    }
  })
  
  return(list(
    province_code = province_codes,
    province_name = as.character(province_names)
  ))
}

# FUNGSI UNTUK MAPPING PULAU
create_island_categories <- function(district_codes) {
  # Ekstrak 2 digit pertama sebagai kode provinsi
  province_codes <- substr(as.character(district_codes), 1, 2)
  
  # Mapping kode provinsi ke pulau
  island_mapping <- list(
    # Sumatera
    "11" = "Sumatera", "12" = "Sumatera", "13" = "Sumatera", 
    "14" = "Sumatera", "15" = "Sumatera", "16" = "Sumatera", 
    "17" = "Sumatera", "18" = "Sumatera", "19" = "Sumatera", 
    "21" = "Sumatera",
    
    # Jawa
    "31" = "Jawa", "32" = "Jawa", "33" = "Jawa", 
    "34" = "Jawa", "35" = "Jawa", "36" = "Jawa",
    
    # Bali dan Nusa Tenggara
    "51" = "Bali_Nusa_Tenggara", 
    "52" = "Bali_Nusa_Tenggara", 
    "53" = "Bali_Nusa_Tenggara",
    
    # Kalimantan
    "61" = "Kalimantan", "62" = "Kalimantan", "63" = "Kalimantan", 
    "64" = "Kalimantan", "65" = "Kalimantan",
    
    # Sulawesi
    "71" = "Sulawesi", "72" = "Sulawesi", "73" = "Sulawesi", 
    "74" = "Sulawesi", "75" = "Sulawesi", "76" = "Sulawesi",
    
    # Maluku
    "81" = "Maluku", "82" = "Maluku",
    
    # Papua
    "91" = "Papua", "94" = "Papua"
  )
  
  # Map kode ke pulau
  islands <- sapply(province_codes, function(code) {
    if (code %in% names(island_mapping)) {
      return(island_mapping[[code]])
    } else {
      return("Lainnya")  # Fallback
    }
  })
  
  return(as.character(islands))
}

# FUNGSI UNTUK MAPPING REGION (LEBIH SEDERHANA)
create_region_categories <- function(district_codes) {
  # Ekstrak 2 digit pertama sebagai kode provinsi
  province_codes <- substr(as.character(district_codes), 1, 2)
  
  # Mapping ke region yang lebih sederhana (3 kategori utama)
  region_mapping <- list(
    # Indonesia Barat
    "11" = "Indonesia_Barat", "12" = "Indonesia_Barat", "13" = "Indonesia_Barat", 
    "14" = "Indonesia_Barat", "15" = "Indonesia_Barat", "16" = "Indonesia_Barat", 
    "17" = "Indonesia_Barat", "18" = "Indonesia_Barat", "19" = "Indonesia_Barat", 
    "21" = "Indonesia_Barat", "31" = "Indonesia_Barat", "32" = "Indonesia_Barat", 
    "33" = "Indonesia_Barat", "34" = "Indonesia_Barat", "35" = "Indonesia_Barat", 
    "36" = "Indonesia_Barat", "51" = "Indonesia_Barat",
    
    # Indonesia Tengah
    "52" = "Indonesia_Tengah", "53" = "Indonesia_Tengah", 
    "61" = "Indonesia_Tengah", "62" = "Indonesia_Tengah", "63" = "Indonesia_Tengah", 
    "64" = "Indonesia_Tengah", "65" = "Indonesia_Tengah",
    "71" = "Indonesia_Tengah", "72" = "Indonesia_Tengah", "73" = "Indonesia_Tengah", 
    "74" = "Indonesia_Tengah", "75" = "Indonesia_Tengah", "76" = "Indonesia_Tengah",
    
    # Indonesia Timur
    "81" = "Indonesia_Timur", "82" = "Indonesia_Timur",
    "91" = "Indonesia_Timur", "94" = "Indonesia_Timur"
  )
  
  # Map kode ke region
  regions <- sapply(province_codes, function(code) {
    if (code %in% names(region_mapping)) {
      return(region_mapping[[code]])
    } else {
      return("Lainnya")  # Fallback
    }
  })
  
  return(as.character(regions))
}

# FUNGSI KATEGORISASI YANG DISEDERHANAKAN
categorize_variable <- function(data, variable, method = "quantile", breaks = 3) {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  values <- data[[variable]]
  clean_values <- values[!is.na(values)]
  
  if (length(clean_values) == 0) {
    stop("Tidak ada data valid untuk dikategorisasi")
  }
  
  if (length(unique(clean_values)) < breaks) {
    stop(paste("Jumlah nilai unik kurang dari jumlah kategori yang diminta"))
  }
  
  tryCatch({
    if (method == "quantile") {
      quantiles <- quantile(clean_values, probs = seq(0, 1, length.out = breaks + 1), na.rm = TRUE)
      # Check for duplicate quantiles
      if (length(unique(quantiles)) < length(quantiles)) {
        method <- "equal"  # fallback to equal intervals
      } else {
        categories <- cut(values, breaks = quantiles, include.lowest = TRUE, 
                          labels = paste0("Q", 1:breaks))
        return(categories)
      }
    }
    
    if (method == "equal") {
      min_val <- min(clean_values)
      max_val <- max(clean_values)
      if (min_val == max_val) {
        stop("Variabel memiliki nilai konstan")
      }
      range_extend <- (max_val - min_val) * 0.001
      breaks_seq <- seq(min_val - range_extend, max_val + range_extend, length.out = breaks + 1)
      categories <- cut(values, breaks = breaks_seq, include.lowest = TRUE,
                        labels = paste0("Cat", 1:breaks))
      return(categories)
    }
    
  }, error = function(e) {
    stop(paste("Error dalam kategorisasi:", e$message))
  })
}

# FUNGSI TRANSFORMASI YANG DISEDERHANAKAN
transform_data <- function(data, variable, method = "log") {
  if (!variable %in% names(data)) {
    stop("Variabel tidak ditemukan dalam data")
  }
  
  values <- data[[variable]]
  na_mask <- is.na(values)
  
  tryCatch({
    transformed <- switch(method,
                          "log" = {
                            if (any(values <= 0, na.rm = TRUE)) {
                              log(values + 1)  # log(x + 1) transformation
                            } else {
                              log(values)
                            }
                          },
                          "sqrt" = {
                            if (any(values < 0, na.rm = TRUE)) {
                              stop("Transformasi akar kuadrat tidak dapat diterapkan pada nilai negatif")
                            }
                            sqrt(values)
                          },
                          "square" = values^2,
                          "standardize" = {
                            mean_val <- mean(values, na.rm = TRUE)
                            sd_val <- sd(values, na.rm = TRUE)
                            if (sd_val == 0) {
                              stop("Tidak dapat standardisasi: standar deviasi = 0")
                            }
                            (values - mean_val) / sd_val
                          },
                          "normalize" = {
                            min_val <- min(values, na.rm = TRUE)
                            max_val <- max(values, na.rm = TRUE)
                            if (min_val == max_val) {
                              stop("Tidak dapat normalisasi: semua nilai sama")
                            }
                            (values - min_val) / (max_val - min_val)
                          },
                          values  # default: return original values
    )
    
    transformed[na_mask] <- NA
    return(transformed)
    
  }, error = function(e) {
    stop(paste("Error dalam transformasi:", e$message))
  })
}

# FUNGSI UNTUK MENDAPATKAN VARIABEL KATEGORI YANG VALID
get_valid_categorical_variables <- function(data, min_levels = 2, max_levels = 20, min_obs_per_level = 2) {
  if (is.null(data) || nrow(data) == 0) {
    return(character(0))
  }
  
  valid_vars <- character(0)
  
  for (col_name in names(data)) {
    col_data <- data[[col_name]]
    
    if (!is.character(col_data) && !is.factor(col_data)) {
      next
    }
    
    clean_data <- col_data[!is.na(col_data)]
    
    if (length(clean_data) == 0) {
      next
    }
    
    unique_levels <- unique(clean_data)
    n_levels <- length(unique_levels)
    
    if (n_levels < min_levels || n_levels > max_levels) {
      next
    }
    
    level_counts <- table(clean_data)
    if (any(level_counts < min_obs_per_level)) {
      next
    }
    
    valid_vars <- c(valid_vars, col_name)
  }
  
  return(valid_vars)
}

# Fungsi untuk mendapatkan metadata variabel
get_variable_metadata <- function() {
  list(
    DISTRICTCODE = list(
      name = "Kode Kabupaten",
      description = "Kode unik untuk setiap kabupaten/kota",
      type = "categorical"
    ),
    CHILDREN = list(
      name = "Persentase Anak-anak",
      description = "Persentase penduduk berusia di bawah 15 tahun",
      type = "numeric",
      unit = "%"
    ),
    FEMALE = list(
      name = "Persentase Perempuan",
      description = "Persentase penduduk perempuan",
      type = "numeric",
      unit = "%"
    ),
    ELDERLY = list(
      name = "Persentase Lansia",
      description = "Persentase penduduk berusia 65 tahun ke atas",
      type = "numeric",
      unit = "%"
    ),
    FHEAD = list(
      name = "Kepala Keluarga Perempuan",
      description = "Persentase rumah tangga dengan kepala keluarga perempuan",
      type = "numeric",
      unit = "%"
    ),
    FAMILYSIZE = list(
      name = "Ukuran Keluarga",
      description = "Rata-rata jumlah anggota keluarga per rumah tangga",
      type = "numeric",
      unit = "orang"
    ),
    NOELECTRIC = list(
      name = "Tanpa Listrik",
      description = "Persentase rumah tangga tanpa akses listrik",
      type = "numeric",
      unit = "%"
    ),
    LOWEDU = list(
      name = "Pendidikan Rendah",
      description = "Persentase penduduk dengan pendidikan rendah",
      type = "numeric",
      unit = "%"
    ),
    GROWTH = list(
      name = "Pertumbuhan Penduduk",
      description = "Tingkat pertumbuhan penduduk tahunan",
      type = "numeric",
      unit = "%"
    ),
    POVERTY = list(
      name = "Kemiskinan",
      description = "Persentase penduduk miskin",
      type = "numeric",
      unit = "%"
    ),
    ILLITERATE = list(
      name = "Buta Huruf",
      description = "Persentase penduduk buta huruf",
      type = "numeric",
      unit = "%"
    ),
    NOTRAINING = list(
      name = "Tanpa Pelatihan",
      description = "Persentase penduduk tanpa pelatihan keterampilan",
      type = "numeric",
      unit = "%"
    ),
    DPRONE = list(
      name = "Rawan Bencana",
      description = "Persentase wilayah rawan bencana",
      type = "numeric",
      unit = "%"
    ),
    RENTED = list(
      name = "Rumah Sewa",
      description = "Persentase rumah tangga yang menyewa tempat tinggal",
      type = "numeric",
      unit = "%"
    ),
    NOSEWER = list(
      name = "Tanpa Saluran Pembuangan",
      description = "Persentase rumah tangga tanpa akses saluran pembuangan",
      type = "numeric",
      unit = "%"
    ),
    TAPWATER = list(
      name = "Air Ledeng",
      description = "Persentase rumah tangga dengan akses air ledeng",
      type = "numeric",
      unit = "%"
    ),
    POPULATION = list(
      name = "Jumlah Penduduk",
      description = "Total jumlah penduduk",
      type = "numeric",
      unit = "jiwa"
    ),
    # KATEGORI YANG DIBUAT OTOMATIS
    POVERTY_LEVEL = list(
      name = "Tingkat Kemiskinan",
      description = "Kategori tingkat kemiskinan (Tinggi/Rendah)",
      type = "categorical"
    ),
    POPULATION_SIZE = list(
      name = "Ukuran Populasi",
      description = "Kategori ukuran populasi (Kecil/Sedang/Besar)",
      type = "categorical"
    ),
    EDUCATION_CATEGORY = list(
      name = "Kategori Pendidikan",
      description = "Kategori tingkat pendidikan (Sangat_Baik/Baik/Cukup/Kurang)",
      type = "categorical"
    ),
    # KATEGORI GEOGRAFIS BARU
    PROVINCE_CODE = list(
      name = "Kode Provinsi",
      description = "Kode provinsi berdasarkan 2 digit pertama kode kabupaten",
      type = "categorical"
    ),
    PROVINCE_NAME = list(
      name = "Nama Provinsi",
      description = "Nama provinsi berdasarkan kode BPS",
      type = "categorical"
    ),
    ISLAND_GROUP = list(
      name = "Kelompok Pulau",
      description = "Pengelompokan berdasarkan pulau (Sumatera, Jawa, Kalimantan, Sulawesi, Bali_Nusa_Tenggara, Maluku, Papua)",
      type = "categorical"
    ),
    REGION = list(
      name = "Wilayah Indonesia",
      description = "Pengelompokan wilayah Indonesia (Indonesia_Barat, Indonesia_Tengah, Indonesia_Timur)",
      type = "categorical"
    )
  )
}

cat("✓ Data module loaded successfully\n")

# Fungsi untuk mendapatkan ringkasan distribusi geografis
get_geographic_summary <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return("Data tidak tersedia")
  }
  
  summary_text <- "=== RINGKASAN DISTRIBUSI GEOGRAFIS ===\n\n"
  
  # Ringkasan per provinsi
  if ("PROVINCE_NAME" %in% names(data)) {
    province_counts <- table(data$PROVINCE_NAME)
    province_counts <- sort(province_counts, decreasing = TRUE)
    
    summary_text <- paste0(summary_text, "DISTRIBUSI PER PROVINSI:\n")
    summary_text <- paste0(summary_text, sprintf("Total Provinsi: %d\n", length(province_counts)))
    summary_text <- paste0(summary_text, "Top 5 Provinsi dengan Kabupaten/Kota Terbanyak:\n")
    
    top_provinces <- head(province_counts, 5)
    for (i in 1:length(top_provinces)) {
      summary_text <- paste0(summary_text, sprintf("%d. %s: %d kabupaten/kota\n", 
                                                   i, names(top_provinces)[i], top_provinces[i]))
    }
    summary_text <- paste0(summary_text, "\n")
  }
  
  # Ringkasan per pulau
  if ("ISLAND_GROUP" %in% names(data)) {
    island_counts <- table(data$ISLAND_GROUP)
    island_counts <- sort(island_counts, decreasing = TRUE)
    
    summary_text <- paste0(summary_text, "DISTRIBUSI PER KELOMPOK PULAU:\n")
    for (i in 1:length(island_counts)) {
      pct <- round(100 * island_counts[i] / nrow(data), 1)
      summary_text <- paste0(summary_text, sprintf("- %s: %d kabupaten/kota (%.1f%%)\n", 
                                                   names(island_counts)[i], island_counts[i], pct))
    }
    summary_text <- paste0(summary_text, "\n")
  }
  
  # Ringkasan per region
  if ("REGION" %in% names(data)) {
    region_counts <- table(data$REGION)
    region_counts <- sort(region_counts, decreasing = TRUE)
    
    summary_text <- paste0(summary_text, "DISTRIBUSI PER WILAYAH:\n")
    for (i in 1:length(region_counts)) {
      pct <- round(100 * region_counts[i] / nrow(data), 1)
      summary_text <- paste0(summary_text, sprintf("- %s: %d kabupaten/kota (%.1f%%)\n", 
                                                   names(region_counts)[i], region_counts[i], pct))
    }
    summary_text <- paste0(summary_text, "\n")
  }
  
  return(summary_text)
}