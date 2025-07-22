# server/data_management_server.R - VERSI FINAL TANPA VISUALISASI & INTERPRETASI

# ===== REACTIVE VALUES =====
processed_data <- reactiveValues(
  current = NULL,
  original = NULL,
  operations = list(),
  operation_log = character(0),
  outliers = NULL,
  last_outlier_var = NULL # Simpan variabel terakhir yang dicek
)

# Load data awal
observe({
  if (is.null(processed_data$current)) {
    tryCatch({
      data <- sovi_data(); if (!is.null(data)) { processed_data$current <- data; processed_data$original <- data }
    }, error = function(e) { cat("Error loading data:", e$message, "\n") })
  }
})

# ===== STATUS DATA SUMMARY =====
output$data_status_summary <- renderText({
  data <- processed_data$current
  if (!is.null(data)) {
    n_obs <- nrow(data)
    n_vars <- ncol(data)
    n_numeric <- sum(sapply(data, is.numeric))
    n_categorical <- sum(sapply(data, function(x) is.character(x) || is.factor(x)))
    missing_pct <- round(100 * sum(is.na(data)) / (n_obs * n_vars), 2)
    outliers_count <- if (!is.null(processed_data$outliers)) length(processed_data$outliers) else 0
    
    status_text <- paste(
      "=== STATUS DATA TERKINI ===",
      paste("📊 Total Observasi:", format(n_obs, big.mark = ",")),
      paste("📋 Total Variabel:", n_vars, paste0("(", n_numeric, " numerik, ", n_categorical, " kategori)")),
      paste("❌ Missing Values:", paste0(missing_pct, "%")),
      paste("🔍 Outliers Terdeteksi:", outliers_count),
      paste("⚙️ Operasi Diterapkan:", length(processed_data$operations)),
      "",
      "=== KUALITAS DATA ===",
      paste("🟢 Data Quality:", if(missing_pct < 5) "Excellent" else if(missing_pct < 15) "Good" else "Needs Attention"),
      paste("📈 Data Size:", if(n_obs > 1000) "Large" else if(n_obs > 100) "Medium" else "Small"),
      sep = "\n"
    )
    
    # Tambah informasi operasi terbaru
    if (length(processed_data$operation_log) > 0) {
      recent_op <- tail(processed_data$operation_log, 1)
      status_text <- paste(status_text, "", "=== OPERASI TERBARU ===", recent_op, sep = "\n")
    }
    
    return(status_text)
  } else {
    return("❌ Data belum tersedia atau gagal dimuat.\n\nPastikan file data SOVI tersedia dan dapat diakses.")
  }
})

# ===== VALUE BOXES (DIPERBAIKI) =====
output$vbox_rows <- renderValueBox({
  req(processed_data$current)
  valueBox(
    value = format(nrow(processed_data$current), big.mark = ","),
    subtitle = "Baris Data",
    icon = icon("table"),
    color = "primary"
  )
})

output$vbox_cols <- renderValueBox({
  req(processed_data$current)
  valueBox(
    value = ncol(processed_data$current),
    subtitle = "Kolom Data",
    icon = icon("columns"),
    color = "info"
  )
})

output$vbox_missing <- renderValueBox({
  req(processed_data$current)
  data <- processed_data$current
  missing_pct <- round(100 * sum(is.na(data)) / (nrow(data) * ncol(data)), 1)
  
  valueBox(
    value = paste0(missing_pct, "%"),
    subtitle = "Missing Values",
    icon = icon("exclamation-triangle"),
    color = if(missing_pct < 5) "success" else if(missing_pct < 15) "warning" else "danger"
  )
})

output$vbox_outliers <- renderValueBox({
  outliers_count <- if (!is.null(processed_data$outliers)) length(processed_data$outliers) else 0
  
  valueBox(
    value = outliers_count,
    subtitle = "Outliers Terdeteksi",
    icon = icon("search"),
    color = if(outliers_count == 0) "success" else if(outliers_count < 10) "warning" else "danger"
  )
})

# ===== TAMBAHAN: INFO BOX UNTUK STATUS OPERASI =====
output$operation_status_box <- renderInfoBox({
  ops_count <- length(processed_data$operations)
  
  infoBox(
    title = "Status Operasi",
    value = if(ops_count == 0) "Ready" else paste(ops_count, "Applied"),
    subtitle = if(ops_count == 0) "Siap untuk transformasi data" else "Operasi telah diterapkan",
    icon = icon("cogs"),
    color = if(ops_count == 0) "primary" else "success",
    fill = TRUE
  )
})

# ===== DATA QUALITY ASSESSMENT =====
output$data_quality_assessment <- renderText({
  req(processed_data$current)
  data <- processed_data$current
  
  n_obs <- nrow(data)
  n_vars <- ncol(data)
  numeric_vars <- sapply(data, is.numeric)
  
  missing_by_var <- sapply(data, function(x) sum(is.na(x)))
  missing_pct_by_var <- round(100 * missing_by_var / n_obs, 1)
  vars_with_missing <- sum(missing_by_var > 0)
  
  outlier_summary <- ""
  if(sum(numeric_vars) > 0) {
    numeric_data <- data[, numeric_vars, drop = FALSE]
    total_outliers <- 0
    
    for(var in names(numeric_data)) {
      values <- numeric_data[[var]]
      clean_values <- values[!is.na(values)]
      if(length(clean_values) > 1) { 
        Q1 <- quantile(clean_values, 0.25)
        Q3 <- quantile(clean_values, 0.75)
        IQR <- Q3 - Q1
        outliers <- sum(clean_values < (Q1 - 1.5 * IQR) | clean_values > (Q3 + 1.5 * IQR))
        total_outliers <- total_outliers + outliers
      }
    }
    outlier_summary <- paste("🔍 Estimasi outliers (IQR method):", total_outliers)
  }
  
  quality_score <- 100
  if(vars_with_missing > 0) quality_score <- quality_score - (vars_with_missing * 5)
  if(any(missing_pct_by_var > 20, na.rm = TRUE)) quality_score <- quality_score - 20
  if(n_obs < 30) quality_score <- quality_score - 15
  quality_score <- max(0, quality_score) 
  
  quality_level <- if(quality_score >= 85) "EXCELLENT ⭐⭐⭐" else
    if(quality_score >= 70) "GOOD ⭐⭐" else
      if(quality_score >= 55) "FAIR ⭐" else "POOR ❌"
  
  assessment <- paste(
    "=== PENILAIAN KUALITAS DATA ===",
    paste("📊 Skor Kualitas:", round(quality_score), "/100 -", quality_level),
    "",
    "=== DETAIL ANALISIS ===",
    paste("📋 Variabel dengan missing values:", vars_with_missing, "dari", n_vars),
    if(vars_with_missing > 0) paste("❌ Missing terbanyak:", max(missing_pct_by_var, na.rm=T), "% pada variabel", names(which.max(missing_pct_by_var))) else "",
    outlier_summary,
    "",
    "=== REKOMENDASI ===",
    if(vars_with_missing > 0) "• Pertimbangkan imputasi untuk missing values" else "• Data lengkap, siap untuk analisis",
    if(any(missing_pct_by_var > 20, na.rm=T)) "• Beberapa variabel memiliki missing values tinggi" else "",
    if(quality_score < 70) "• Data perlu pembersihan sebelum analisis lanjutan" else "• Data berkualitas baik untuk analisis",
    sep = "\n"
  )
  
  return(assessment)
})
# ===== UI SELECTORS DINAMIS =====
observe({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  all_vars <- names(processed_data$current)
  updateSelectInput(session, "transform_var", choices = numeric_vars)
  updateSelectInput(session, "categorize_var", choices = numeric_vars)
  updateSelectInput(session, "outlier_var", choices = numeric_vars)
  updateSelectInput(session, "filter_var", choices = all_vars)
  # Selector untuk visualisasi tidak lagi diperlukan di sini karena UI dihapus
})

# ===== TRANSFORMASI, KATEGORISASI, OUTLIER, FILTER (Tidak Berubah) =====
observeEvent(input$btn_transform, {
  req(processed_data$current, input$transform_var, input$transform_method)
  tryCatch({
    data <- processed_data$current; var_name <- input$transform_var; method <- input$transform_method
    suffix <- if (!is.null(input$transform_suffix) && nzchar(input$transform_suffix)) input$transform_suffix else method
    values <- data[[var_name]]; if (all(is.na(values))) stop("Variabel tidak memiliki data valid")
    transformed <- switch(method, log={if(any(values<=0,na.rm=T))stop("Log memerlukan nilai > 0");log(values)}, log1p=log1p(values), sqrt={if(any(values<0,na.rm=T))stop("Sqrt memerlukan nilai >= 0");sqrt(values)}, square=values^2, inverse={if(any(values==0,na.rm=T))stop("Inverse tidak bisa untuk nilai 0");1/values}, standardize=as.numeric(scale(values)), normalize=(values-min(values,na.rm=T))/(max(values,na.rm=T)-min(values,na.rm=T)))
    new_col_name <- paste0(var_name, "_", suffix); data[[new_col_name]] <- transformed; processed_data$current <- data
    log_entry <- paste("✅ Transformasi:", method, "pada", var_name, "→", new_col_name)
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    shiny::showNotification(paste("✅ Transformasi berhasil!"), type = "default")
  }, error = function(e) { shiny::showNotification(paste("❌ Error:", e$message), type = "error") })
})
observeEvent(input$btn_categorize, {
  req(processed_data$current, input$categorize_var, input$categorize_method)
  tryCatch({
    data <- processed_data$current; var_name <- input$categorize_var; method <- input$categorize_method
    values <- data[[var_name]]; labels_str <- input$categorize_labels
    custom_labels <- NULL; if (!is.null(labels_str) && nzchar(trimws(labels_str))) { custom_labels <- trimws(unlist(strsplit(labels_str, ","))) }
    if (all(is.na(values))) stop("Variabel tidak memiliki data valid")
    n_bins <- 0
    if (method == "quantile") {
      n_bins <- as.numeric(input$quantile_bins); if(is.na(n_bins)||n_bins<2||n_bins>10)stop("Jml kategori kuantil: 2-10")
      if(!is.null(custom_labels)&&length(custom_labels)!=n_bins)stop(paste("Jml nama (",length(custom_labels),") != jml kategori (",n_bins,")"))
      categories <- cut(values, breaks=quantile(values,probs=seq(0,1,length.out=n_bins+1),na.rm=T), labels=custom_labels%||%paste0("Q",1:n_bins), include.lowest=T)
    } else if (method == "equal_width") {
      n_bins <- as.numeric(input$equal_width_bins); if(is.na(n_bins)||n_bins<2||n_bins>10)stop("Jml kategori interval: 2-10")
      if(!is.null(custom_labels)&&length(custom_labels)!=n_bins)stop(paste("Jml nama (",length(custom_labels),") != jml kategori (",n_bins,")"))
      categories <- cut(values, breaks=n_bins, labels=custom_labels%||%paste0("Bin",1:n_bins), include.lowest=T)
    } else if (method == "custom") {
      breaks_str <- input$custom_breaks; if(is.null(breaks_str)||nzchar(breaks_str)==F)stop("Masukkan breakpoints")
      breaks <- as.numeric(unlist(strsplit(breaks_str,","))); if(any(is.na(breaks))||length(breaks)<2)stop("Min. 2 breakpoints numerik")
      n_bins <- length(breaks)-1
      if(!is.null(custom_labels)&&length(custom_labels)!=n_bins)stop(paste("Jml nama (",length(custom_labels),") != jml kategori (",n_bins,")"))
      categories <- cut(values, breaks=sort(unique(breaks)), labels=custom_labels, include.lowest=T)
    }
    new_col_name <- paste0(var_name,"_cat"); data[[new_col_name]]<-categories; processed_data$current<-data
    log_entry <- paste("✅ Kategorisasi:",method,"pada",var_name,"→",new_col_name)
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    shiny::showNotification("✅ Kategorisasi berhasil!", type="default")
  }, error=function(e){shiny::showNotification(paste("❌ Error:",e$message),type="error")})
})
observeEvent(input$btn_detect_outliers, {
  req(processed_data$current, input$outlier_var, input$outlier_method)
  tryCatch({
    var_name <- input$outlier_var; method <- input$outlier_method; values <- processed_data$current[[var_name]]
    valid_values <- values[!is.na(values)]; if(length(valid_values)<2)stop("Tidak cukup data valid")
    outlier_indices <- switch(method, iqr={Q1<-quantile(valid_values,0.25);Q3<-quantile(valid_values,0.75);lower<-Q1-1.5*(Q3-Q1);upper<-Q3+1.5*(Q3-Q1);which(values<lower|values>upper)}, zscore={z_scores<-abs(scale(values));which(z_scores>3)}, modified_zscore={median_val<-median(valid_values);mad_val<-mad(valid_values,constant=1);if(mad_val==0)stop("MAD is zero");modified_z<-abs(0.6745*(values-median_val)/mad_val);which(modified_z>3.5)})
    processed_data$outliers <- outlier_indices; processed_data$last_outlier_var <- var_name
    log_entry <- paste("🔍 Deteksi:",method,"pada",var_name,"- ditemukan",length(outlier_indices),"outliers")
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    shiny::showNotification(paste("🔍 Ditemukan",length(outlier_indices),"outliers."), type="default")
  }, error=function(e){shiny::showNotification(paste("❌ Error:",e$message),type="error")})
})
output$outlier_treatment_ui <- renderUI({
  req(processed_data$outliers)
  if (length(processed_data$outliers) > 0) {
    tagList(h5("2. Penanganan Outlier"),p(paste("Ditemukan",length(processed_data$outliers),"outliers pada '",processed_data$last_outlier_var,"'.")),
            radioButtons("outlier_treatment_method","Metode:",choices=list("Hapus Baris"="remove","Imputasi Batas (Cap)"="cap","Imputasi Mean"="mean","Imputasi Median"="median","Imputasi Modus"="mode"),selected="remove"),
            actionButton("btn_treat_outliers","Terapkan Penanganan",class="btn-danger btn-block"))
  }
})
observeEvent(input$btn_treat_outliers, {
  req(processed_data$current, processed_data$outliers, input$outlier_treatment_method)
  tryCatch({
    data <- processed_data$current; indices <- processed_data$outliers; method <- input$outlier_treatment_method; var_name <- processed_data$last_outlier_var
    if(method=="remove"){
      data <- data[-indices,]; log_entry <- paste("🗑️ Hapus",length(indices),"outlier dari",var_name)
    } else {
      clean_values <- data[[var_name]][-indices]; log_method_text <- ""
      if(method=="cap"){
        valid_values<-data[[var_name]][!is.na(data[[var_name]])]; Q1<-quantile(valid_values,0.25); Q3<-quantile(valid_values,0.75); lower<-Q1-1.5*(Q3-Q1); upper<-Q3+1.5*(Q3-Q1)
        outlier_values <- data[indices, var_name]; data[indices, var_name] <- ifelse(outlier_values > upper, upper, lower); log_method_text <- "Imputasi (Cap)"
      } else if (method=="mean"){ data[indices, var_name] <- mean(clean_values,na.rm=T); log_method_text <- "Imputasi (Mean)"
      } else if (method=="median"){ data[indices, var_name] <- median(clean_values,na.rm=T); log_method_text <- "Imputasi (Median)"
      } else if (method=="mode"){ calculate_mode<-function(x){ux<-unique(x[!is.na(x)]);ux[which.max(tabulate(match(x,ux)))]}; data[indices, var_name] <- calculate_mode(clean_values); log_method_text <- "Imputasi (Modus)"}
      log_entry <- paste("🛠️", log_method_text, length(indices), "outlier pada", var_name)
    }
    processed_data$current<-data; processed_data$outliers<-NULL; processed_data$last_outlier_var<-NULL
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    shiny::showNotification("✅ Penanganan outlier berhasil!", type="default")
  }, error=function(e){shiny::showNotification(paste("❌ Error:",e$message), type="error")})
})
output$dynamic_filter_controls <- renderUI({
  req(input$filter_var, processed_data$current); var_name <- input$filter_var; values <- processed_data$current[[var_name]]
  if(is.numeric(values)){min_val<-min(values,na.rm=T);max_val<-max(values,na.rm=T);sliderInput("filter_range","Rentang nilai:",min=min_val,max=max_val,value=c(min_val,max_val))}else{unique_vals<-sort(unique(values[!is.na(values)]));checkboxGroupInput("filter_categories","Pilih kategori:",choices=unique_vals,selected=unique_vals)}
})
observeEvent(input$btn_filter, {
  req(processed_data$current, input$filter_var)
  tryCatch({
    data <- processed_data$current; var_name <- input$filter_var; values <- data[[var_name]]
    if(is.numeric(values)){req(input$filter_range); rows_to_keep <- !is.na(values)&values>=input$filter_range[1]&values<=input$filter_range[2]; filter_desc <- paste("Rentang",input$filter_range[1],"-",input$filter_range[2])}else{req(input$filter_categories); rows_to_keep <- !is.na(values)&values%in%input$filter_categories; filter_desc <- paste("Kategori:",paste(input$filter_categories,collapse=", "))}
    processed_data$current <- data[rows_to_keep,]; log_entry <- paste("🔽 Filter:",var_name,"-",filter_desc,"- hasil:",nrow(processed_data$current),"baris")
    processed_data$operation_log <- c(processed_data$operation_log, log_entry)
    shiny::showNotification("🔽 Filter diterapkan!", type="default")
  }, error=function(e){shiny::showNotification(paste("❌ Error:",e$message),type="error")})
})
observeEvent(input$btn_reset_filter, { if(!is.null(processed_data$original)){processed_data$current<-processed_data$original;log_entry<-"🔄 Filter direset";processed_data$operation_log<-c(processed_data$operation_log,log_entry);shiny::showNotification("Filter direset!",type="default")}})
observeEvent(input$btn_reset_all, { if(!is.null(processed_data$original)){processed_data$current<-processed_data$original;processed_data$operations<-list();processed_data$operation_log<-character(0);processed_data$outliers<-NULL;processed_data$last_outlier_var<-NULL;shiny::showNotification("Semua perubahan direset!",type="default")}})

# ===== TABEL PREVIEW DATA & STATS =====
output$data_preview_table <- DT::renderDataTable({ req(processed_data$current); DT::datatable(processed_data$current, options = list(scrollX = TRUE, pageLength = 5), filter = 'top') })

# ===== LOG OPERASI =====
output$operation_log <- renderText({
  if (length(processed_data$operation_log) > 0) {
    paste("=== LOG OPERASI DATA ===\n", paste(rev(processed_data$operation_log), collapse = "\n"), sep = "\n")
  } else { "Belum ada operasi yang dilakukan." }
})


# ===== DOWNLOAD DATA =====
output$btn_export_data <- downloadHandler(
  filename = function() { paste("processed_data_", Sys.Date(), ".csv", sep = "") },
  content = function(file) { if (!is.null(processed_data$current)) write.csv(processed_data$current, file, row.names = FALSE) }
)

# Custom infix operator
`%||%` <- function(a, b) { if (!is.null(a)) a else b }