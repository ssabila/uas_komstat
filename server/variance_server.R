# --- 1. UI DINAMIS ---
output$variance_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("variance_var", "Pilih Variabel Numerik:", choices = numeric_vars)
})

output$variance_group_selector <- renderUI({
  req(processed_data$current, input$variance_test_type == "two_sample")
  data <- processed_data$current
  valid_group_vars <- sapply(names(data), function(col_name) {
    x <- data[[col_name]]
    if (!is.character(x) && !is.factor(x)) return(FALSE)
    clean_x <- x[!is.na(x)]; if (length(clean_x) == 0) return(FALSE)
    if (length(unique(clean_x)) != 2) return(FALSE)
    return(TRUE)
  })
  cat_vars <- names(data)[valid_group_vars]
  if (length(cat_vars) == 0) cat_vars <- c("Tidak ada variabel grup yang valid" = "")
  selectInput("variance_group_var", "Pilih Variabel Grup (2 Kategori):", choices = cat_vars)
})

# --- 2. FUNGSI UJI VARIANCE ---
perform_variance_test <- function(data, variable, test_type, sigma2=NULL, group_var=NULL, alternative="two.sided", alpha=0.05) {
  if (test_type == "one_sample") {
    values <- data[[variable]][!is.na(data[[variable]])]; n <- length(values)
    if (n < 2) stop("Data tidak cukup untuk menghitung varians.")
    sample_var <- var(values); chi_stat <- (n - 1) * sample_var / sigma2
    p_value <- if(alternative=="two.sided"){2*min(pchisq(chi_stat,df=n-1),1-pchisq(chi_stat,df=n-1))}else if(alternative=="greater"){1-pchisq(chi_stat,df=n-1)}else{pchisq(chi_stat,df=n-1)}
    ci <- if(alternative=="two.sided"){c((n-1)*sample_var/qchisq(1-alpha/2,df=n-1),(n-1)*sample_var/qchisq(alpha/2,df=n-1))}else{c(ifelse(alternative=="greater",(n-1)*sample_var/qchisq(1-alpha,df=n-1),0),ifelse(alternative=="less",(n-1)*sample_var/qchisq(alpha,df=n-1),Inf))}
    return(list(test_type="Chi-square test for variance",statistic=chi_stat,df=n-1,p_value=p_value,sample_var=sample_var,hypothesized_var=sigma2,conf_int=ci,alternative=alternative,alpha=alpha,n=n))
  } else {
    complete_data<-data[complete.cases(data[c(variable,group_var)]),]; groups<-unique(complete_data[[group_var]])
    d1<-complete_data[[variable]][complete_data[[group_var]]==groups[1]]; d2<-complete_data[[variable]][complete_data[[group_var]]==groups[2]]
    if(length(d1)<2||length(d2)<2)stop("Setiap grup harus memiliki minimal 2 observasi.")
    var1<-var(d1);var2<-var(d2);n1<-length(d1);n2<-length(d2);f_stat<-var1/var2
    p_value<-if(alternative=="two.sided"){2*min(pf(f_stat,df1=n1-1,df2=n2-1),1-pf(f_stat,df1=n1-1,df2=n2-1))}else if(alternative=="greater"){1-pf(f_stat,df1=n1-1,df2=n2-1)}else{pf(f_stat,df1=n1-1,df2=n2-1)}
    ci<-if(alternative=="two.sided"){c(f_stat/qf(1-alpha/2,df1=n1-1,df2=n2-1),f_stat/qf(alpha/2,df1=n1-1,df2=n2-1))}else{c(ifelse(alternative=="greater",f_stat/qf(1-alpha,df1=n1-1,df2=n2-1),0),ifelse(alternative=="less",f_stat/qf(alpha,df1=n1-1,df2=n2-1),Inf))}
    return(list(test_type="F-test for equality of variances",statistic=f_stat,df1=n1-1,df2=n2-1,p_value=p_value,var1=var1,var2=var2,group1=groups[1],group2=groups[2],n1=n1,n2=n2,conf_int=ci,alternative=alternative,alpha=alpha))
  }
}

# --- 3. LOGIKA UJI VARIANCE ---
variance_results <- eventReactive(input$run_variance_test, {
  req(processed_data$current, input$variance_var, input$variance_test_type)
  result <- tryCatch({
    if (input$variance_test_type == "one_sample") {
      req(input$variance_sigma2)
      perform_variance_test(data=processed_data$current, variable=input$variance_var, test_type="one_sample", sigma2=input$variance_sigma2, alternative=input$variance_alternative, alpha=input$variance_alpha)
    } else {
      req(input$variance_group_var, nzchar(input$variance_group_var))
      perform_variance_test(data=processed_data$current, variable=input$variance_var, test_type="two_sample", group_var=input$variance_group_var, alternative=input$variance_alternative, alpha=input$variance_alpha)
    }
  }, error = function(e) { showNotification(paste("Error:", e$message), type = "error"); NULL })
  analysis_results$variance <- result
  return(result)
})

# --- 4. FUNGSI PEMBANTU (HELPER FUNCTIONS) UNTUK TEKS ---
generate_variance_summary_text <- function(result) {
  req(result)
  summary_lines <- c(
    "=== HASIL UJI VARIANS ===",
    paste("Uji:", result$test_type)
  )
  if (result$test_type == "Chi-square test for variance") {
    summary_lines <- c(summary_lines,
                       paste("H₀: σ² =", result$hypothesized_var),
                       paste("H₁: σ²", switch(result$alternative, "two.sided"="≠", "greater"=">", "less"="<"), result$hypothesized_var),
                       "",
                       paste("Statistik Chi-square:", round(result$statistic, 4)),
                       paste("Derajat bebas:", result$df),
                       paste("P-value:", round(result$p_value, 6))
    )
  } else {
    summary_lines <- c(summary_lines,
                       "H₀: σ₁² = σ₂² (variance sama)",
                       paste("H₁: σ₁²", switch(result$alternative, "two.sided"="≠", "greater"=">", "less"="<"), "σ₂²"),
                       "",
                       paste("Statistik F:", round(result$statistic, 4)),
                       paste("Derajat bebas:", result$df1, ",", result$df2),
                       paste("P-value:", round(result$p_value, 6))
    )
  }
  return(paste(summary_lines, collapse = "\n"))
}

generate_variance_interpretation_text <- function(result) {
  req(result, !is.null(result$test_type))
  p_val <- result$p_value; alpha <- result$alpha
  decision_text <- sprintf("KEPUTUSAN: Karena p-value (%.4f) %s dari alpha (%.2f), maka H₀ %s.", p_val, ifelse(p_val < alpha, "<", "≥"), alpha, ifelse(p_val < alpha, "DITOLAK", "GAGAL DITOLAK"))
  
  if (result$test_type == "Chi-square test for variance") {
    conclusion_text <- if(p_val<alpha){sprintf("KESIMPULAN: Dengan tingkat kepercayaan %d%%, data memberikan cukup bukti untuk menyatakan\nbahwa varians populasi (σ²) secara signifikan berbeda dari nilai hipotesis (%.2f).", (1-alpha)*100, result$hypothesized_var)}else{sprintf("KESIMPULAN: Dengan tingkat kepercayaan %d%%, data tidak memberikan cukup bukti untuk menyatakan\nbahwa varians populasi (σ²) berbeda dari nilai hipotesis (%.2f).",(1-alpha)*100,result$hypothesized_var)}
  } else {
    conclusion_text <- if(p_val<alpha){sprintf("KESIMPULAN: Dengan tingkat kepercayaan %d%%, data memberikan cukup bukti untuk menyatakan\nbahwa terdapat perbedaan varians yang signifikan antara grup '%s' dan '%s'.",(1-alpha)*100, result$group1,result$group2)}else{sprintf("KESIMPULAN: Dengan tingkat kepercayaan %d%%, data tidak memberikan cukup bukti untuk menyatakan\nadanya perbedaan varians yang signifikan antara grup '%s' dan '%s'.",(1-alpha)*100,result$group1,result$group2)}
  }
  return(paste("=== INTERPRETASI HASIL UJI STATISTIK ===\n\n", decision_text, "\n\n", conclusion_text))
}

generate_variance_vis_interpretation_text <- function(result) {
  req(result, !is.null(result$test_type))
  p_val <- result$p_value; alpha <- result$alpha
  if (result$test_type == "Chi-square test for variance") {
    text <- if(p_val<alpha){sprintf("Hasil uji statistik menunjukkan varians sampel (%.2f) berbeda signifikan dari varians hipotesis (%.2f).\nHal ini mungkin terlihat dari lebar boxplot atau sebaran histogram yang tidak sesuai dengan yang diharapkan.",result$sample_var,result$hypothesized_var)}else{sprintf("Hasil uji statistik tidak menemukan perbedaan signifikan antara varians sampel (%.2f) dan varians hipotesis (%.2f).\nSecara visual, lebar boxplot dan sebaran histogram konsisten dengan hasil ini.",result$sample_var,result$hypothesized_var)}
  } else {
    text <- if(p_val<alpha){larger_group <- if(result$var1 > result$var2) result$group1 else result$group2; sprintf("Hasil F-test (p=%.4f) menunjukkan ada perbedaan varians yang signifikan.\nIni didukung secara visual, di mana boxplot grup '%s' tampak lebih lebar,\nmenandakan sebaran data yang lebih besar.", p_val, larger_group)}else{sprintf("Hasil F-test (p=%.4f) menunjukkan tidak ada perbedaan varians yang signifikan.\nSecara visual, boxplot untuk kedua grup memiliki lebar dan sebaran yang relatif sama,\nsejalan dengan hasil uji.",p_val)}
  }
  return(paste("ANALISIS VISUAL:\n", text))
}


# --- 5. RENDER OUTPUT KE UI ---
output$variance_test_result <- renderPrint({ cat(generate_variance_summary_text(variance_results())) })
output$variance_interpretation <- renderText({ generate_variance_interpretation_text(variance_results()) })
output$variance_visualization_interpretation <- renderText({ generate_variance_vis_interpretation_text(variance_results()) })


# --- 6. STATISTIK DESKRIPTIF & VISUALISASI ---
output$variance_descriptive_stats <- DT::renderDataTable({
  req(variance_results(), input$variance_var); data <- processed_data$current
  if (input$variance_test_type=="one_sample"){df<-data.frame(Statistik=c("N","Mean","Variance","Std Dev"),Nilai=c(length(data[[input$variance_var]][!is.na(data[[input$variance_var]])]),round(mean(data[[input$variance_var]],na.rm=T),4),round(var(data[[input$variance_var]],na.rm=T),4),round(sd(data[[input$variance_var]],na.rm=T),4)))}else{req(input$variance_group_var);groups<-unique(data[[input$variance_group_var]]);d1<-data[[input$variance_var]][data[[input$variance_group_var]]==groups[1]];d2<-data[[input$variance_var]][data[[input$variance_group_var]]==groups[2]];df<-data.frame(Statistik=c("N","Mean","Variance","Std Dev"),G1=c(length(d1),round(mean(d1),4),round(var(d1),4),round(sd(d1),4)),G2=c(length(d2),round(mean(d2),4),round(var(d2),4),round(sd(d2),4)));names(df)[2:3]<-groups}
  DT::datatable(df, options=list(dom='t'), rownames=FALSE)
})
output$variance_boxplot <- renderPlot({
  req(processed_data$current, input$variance_var); data <- processed_data$current
  if (input$variance_test_type=="one_sample"){boxplot(data[[input$variance_var]],main=paste("Boxplot untuk",input$variance_var),ylab=input$variance_var,col="lightblue")}else{req(input$variance_group_var);boxplot(data[[input$variance_var]]~data[[input$variance_group_var]],main=paste("Boxplot",input$variance_var,"berdasarkan",input$variance_group_var),xlab=input$variance_group_var,ylab=input$variance_var,col=c("lightblue","lightcoral"))}
})
output$variance_histogram <- renderPlot({
  req(processed_data$current, input$variance_var); data <- processed_data$current
  if (input$variance_test_type=="one_sample"){hist(data[[input$variance_var]],main=paste("Histogram untuk",input$variance_var),xlab=input$variance_var,col="lightblue",breaks=20)}else{req(input$variance_group_var);groups<-unique(data[[input$variance_group_var]]);par(mfrow=c(1,2));for(i in 1:2){hist(data[[input$variance_var]][data[[input$variance_group_var]]==groups[i]],main=paste("Histogram",groups[i]),xlab=input$variance_var,col=c("lightblue","lightcoral")[i],breaks=15)};par(mfrow=c(1,1))}
})


# --- 7. LOGIKA UNDUH ---
# PERBAIKAN UNTUK server/variance_server.R
# Bagian: LOGIKA UNDUH (Ganti bagian yang ada dengan kode ini)

# PERBAIKAN UNTUK server/variance_server.R
# Bagian: LOGIKA UNDUH (Ganti bagian yang ada dengan kode ini)

output$download_variance_result <- downloadHandler(
  filename = function() {
    paste("laporan-uji-varians-", Sys.Date(), ".", input$variance_format, sep = "")
  },
  content = function(file) {
    req(variance_results())
    
    # TAMBAHKAN: Progress notification untuk user experience yang lebih baik
    showNotification("📋 Sedang menyiapkan laporan...", type = "message", duration = 3)
    
    # Gunakan pendekatan yang sama dengan assumptions - buat R Markdown secara langsung
    temp_dir <- tempdir()
    plot_file <- file.path(temp_dir, "variance_plots.png")
    
    # PERBAIKAN: Buat plot dengan resolusi rendah untuk kecepatan
    tryCatch({
      png(plot_file, width = 800, height = 600, res = 150)
      
      data <- processed_data$current
      if (input$variance_test_type == "one_sample") {
        # Layout untuk satu plot
        boxplot(data[[input$variance_var]], 
                main = paste("Boxplot untuk", input$variance_var),
                ylab = input$variance_var, 
                col = "lightblue")
      } else {
        req(input$variance_group_var)
        # Layout untuk dua plot
        par(mfrow = c(1, 2))
        
        # Boxplot
        boxplot(data[[input$variance_var]] ~ data[[input$variance_group_var]],
                main = paste("Boxplot", input$variance_var),
                xlab = input$variance_group_var,
                ylab = input$variance_var,
                col = c("lightblue", "lightcoral"))
        
        # Histogram
        groups <- unique(data[[input$variance_group_var]])
        hist(data[[input$variance_var]][data[[input$variance_group_var]] == groups[1]],
             main = paste("Histogram", groups[1]),
             xlab = input$variance_var,
             col = "lightblue",
             breaks = 15)
        
        par(mfrow = c(1, 1))
      }
      dev.off()
      
    }, error = function(e) {
      # Plot fallback
      png(plot_file, width = 800, height = 600, res = 150)
      plot(1, 1, type = "n", main = "Plot Tidak Tersedia", xlab = "", ylab = "", axes = FALSE)
      text(1, 1, paste("Error:", e$message), col = "red", cex = 1.2)
      dev.off()
    })
    
    showNotification("📊 Sedang memproses hasil analisis...", type = "message", duration = 2)
    
    # Ambil hasil analisis
    result <- variance_results()
    result_text <- generate_variance_summary_text(result)
    interpretation_text <- generate_variance_interpretation_text(result)
    vis_interpretation_text <- generate_variance_vis_interpretation_text(result)
    
    # Format output seperti assumptions
    output_format <- switch(input$variance_format,
                            "pdf" = "pdf_document",
                            "docx" = "word_document")
    
    temp_rmd <- tempfile(fileext = ".Rmd")
    
    # PERBAIKAN: Gunakan format R Markdown langsung seperti assumptions
    rmd_content <- paste(
      "---",
      "title: 'Laporan Hasil Uji Varians'",
      paste0("date: '", format(Sys.time(), "%A, %d %B %Y %H:%M:%S"), "'"),
      "output:",
      if(input$variance_format == "pdf") {
        "  pdf_document:\n    latex_engine: xelatex\n    toc: true"
      } else if(input$variance_format == "docx") {
        "  word_document:\n    toc: true"
      },
      "---",
      "",
      "## 1. Ringkasan Analisis",
      paste("- **Jenis Uji:**", if(input$variance_test_type == "one_sample") "Uji Varians Satu Sampel" else "Uji Varians Dua Sampel"),
      paste("- **Variabel yang diuji:**", input$variance_var),
      if(input$variance_test_type != "one_sample") paste("- **Variabel Grup:**", input$variance_group_var) else "",
      paste("- **Tingkat Signifikansi (α):** 0.05"),
      "",
      "## 2. Hasil Pengujian",
      "```",
      result_text,
      "```",
      "",
      "## 3. Interpretasi Hasil",
      interpretation_text,
      "",
      "## 4. Visualisasi Data",
      if(file.exists(plot_file)) paste0("![Visualisasi Distribusi Data](", plot_file, ")") else "Plot tidak tersedia.",
      "",
      "## 5. Interpretasi Visual",
      vis_interpretation_text,
      "",
      "---",
      paste0("*Laporan ini dibuat secara otomatis pada ", format(Sys.time(), "%d %B %Y, %H:%M:%S"), ".*"),
      sep = "\n"
    )
    
    writeLines(rmd_content, temp_rmd)
    
    showNotification("🔄 Sedang menyusun dokumen final...", type = "message", duration = 2)
    
    # PERBAIKAN: Render dengan error handling yang baik
    tryCatch({
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        output_format = output_format,
        quiet = TRUE  # Mencegah output verbose
      )
      showNotification("✅ Laporan berhasil dibuat!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("❌ Error membuat laporan:", e$message), type = "error", duration = 10)
      # Fallback: buat file teks sederhana
      writeLines(c(
        paste("LAPORAN UJI VARIANS -", Sys.Date()),
        paste(rep("=", 50), collapse = ""),
        "",
        "HASIL UJI STATISTIK:",
        result_text,
        "",
        "INTERPRETASI:",
        interpretation_text,
        "",
        "INTERPRETASI VISUAL:",
        vis_interpretation_text
      ), file)
    })
    
    # Bersihkan file temporer
    if(file.exists(plot_file)) unlink(plot_file)
    if(file.exists(temp_rmd)) unlink(temp_rmd)
  }
)