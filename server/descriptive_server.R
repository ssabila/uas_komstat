# Membuat tabel statistik deskriptif
output$descriptive_table <- DT::renderDataTable({
  req(processed_data$current)
  stats <- calculate_descriptive_stats(processed_data$current, variables = NULL)
  
  stats_df <- do.call(rbind, lapply(names(stats), function(var) {
    data.frame(
      Variabel = var,
      N = stats[[var]]$n,
      Mean = round(stats[[var]]$mean, 2),
      Median = round(stats[[var]]$median, 2),
      SD = round(stats[[var]]$sd, 2),
      Min = round(stats[[var]]$min, 2),
      Max = round(stats[[var]]$max, 2)
    )
  }))
  
  # Simpan hasil untuk diunduh
  analysis_results$descriptive <- stats_df
  
  DT::datatable(stats_df, options = list(scrollX = TRUE), rownames = FALSE)
})

# Membuat interpretasi otomatis
output$descriptive_interpretation <- renderText({
  "Interpretasi:\n- Mean & Median: Perbandingan nilai mean dan median dapat memberikan indikasi awal kemiringan (skewness) distribusi data.\n- Standar Deviasi (SD): Nilai SD yang besar menandakan sebaran data yang luas."
})

# Logika unduh untuk tabel deskriptif
output$download_descriptive <- downloadHandler(
  filename = function() {
    paste("statistik-deskriptif-", Sys.Date(), ".", input$descriptive_format, sep = "")
  },
  content = function(file) {
    req(analysis_results$descriptive)
    
    # Konversi tabel dataframe menjadi teks yang diformat dengan knitr::kable
    table_as_text <- knitr::kable(analysis_results$descriptive, format = "pandoc", caption = "Tabel Statistik Deskriptif")
    text_to_render <- paste(table_as_text, collapse = "\n")
    
    # Render laporan menggunakan template
    rmarkdown::render(
      input = "text_report.Rmd",
      output_file = file,
      output_format = if(input$descriptive_format == "pdf") "pdf_document" else "word_document",
      params = list(
        report_title = "Hasil Statistik Deskriptif",
        text_output = text_to_render
      ),
      envir = new.env(parent = globalenv())
    )
  }
)