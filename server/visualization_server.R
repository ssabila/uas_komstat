# server/visualization_server.R

# Selektor dinamis untuk UI
output$vis_variable_selector <- renderUI({
  req(processed_data$current)
  numeric_vars <- names(processed_data$current)[sapply(processed_data$current, is.numeric)]
  selectInput("vis_var", "Pilih Variabel Utama:", choices = numeric_vars)
})

output$vis_group_selector <- renderUI({
  req(processed_data$current)
  categorical_vars <- names(processed_data$current)[sapply(processed_data$current, function(x) is.character(x) || is.factor(x))]
  selectInput("vis_group", "Pilih Variabel Grup (Opsional):", choices = c("Tidak Ada", categorical_vars))
})

output$vis_plot_type_selector <- renderUI({
  selectInput("vis_plot_type", "Pilih Jenis Plot:", choices = c("Histogram", "Boxplot", "Scatter Plot"))
})

# Render plot utama
output$main_plot <- renderPlotly({
  req(input$vis_var, input$vis_plot_type, processed_data$current)
  
  p <- switch(input$vis_plot_type,
              "Histogram" = create_histogram(processed_data$current, input$vis_var),
              "Boxplot" = create_boxplot(processed_data$current, input$vis_var, group_var = if(input$vis_group == "Tidak Ada") NULL else input$vis_group),
              "Scatter Plot" = create_scatterplot(processed_data$current, x_var = names(processed_data$current)[1], y_var = input$vis_var)
  )
  ggplotly(p)
})

# Render heatmap korelasi
output$correlation_heatmap <- renderPlotly({
  req(processed_data$current)
  p <- create_correlation_heatmap(processed_data$current)
  ggplotly(p)
})