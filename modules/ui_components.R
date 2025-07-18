# Fungsi untuk membuat info box kustom
create_info_box <- function(title, value, subtitle, icon_name, color = "blue") {
  infoBox(
    title = title,
    value = value,
    subtitle = subtitle,
    icon = icon(icon_name),
    color = color,
    width = 12
  )
}

# Fungsi untuk membuat box dengan header kustom
create_custom_box <- function(title, content, status = "primary", width = 12, collapsible = FALSE) {
  box(
    title = title,
    status = status,
    solidHeader = TRUE,
    width = width,
    collapsible = collapsible,
    content
  )
}

# Fungsi untuk membuat selectInput dengan choices yang dinamis
create_variable_selector <- function(inputId, label, choices = NULL, selected = NULL) {
  selectInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected
  )
}

# Fungsi untuk membuat action button dengan style kustom
create_action_button <- function(inputId, label, class = "btn-primary") {
  actionButton(
    inputId = inputId,
    label = label,
    class = class
  )
}

# Fungsi untuk membuat tabset panel dengan konten
create_analysis_tabs <- function(tab_list) {
  do.call(tabsetPanel, tab_list)
}
