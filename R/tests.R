library(bslib)
library(bsicons)

source("R/helpers.R")

card(
  tags$b("Isla de Maipo"),
  tags$small("Región Metropolitana"),
  tags$h2("66", class = "pull-right text-right"),
  horizontal_gauge_html(percent = 0.66)
)

value_box(
  title = "Isla de Maipo",
  showcase = tags$p(66, style = "font-size:4rem"),
  showcase_layout = "top right",
  # value = 66,
  tags$p("Región Metropolitana"),
  horizontal_gauge_html(percent = 0.66)
)

card(
  value_box(
    title = tags$p("Región Metropolitana"),
    value = "Isla de Maipo",
    style = "border:none",
    showcase = tags$p(66, style = "font-size:4rem"),
    showcase_layout = "top right",
  ),
  horizontal_gauge_html(percent = 0.66),
  tags$small(tags$button("Ver detalles"))
)
