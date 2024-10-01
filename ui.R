bslib::page_navbar(
  title = tags$span(tags$span("Resultados", style = "font-size:12px;font-weight:300"), tags$br(), "Índice de Inclusión Digital") ,
  id = "nav",
  theme = app_theme,
  bg = colores$gris,
  # fg = colores$ahuesado,
  lang = "es",
  sidebar = sidebar_app,
  nav_spacer(),
  nav_panel(
    includeScript("www/custom.js"),
    title = "Resultados por comuna",
    uiOutput("comuna_boxes")
  ),
  nav_panel(
    title = "Resultados por región",
    uiOutput("region_boxes")
  ),
  nav_panel(
    title = "Metodología",
    tags$h2("Metodología")
  ),
)
