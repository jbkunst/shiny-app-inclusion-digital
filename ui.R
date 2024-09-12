bslib::page_navbar(
  title = "Índice de Inclusión Digital",
  id = "nav",
  theme = app_theme,
  lang = "es",
  sidebar = sidebar_app,
  nav_panel(
    includeScript("www/custom.js"),
    title = "Comunas",
    uiOutput("comuna_boxes")
  ),
  nav_panel(
    title = "Regiones",
    uiOutput("region_boxes")
  ),
  nav_panel(
    title = "Metodología",
    tags$h2("Metodología")
  ),
)
