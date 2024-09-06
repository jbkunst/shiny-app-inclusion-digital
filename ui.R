bslib::page_navbar(
  title = "Inclusión Digital",
  theme = app_theme,
  lang = "es",
  sidebar = sidebar_app,
  nav_panel(
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
