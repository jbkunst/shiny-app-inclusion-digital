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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(
      tags$style(HTML("

    "))
    ),
    includeScript("www/custom.js"),
    title = "Resultados por comuna",
    tags$h4(("Resultados por Comuna"), style = "margin: 0;padding: 0;"),
    uiOutput("comuna_resultados", inline = TRUE),
    uiOutput("comuna_boxes")
  ),
  nav_panel(
    title = "Resultados por región",
    uiOutput("region_boxes")
  ),
  nav_panel(
    title = "Metodología",
    column(
      class = "p-5",
      width = 8,
      offset = 2,
      fluidRow(
        column(
          class = "p-10",
          width = 8,
          tags$h3(tags$strong("LA METODOLOGÍA"))
        )
      ),
      fluidRow(
        column(
          width = 8,
          shiny::includeMarkdown("data/metodologia.md")
        ),
        column(
          width = 4,
          shiny::downloadButton(label = "Descargar documento", class = "btn btn-danger", outputId = "downloaddoc")
        )
      )
    )
  ),
)
