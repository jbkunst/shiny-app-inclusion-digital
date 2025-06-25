bslib::page_navbar(
  window_title = "Índice de Digitalización Comunal",
  # title = tags$span(tags$span("Resultados", style = "font-size:12px;font-weight:300"), tags$br(), "Índice de Digitalización Comunal") ,
  title = tags$img(src = "banner.svg", height = "50px"),
  id = "nav",
  theme = app_theme,
  bg = colores$gris,
  # fg = colores$ahuesado,
  lang = "es",
  sidebar = sidebar_app,
  nav_spacer(),
  nav_panel(
    title = "Resultados generales",
    useBusyIndicators(),
    tags$head(
      tags$link(href = "Favicon_nudos.png", rel = "icon"),
      tags$link(rel = "stylesheet", type = "text/css", href = str_glue("custom.css?id={round(as.numeric(Sys.time()))}"))
    ),
    tags$head(tags$style(HTML(""))),
    tags$head(tags$script(HTML("setInterval(function(){Shiny.setInputValue('keepAlive', new Date());},20000);"))),
    includeScript("www/custom.js"),
    # https://www.figma.com/design/D1FRd62mmcqNQdAeZ047h7/Propuesta-Indicador?node-id=298-841&t=I5Tv0KbtIN1kn7Ub-0
    tags$h4(("Resultados Generales"), style = "margin: 0;padding: 0;"),
    tags$h6(
      style = "margin-bottom: -15px;",
      tags$b("Valores estadísticos"),
      tags$small("(se modifican según la selección de filtros)")
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(12, 12),
      # row_heights = c(1, 6),
      layout_columns(
        height = 100,
        col_widths = 3,  # Divide el espacio en 5 columnas iguales
        uiOutput("dash_vb_mean"),
        uiOutput("dash_vb_median"),
        uiOutput("dash_vb_min"),
        uiOutput("dash_vb_max"),
      ),
      tags$h6(
        style = "margin-bottom: -15px; margin-top:10px",
        tags$b("Gráficos estadísticos"),
        tags$small("(se modifican según la selección de filtros)")
      ),
      layout_columns(
        row_heights = c(6, 6),
        col_widths = c(5, 7),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$span("Distribución territorial"),
            selectInput(
              "select_var_map",
              label = NULL,
              choices = c(
                "Índice de digitalización" = "v",
                "Conectividad Hogar" = "v1",
                "Educación Digital" = "v3",
                "Municipio Digital" = "v2"
                )
              )
            ),
          leafletOutput("dash_map", width="100%", height="100%")
          ),
        layout_columns(
          col_widths = c(12, 12),
          row_heights = c(4, 2, 2),
          card(
            card_header("Índice de Inclusión Digital e Índice de Desarrollo Humano"),
            highchartOutput("dash_scatter")
            ),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$span("Distribución de valores"),
              selectInput(
                "select_var_dist",
                label = NULL,
                choices = c(
                  "Índice de digitalización" = "v",
                  "Conectividad Hogar" = "v1",
                  "Educación Digital" = "v3",
                  "Municipio Digital" = "v2"
                  )
                )
              ),
            highchartOutput("dash_dist")
            ),
          card(
            card_header("Población por Índice de Inclusión Digital"),
            highchartOutput("dash_pob_index")
          ),
        )
      )
    )
  ),
  nav_panel(
    title = "Resultados por comuna",
    tags$h4(("Resultados por Comuna"), style = "margin: 0;padding: 0;"),
    uiOutput("comuna_resultados", inline = TRUE),
    uiOutput("comuna_boxes")
  ),
  nav_panel(
    title = "Resultados por región",
    tags$h4(("Resultados por Región"), style = "margin: 0;padding: 0;"),
    uiOutput("region_resultados", inline = TRUE),
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
          tags$a(
            href="Informe_Índice Digitalización_2024.pdf",
            "Descargar Reporte",
            download=NA,
            target="_blank",
            class = "btn btn-danger"
            )
          )
        )
      )
    ),
  nav_item(tags$a("Nudos.cl", href = "https://nudos.cl", target = "_blank"))
  )
