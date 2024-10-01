function(input, output, session) {

  # modal bienvenida --------------------------------------------------------
  # showModal(
  #   modalDialog(
  #     # title = tags$small("Bienvenido al"),
  #     tags$p("Bienvenido al"),
  #     tags$h1("Índice inclusión Digital"),
  #     tags$br(),
  #     tags$p("Definición o texto introductorio del índice..."),
  #     easyClose = TRUE,
  #     footer = NULL
  #   )
  # )

  data_filtrada <- reactive({
    data |>
      filter(input$habitantes[1]        <= habitantes                 , habitantes                  <= input$habitantes[2]       ) |>
      filter(input$indice_desarrollo[1] <= indice_de_desarrollo_humano, indice_de_desarrollo_humano <= input$indice_desarrollo[2]) |>
      filter(TRUE)
  })

  # comuna ------------------------------------------------------------------
  output$comuna_boxes <- renderUI({
    # data_filtrada <- data
    data_filtrada <- data_filtrada()

    cli::cli_inform("Partiendo value_boxes")

    value_boxes <- data_filtrada |>
      select(comuna, region, codigo_comuna, v, v_cat, v1, v1_cat, v2, v2_cat, v3, v3_cat) |>
      purrr::pmap(function(comuna, region, codigo_comuna, v, v_cat, v1, v1_cat, v2, v2_cat, v3, v3_cat){

        # cli::cli_inform(comuna)
        # comuna <- "Copiapó"
        # region <- "Atacama"
        # codigo_comuna <- "03101"
        # v <- v1 <- v2 <- v3 <- 0.432
        # v_cat <- v1_cat <- v2_cat <- v3_cat <- "Alto"

        lc1 <- layout_columns(
          col_widths = c(6, 6, 12),
          # fill = FALSE, fillable = FALSE,
          col(
            style="height: 100%;position: relative",
            tags$h5(style = "position: absolute;bottom: 0", "Indicador acceso")
          ),
          col(
            style = "text-align: right",
            tags$small(coalesce(v_cat, "-")),
            tags$h1(formatear_numero(v))
          ),
          col(horizontal_gauge_html(percent = v, height = 10), tags$br()),
        )

        lc2 <- layout_columns(
          col_widths = c(6, 6, 12),
          # fill = FALSE, fillable = FALSE,
          col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Subindicador 1")),
          col(style = "text-align: right",tags$small(coalesce(v1_cat, "-")), tags$h1(formatear_numero(v1))),
          col(horizontal_gauge_html(percent = v1, height = 10), tags$br()),
          col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Subindicador 2")),
          col(style = "text-align: right",tags$small(coalesce(v2_cat, "-")), tags$h1(formatear_numero(v2))),
          col(horizontal_gauge_html(percent = v2, height = 10), tags$br()),
          col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Subindicador 3")),
          col(style = "text-align: right",tags$small(coalesce(v3_cat, "-")), tags$h1(formatear_numero(v3))),
          horizontal_gauge_html(percent = v3, height = 10)
        )

        c <- card(
          style = str_glue("background-color:{colores$ahuesado}; color: {colores$gris}"),
          # tags$small(region),
          tags$h2(tags$strong(str_to_upper(comuna))),
          lc1,
          tags$div(id = str_glue("comuna{codigo_comuna}"), class = "collapse", lc2),
          tags$button(
            "Ver subindicadores",
            onclick = str_glue("$('#comuna{codigo_comuna}').collapse('toggle'); this.textContent = this.textContent === 'Ver subindicadores' ? 'Cerrar detalle' : 'Ver subindicadores';"),
            class = "btn btn-primary btn-md",
            style = "max-width:200px"
          ),
        )

        c
        # lc2 |> as.character() |> cat()

        c

        # c |> as.character() |> cat()

        # htmltools::tagQuery(c)$find(".card-body")$removeClass("bslib-gap-spacing")$allTags()


      })

    cli::cli_inform("Terminando value_boxes")

    layout_columns(
      col_widths = 4,
      fill = FALSE, fillable = FALSE,
      !!!value_boxes
      )

  })

  # observeEvent(input$codigo, {
  #
  #   cli::cli_inform(input$codigo)
  #   v <- input$codigo$valor
  #   # v <- "03101"
  #
  #   vals <- data |>
  #     filter(`Código Comuna` == v) |>
  #     as.list()
  #
  #   showModal(
  #     modalDialog(
  #       title = vals$Comuna,
  #       size = "s",
  #       # footer = modalButton("Cerrar"),
  #       footer = tags$button(type = "button", class = "btn btn-primary",
  #                            `data-dismiss` = "modal",
  #                            `data-bs-dismiss` = "modal",
  #                            "Cerrar")
  #     )
  #   )
  #
  # })

}
