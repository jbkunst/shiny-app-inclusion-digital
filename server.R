function(input, output, session) {

  # modal bienvenida --------------------------------------------------------
  showModal(
    modalDialog(
      # title = tags$small("Bienvenido al"),
      tags$p("Bienvenido al"),
      tags$h1("Índice inclusión Digital"),
      tags$br(),
      tags$p("Definición o texto introductorio del índice..."),
      easyClose = TRUE,
      footer = NULL
    )
  )

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

    value_boxes <- data_filtrada |>
      select(comuna, region, valor = indice_inclusion_digital, codigo = codigo_comuna, v1, v2, v3) |>
      purrr::pmap(function(comuna, region, valor, codigo, v1, v2, v3){

        # comuna <- "Copiapó"
        # region <- "Atacama"
        # valor <- runif(1)
        # codigo <- "03101"
        # v1 <- runif(1); v2 <- runif(1); v3 <- runif(1)


        lc1 <- layout_columns(
          col_widths = c(6, 6, 12),
          # fill = FALSE, fillable = FALSE,
          col(
            style="height: 100%;position: relative",
            tags$h5(style = "position: absolute;bottom: 0", "Indicador acceso")
          ),
          col(
            style = "text-align: right",
            tags$small(categorizar_indicador(valor)),
            tags$h1(formatear_numero(valor))
          ),
          col(horizontal_gauge_html(percent = valor, height = 10), tags$br()),
        )

        lc2 <- layout_columns(
          col_widths = c(6, 6, 12),
          # fill = FALSE, fillable = FALSE,
          col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Subindicador 1")),
          col(style = "text-align: right",tags$small(categorizar_indicador(v1)), tags$h1(formatear_numero(v1))),
          col(horizontal_gauge_html(percent = v1, height = 10), tags$br()),
          col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Subindicador 2")),
          col(style = "text-align: right",tags$small(categorizar_indicador(v2)), tags$h1(formatear_numero(v2))),
          col(horizontal_gauge_html(percent = v2, height = 10), tags$br()),
          col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Subindicador 3")),
          col(style = "text-align: right",tags$small(categorizar_indicador(v3)), tags$h1(formatear_numero(v3))),
          horizontal_gauge_html(percent = v3, height = 10)
        )

        c <- card(
          style = str_glue("background-color:{colores$ahuesado}; color: {colores$gris}"),
          # tags$small(region),
          tags$h2(tags$strong(str_to_upper(comuna))),
          lc1,
          tags$div(id = str_glue("comuna{codigo}"), class = "collapse", lc2),
          tags$button(
            "Ver detalle",
            onclick = str_glue("$('#comuna{codigo}').collapse('toggle'); this.textContent = this.textContent === 'Ver detalle' ? 'Cerrar detalle' : 'Ver detalle';"),
            class = "btn btn-primary btn-md",
            style = "max-width:160px"
          ),
        )

        c
        # lc2 |> as.character() |> cat()

        c

        # c |> as.character() |> cat()

        # htmltools::tagQuery(c)$find(".card-body")$removeClass("bslib-gap-spacing")$allTags()


      })

    layout_columns(
      col_widths = 6,
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
