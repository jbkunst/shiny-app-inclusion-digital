function(input, output, session) {

  data_filtrada <- reactive({
    data
  })

  output$boxes <- renderUI({
    # data_filtrada <- data
    data_filtrada <- data_filtrada()

    value_boxes <- data_filtrada |>
      select(comuna = Comuna, region = Región, valor = `Valor Índice de inclusión digital`, codigo = `Código Comuna`) |>
      purrr::pmap(function(comuna, region, valor, codigo){

        # comuna <- "Copiapó"
        # region <- "Atacama"
        # valor <- 0.994
        # codigo <- "03101"

        card(
          value_box(
            title = region,
            value = comuna,
            style = "border:none",
            showcase = tags$p(round(valor * 100), style = "font-size:4rem"),
            showcase_layout = "top right",
          ),
          horizontal_gauge_html(percent = valor),
          tags$button(
            "Ver detalles",
            onclick = stringr::str_glue("(function(){{
              Shiny.onInputChange('codigo', {{valor: '{codigo}', nonce: Math.random()}})
            }})()"),
            class = "btn btn-primary btn-sm"
          )
        )

      })

    layout_column_wrap(
      width = 1/2,
      fillable = TRUE,
      fill = TRUE,
      !!!value_boxes
      )

  })

  observeEvent(input$codigo, {

    cli::cli_inform(input$codigo)
    v <- input$codigo$valor
    # v <- "03101"

    vals <- data |>
      filter(`Código Comuna` == v) |>
      as.list()

    showModal(
      modalDialog(
        title = vals$Comuna,
        size = "s",
        # footer = modalButton("Cerrar"),
        footer = tags$button(type = "button", class = "btn btn-primary",
                             `data-dismiss` = "modal",
                             `data-bs-dismiss` = "modal",
                             "Cerrar")
      )
    )

  })

}
