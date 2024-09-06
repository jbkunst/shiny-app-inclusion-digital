function(input, output, session) {

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

  output$comuna_boxes <- renderUI({
    # data_filtrada <- data
    data_filtrada <- data_filtrada()

    value_boxes <- data_filtrada |>
      select(comuna, region, valor = indice_inclusion_digital, codigo = codigo_comuna, v1, v2, v3) |>
      purrr::pmap(function(comuna, region, valor, codigo, v1, v2, v3){

        # comuna <- "Copiapó"
        # region <- "Atacama"
        # valor <- 0.994
        # codigo <- "03101"
        # v1 <- v2 <- v3 <- runif(1)

        card(
          value_box(
            title = region,
            value = comuna,
            style = "border:none",
            showcase = tags$p(round(valor * 100), style = "font-size:4rem"),
            showcase_layout = "top right",
          ),
          tags$h3("Indicador de Acceso"),
          horizontal_gauge_html(percent = valor, height = 15),

          accordion(
              open = FALSE,
              style = "border:none",
              accordion_panel(
                title = "Ver detalles",
                value = "asd",
                tags$p("Variable 1"),
                horizontal_gauge_html(percent = v1),

                tags$p("Variable 2"),
                horizontal_gauge_html(percent = v2),

                tags$p("Varable 3"),
                horizontal_gauge_html(percent = v3)
              )
          )

          # tags$button("Ver detalles", class="btn btn-primary", type="button", `data-bs-toggle`="collapse",
          #         `data-bs-target`= str_glue("#comuna{codigo}"),
          #         `aria-expanded`="false", `aria-controls`="collapseExample"),
          #
          # tags$div(class="collapse", id = str_glue("#comuna{codigo}"),

          # )
          # tags$button(
          #   "Ver detalles",
          #   onclick = stringr::str_glue("(function(){{
          #     Shiny.onInputChange('codigo', {{valor: '{codigo}', nonce: Math.random()}})
          #   }})()"),
          #   class = "btn btn-primary btn-sm"
          # )
        )

      })

    layout_column_wrap(
      width = 1/1,
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
