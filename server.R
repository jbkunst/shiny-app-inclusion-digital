# input <- list(habitantes = c(0, 1e10), indice_desarrollo = c(0, 1), buscar = "maipo")
function(input, output, session) {

  # modal bienvenida --------------------------------------------------------
  showModal(
    modalDialog(
      fluidRow(
        tags$div(
          class = "col-sm-10 offset-sm-1 col-md-8 offset-md-2",
          # width = 8, offset = 2, styles = "max-width: 200px;",
          tags$img(src = "2305_LogoNudos_Pos.png", width="320px", class="rounded mx-auto d-block"),
          tags$div(
            class="text-center",
            tags$p("Bienvenido al"),
            tags$h2("Índice de Digitalización Comunal")
            ),
          tags$br(),
          includeMarkdown("data/bienvenida.md"),

          tags$div(
            class="text-center",
            tags$button(
              type = "button", class = "btn btn-danger",
              style = "width: 300px",
              `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
              "Ver resultados"
            )
          )
        )
      ),
      easyClose = FALSE,
      footer = NULL
    )
  )


  # observer de seccion -----------------------------------------------------
  observe({

    cli::cli_inform("observe `nav` {input$nav}")

    sidebar_toggle(
      id = "mainsidebar",
      open = input$nav != "Metodología"
    )

    # si cambia de tipo de resultado restea filtros
    if(input$nav != "Metodología") {
      updateTextInput(session, "buscar", value = "")
      shinyWidgets::updatePickerInput(session, "orden", selected = "Alfabéticamente")
      updateSliderInput(session, "habitantes", value = c(100, 700000))
      updateSliderInput(session, "indice_desarrollo", value = c(0, 1))
      updateSelectizeInput(session, "region", selected = "Metropolitana")
      shinyWidgets::updateCheckboxGroupButtons(session, "segmento", selected = character(0))
    }

  }) |>
    bindEvent(input$nav)

  # comuna ------------------------------------------------------------------
  data_filtrada <- reactive({

    cli::cli_inform("reactive `data_filtrada`")

    str(reactiveValuesToList(input)) |> print()
    data_filtrada <- data

    if(input$buscar != ""){
      data_filtrada <- data_filtrada |>
        filter(str_detect(str_clean(comuna), str_clean(input$buscar)))
    }

    if(!is.null(input$region)){
      data_filtrada <- data_filtrada |>
        filter(region %in% input$region)
    }

    # checkboxgroup
    inds <- c()
    if(input$segmento1) inds <- c(inds, "ALTO")
    if(input$segmento2) inds <- c(inds, "MEDIO ALTO")
    if(input$segmento3) inds <- c(inds, "MEDIO BAJO")
    if(input$segmento4) inds <- c(inds, "BAJO")

    if(!is.null(inds)){
      data_filtrada <- data_filtrada |>
        filter(v_cat %in% inds)
    }

    data_filtrada <- data_filtrada |>
      filter(input$habitantes[1]        <= habitantes                 , habitantes                  <= input$habitantes[2]       ) |>
      filter(input$indice_desarrollo[1] <= indice_de_desarrollo_humano, indice_de_desarrollo_humano <= input$indice_desarrollo[2]) |>
      filter(TRUE)

    # orden
    if(input$orden == "Alfabéticamente")                   data_filtrada <- data_filtrada |> arrange(comuna)
    if(input$orden == "Alfabéticamente descendente")       data_filtrada <- data_filtrada |> arrange(desc(comuna))
    if(input$orden == "Índice digitalización ascendente")  data_filtrada <- data_filtrada |> arrange(v)
    if(input$orden == "Índice digitalización descendente") data_filtrada <- data_filtrada |> arrange(desc(v))


    cli::cli_inform("reactive `data_filtrada` {nrow(data_filtrada)} comunas")

    data_filtrada

  }) |>
    bindEvent(input$go, input$orden, ignoreNULL = FALSE)

  output$comuna_resultados <- renderUI({
    cli::cli_inform("output `comuna_resultados`")
    # data_filtrada <- data
    data_filtrada <- data_filtrada()
    str_glue("{nrow(data_filtrada)} comunas")

  })

  output$comuna_boxes <- renderUI({

    cli::cli_inform("output `comuna_boxes`")

    # data_filtrada <- data
    data_filtrada <- data_filtrada()

    layout_columns(
      col_widths = 6,
      fill = FALSE, fillable = FALSE,
      !!!data_filtrada$value_box
      )

  })

  # region ------------------------------------------------------------------
  data_filtrada_regiones <- reactive({

    cli::cli_inform("reactive `data_filtrada_regiones`")

    str(reactiveValuesToList(input)) |> print()
    data_filtrada_regiones <- data_regiones

    if(input$buscar != ""){
      data_filtrada_regiones <- data_filtrada_regiones |>
        filter(str_detect(str_clean(region), str_clean(input$buscar)))
    }

    # if(!is.null(input$region)){
    #   data_filtrada <- data_filtrada |>
    #     filter(region %in% input$region)
    # }

    # checkboxgroup
    inds <- c()
    if(input$segmento1) inds <- c(inds, "ALTO")
    if(input$segmento2) inds <- c(inds, "MEDIO ALTO")
    if(input$segmento3) inds <- c(inds, "MEDIO BAJO")
    if(input$segmento4) inds <- c(inds, "BAJO")

    if(!is.null(inds)){
      data_filtrada_regiones <- data_filtrada_regiones |>
        filter(v_cat %in% inds)
    }

    # data_filtrada_regiones <- data_filtrada_regiones |>
    #   filter(input$habitantes[1]        <= habitantes                 , habitantes                  <= input$habitantes[2]       ) |>
    #   filter(input$indice_desarrollo[1] <= indice_de_desarrollo_humano, indice_de_desarrollo_humano <= input$indice_desarrollo[2]) |>
    #   filter(TRUE)

    # orden
    if(input$orden == "Alfabéticamente")                   data_filtrada_regiones <- data_filtrada_regiones |> arrange(region)
    if(input$orden == "Alfabéticamente descendente")       data_filtrada_regiones <- data_filtrada_regiones |> arrange(desc(region))
    if(input$orden == "Índice digitalización ascendente")  data_filtrada_regiones <- data_filtrada_regiones |> arrange(v)
    if(input$orden == "Índice digitalización descendente") data_filtrada_regiones <- data_filtrada_regiones |> arrange(desc(v))


    cli::cli_inform("reactive `data_filtrada_regiones` {nrow(data_filtrada_regiones)} regiones")

    data_filtrada_regiones

  }) |>
    bindEvent(input$go, input$orden, ignoreNULL = FALSE)

  output$region_resultados <- renderUI({
    cli::cli_inform("output `region_resultados`")
    # data_filtrada <- data
    data_filtrada_regiones <- data_filtrada_regiones()
    str_glue("{nrow(data_filtrada_regiones)} regiones")

  })

  output$region_boxes <- renderUI({

    cli::cli_inform("output `region_boxes`")

    # data_filtrada_regiones <- data_regiones
    data_filtrada_regiones <- data_filtrada_regiones()

    layout_columns(
      col_widths = 6,
      fill = FALSE, fillable = FALSE,
      !!!data_filtrada_regiones$value_box
    )

  })



  # keep alive --------------------------------------------------------------
  keep_alive <- shiny::reactiveTimer(
    intervalMs = 10000,
    session = shiny::getDefaultReactiveDomain()
    )
  shiny::observe({keep_alive()})

}
