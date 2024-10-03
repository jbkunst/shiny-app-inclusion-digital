# input <- list(habitantes = c(0, 1e10), indice_desarrollo = c(0, 1), buscar = "maipo")
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
    if(input$orden == "Alfabéticamente")               data_filtrada <- data_filtrada |> arrange(comuna)
    if(input$orden == "Alfabéticamente descendente")   data_filtrada <- data_filtrada |> arrange(desc(comuna))
    if(input$orden == "Inclusión digital")             data_filtrada <- data_filtrada |> arrange(v)
    if(input$orden == "Inclusión digital descendente") data_filtrada <- data_filtrada |> arrange(desc(v))


    cli::cli_inform("reactive `data_filtrada` {nrow(data_filtrada)} comunas")

    data_filtrada

  }) |>
    bindEvent(input$go, ignoreNULL = FALSE)


  # texto comuna ------------------------------------------------------------
  output$comuna_resultados <- renderUI({
    cli::cli_inform("output `comuna_resultados`")
    # data_filtrada <- data
    data_filtrada <- data_filtrada()
    str_glue("{nrow(data_filtrada)} comunas")

  })


  # comuna ------------------------------------------------------------------
  output$comuna_boxes <- renderUI({

    cli::cli_inform("output `comuna_boxes`")

    # data_filtrada <- data
    data_filtrada <- data_filtrada()

    layout_columns(
      col_widths = 4,
      fill = FALSE, fillable = FALSE,
      !!!data_filtrada$value_box
      )

  })

}
