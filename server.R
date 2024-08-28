function(input, output, session) {

  data_filtrada <- reactive({
    data
  })

  output$boxes <- renderUI({
    # data_filtrada <- data
    data_filtrada <- data_filtrada()

    data_filtrada |>
      select(comuna = Comuna, region = Región, valor = `Valor Índice de inclusión digital`)

    # value_boxes <- pmap(dunimag, function(eje_programa_de_gobierno, tipologia_dentro_del_eje, unidad, magnitud, n){

      # eje_programa_de_gobierno <- "Santiago sustentable"
      # tipologia_dentro_del_eje <- "Construcción, recuperación o mejoramiento de plazas"
      # unidad   <- "m2"
      # magnitud <- "289.145"
      # n        <- 120

    #   value_box(
    #     tags$span(
    #       tags$h6(tags$strong(eje_programa_de_gobierno)),
    #       tags$h5(tipologia_dentro_del_eje),
    #       tags$h3(magnitud, " ", unidad)
    #     ),
    #     value = NULL,
    #     theme = value_box_theme(bg = "white", fg = fndr_pars$fg)
    #   )
    #
    # })

    layout_column_wrap(
      width = 1/4,
      fillable = TRUE,
      fill = TRUE,
      # !!!value_boxes
      # card(
      #   tags$h3("Isla de Maipo"),
      #   tags$h5("Región Metropolitana"),
      #   tags$h2("66", style = "float: right;"),
      #   tags$div(
      #     class = "gauge-container",
      #     tags$div(class = "gauge-segment segment-1"),
      #     tags$div(class = "gauge-segment segment-2"),
      #     tags$div(class = "gauge-segment segment-3"),
      #     tags$div(class = "gauge-segment segment-4"),
      #     tags$div(class = "gauge-indicator", style = "left: 66%;")
      #   )
      # ),
      card(
        tags$b("Isla de Maipo"),
        tags$small("Región Metropolitana"),
        tags$h2("66", class = "text-right"),
        tags$div(
          style = "width: 100%; height: 10px; display: flex; position: relative;",
          tags$div(style = " background-color: #FF0000; flex: 1; height: 100%"),
          tags$div(style = " background-color: #FFA500; flex: 1; height: 100%"),
          tags$div(style = " background-color: #FFFF00; flex: 1; height: 100%"),
          tags$div(style = " background-color: #008000; flex: 1; height: 100%"),
          tags$div(style = "left: 33%; position: absolute; top: 50%; transform: translate(-50%, -50%); width: 15px; height: 15px; background-color: #FFFFFF; border: 2px solid #000000; border-radius: 50%; box-shadow: 0 0 5px rgba(0, 0, 0, 0.5);")
        )
      )
    )

  })

}
