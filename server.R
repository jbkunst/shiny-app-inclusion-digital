# input <- list(habitantes = c(0, 1e10), indice_desarrollo = c(0, 1), buscar = "maipo")
function(input, output, session) {

  # modal bienvenida --------------------------------------------------------
  # showModal(
  #   modalDialog(
  #     fluidRow(
  #       tags$div(
  #         class = "col-sm-10 offset-sm-1 col-md-8 offset-md-2",
  #         # width = 8, offset = 2, styles = "max-width: 200px;",
  #         tags$img(src = "2305_LogoNudos_Pos.png", width="320px", class="rounded mx-auto d-block"),
  #         tags$div(
  #           class="text-center",
  #           tags$p("Bienvenido al"),
  #           tags$h2("Índice de Digitalización Comunal")
  #           ),
  #         tags$br(),
  #         includeMarkdown("data/bienvenida.md"),
  #
  #         tags$div(
  #           class="text-center",
  #           tags$button(
  #             type = "button", class = "btn btn-danger",
  #             style = "width: 300px",
  #             `data-dismiss` = "modal", `data-bs-dismiss` = "modal",
  #             "Ver resultados"
  #           )
  #         )
  #       )
  #     ),
  #     easyClose = FALSE,
  #     footer = NULL
  #   )
  # )

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
      # updateSelectizeInput(session, "region", selected = "Metropolitana")
      shinyWidgets::updateCheckboxGroupButtons(session, "segmento", selected = character(0))
    }

  }) |>
    bindEvent(input$nav)


  # dashboard ---------------------------------------------------------------
  output$dash_map <- renderLeaflet({
    # data_filtrada <- data
    d <- data_filtrada()
    d <- d |>
      select(codigo_comuna, v, v_cat) |>
      mutate(v_cat = str_to_title(v_cat))

    data_geo <- sf::read_sf("data/comunas_sim.gpkg")
    data_geo <- inner_join(data_geo, d, by = join_by(cut_com ==  codigo_comuna))
    # data_geo <- data_geo |>
      # filter(!is.na(v))
    data_geo <- data_geo |>
      filter(comuna != "Juan Fernández") |>
      filter(comuna != "Isla de Pascua")

    colorData <- factor(
      data_geo$v_cat,
      levels = c("Bajo", "Medio Bajo", "Medio Alto", "Alto")
    )

    pal <- colorFactor(
      palette = as.character(colores[c('rojo', 'naranjo', 'verde', 'azul')]),
      domain = colorData,
      na.color = "#ccc")

    lb <- data_geo |>
      str_glue_data("{comuna}: {v} ({v_cat})") |>
      map(htmltools::HTML)

    # popp <- ~paste0(
    #   comuna, " ",  round(v, 3), " (", v_cat, ")",
    #   tags$br(),
    #   actionButton(
    #     "reporte", "Reporte Sequía", class = "btn-primary btn-sm", size = 'xs',
    #     icon = icon('line-chart'),
    #     onclick = "Shiny.onInputChange('reporte', Math.random())"
    #   )
    # )


    leaflet(
      options = leafletOptions(
        attributionControl = FALSE,
        zoomControl = FALSE
      )
    ) |>
      addProviderTiles(providers$CartoDB.Positron,  group = "Administrativo") |>
      # addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") |>
      # addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") |>
      leaflet::addPolygons(
        data = data_geo,
        fillColor        = ~ pal(colorData),
        weight           = .5,
        dashArray        = "3",
        stroke           = NULL,
        fillOpacity      = 0.9,
        layerId          = ~ cut_com,
        # popup            = popp,
        label            = lb,
        highlightOptions = highlightOptions(
          color        = "white",
          weight       = 2,
          # fillColor    = parametros$color,
          bringToFront = TRUE
        ),
        labelOptions = labelOptions(
          style = list(
            "font-family"  = "Inria Sans",
            "box-shadow"   = "2px 2px rgba(0,0,0,0.15)",
            "font-size"    = "15px",
            "padding"      = "15px",
            "border-color" = "rgba(0,0,0,0.15)"
          )
        )
      )

  })

  output$dash_scatter <- renderHighchart({

    data <- data_filtrada()

    d <- data |>
      select(comuna, indice_de_desarrollo_humano, indice_de_inclusion_digital = v, v_cat) |>
      mutate(
        v_cat = str_to_title(v_cat),
        v_cat = factor(v_cat, levels = c("Bajo", "Medio Bajo", "Medio Alto", "Alto"))
      )
    d <- filter(d, complete.cases(d))

    cols <- colores[c('rojo', 'naranjo', 'verde', 'azul')]
    cols <- cols[which(levels(d$v_cat)  %in% d$v_cat)]
    cols <- as.character(cols)

    hchart(d, "scatter", hcaes(indice_de_inclusion_digital, indice_de_desarrollo_humano, name = comuna, group = v_cat)) |>
      hc_xAxis(min = .3, max = .7, title = list(text = "Índice de inclusion digital")) |>
      hc_yAxis(min = .2, max = 1., title = list(text = "Índice de desarrollo humano")) |>
      hc_colors(cols) |>
      hc_tooltip(
        pointFormat =  "<b>{point.comuna}</b><br/>Índice de inclusion digital: <b>{point.v_cat} {point.x}</b><br/>Índice de desarrollo humano: <b>{point.y}</b>"
      )

  })

  output$dash_dist <- renderHighchart({

    d <- data_filtrada()
    d <- d

    d |>
      count(vc = v2_cat) |>
      mutate(vc = str_to_title(vc)) |>
      hchart("column", hcaes(vc, n), colorByPoint = TRUE, name = input$select_var_dist) |>
      hc_colors(as.character(colores[c('rojo', 'naranjo', 'verde', 'azul')]))

  })

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
