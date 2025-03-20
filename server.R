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
      size = "xl",
      easyClose = FALSE,
      footer = NULL
    ) |>  tagAppendAttributes(class = "model-main")
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
      # updateSelectizeInput(session, "region", selected = "Metropolitana")
      shinyWidgets::updateCheckboxGroupButtons(session, "segmento", selected = character(0))
    }

  }) |>
    bindEvent(input$nav)

  # dashboard ---------------------------------------------------------------
  output$dash_vb_mean <- renderUI({
    data <- data_filtrada()
    v <- mean(data$v, na.rm = TRUE)
    value_box(
      title = "Media",
      value = formatear_numero(v),
      categorizar_indicador(v)
    )
  })

  output$dash_vb_median <- renderUI({
    data <- data_filtrada()
    v <- quantile(data$v, 0.5, na.rm = TRUE)
    value_box(title = "Mediana", value = formatear_numero(v), categorizar_indicador(v))
  })

  output$dash_vb_min <- renderUI({
    data <- data_filtrada()
    v <- min(data$v, na.rm = TRUE)
    value_box(title = "Mínimo", value = formatear_numero(v), categorizar_indicador(v))
  })

  output$dash_vb_max <- renderUI({
    data <- data_filtrada()
    v <- max(data$v, na.rm = TRUE)
    value_box(title = "Máximo", value = formatear_numero(v), categorizar_indicador(v))
  })

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
            "z-index" = 10000,
            "border-color" = "rgba(0,0,0,0.15)"
          )
        )
      )

  })

  output$dash_scatter <- renderHighchart({

    data <- data_filtrada()

    d <- data |>
      select(codigo_comuna, comuna, indice_de_desarrollo_humano, indice_de_inclusion_digital = v, v_cat) |>
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
      ) |> 
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('dash_scatter_point_click', {'cat': this.codigo_comuna, '.nonce': Math.random()}) }")))
          # point = list(events = list(click = JS("function(){ console.log(this.codigo_comuna) }")))
        )
      )
      

  })

  output$dash_dist <- renderHighchart({

    d <- data_filtrada()
    
    # input <- list(select_var_dist = "v1")

    d$c     <- d[[input$select_var_dist]]
    d$c_cat <- d[[str_c(input$select_var_dist, "_cat")]]

    d <- d |> 
      select(codigo_comuna, comuna, c, c_cat) |> 
      arrange(c) |> 
      filter(!is.na(c)) |> 
      mutate(x = row_number()) |> 
      mutate(
      c_cat = str_to_title(c_cat),
      c_cat = factor(c_cat, levels = c("Bajo", "Medio Bajo", "Medio Alto", "Alto"))
    )

    cols <- colores[c('rojo', 'naranjo', 'verde', 'azul')]
    cols <- cols[which(levels(d$c_cat)  %in% d$c_cat)]
    cols <- as.character(cols)
    
    hchart(
      d,
      type = "column",
      hcaes(x, c, group = c_cat, name = comuna)
    ) |>
      hc_colors(cols) |> 
      hc_xAxis(visible = FALSE) |> 
      hc_yAxis(title = list(text = "")) |> 
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(events = list(click = JS("function(){ Shiny.onInputChange('dash_scatter_point_click', {'cat': this.codigo_comuna, '.nonce': Math.random()}) }")))
          # point = list(events = list(click = JS("function(){ console.log(this.codigo_comuna) }")))
        )
      )      

  })

  comuna_reactive <- reactiveVal(NULL)

  observeEvent(input$dash_map_shape_click, {
    cli::cli_inform("dash_map_shape_click: {input$dash_map_shape_click$id}")
    comuna_reactive(input$dash_map_shape_click$id)
  })
  observeEvent(input$dash_scatter_point_click, {
    cli::cli_inform("dash_scatter_point_click: {input$dash_scatter_point_click$cat}")
    comuna_reactive(input$dash_scatter_point_click$cat)
  })
  # Modal
  observeEvent(comuna_reactive(), {

    com <- comuna_reactive()
    # com <- "09104"
    # vb <- data |> 
    #   filter(codigo_comuna == com) |> 
    #   pull(value_box)

    value_boxes <- data |>
      filter(codigo_comuna == com) |>
      select(comuna, region, codigo_comuna, v, v_cat, v_gauge, v1, v1_cat, v1_gauge, v2, v2_cat, v2_gauge, v3, v3_cat, v3_gauge) |>
      purrr::pmap(function(comuna, region, codigo_comuna, v, v_cat, v_gauge, v1, v1_cat, v1_gauge, v2, v2_cat, v2_gauge, v3, v3_cat, v3_gauge){
  
        cli::cli_inform(comuna)
  
        # comuna <- "Copiapó"
        # region <- "Atacama"
        # codigo_comuna <- "03101"
        # v <- v1 <- v2 <- v3 <- v_gauge <- v1_gauge <- v2_gauge <- v3_gauge <- 0.432
        # v_cat <- v1_cat <- v2_cat <- v3_cat <- "Alto"

        lc2 <- layout_columns(
          # col_widths = c(6, 6, 12, 12),
          col_widths = c(6, 6, 12),
          # fill = FALSE, fillable = FALSE,
          col(style="height: 100%;position: relative", tags$h6(style = "position: absolute;bottom: 0", "Índice Digitalización")),
          col(style = "text-align: right", tags$small(coalesce(v_cat, "-")), tags$h3(formatear_numero(v))),
          col(horizontal_gauge_html(percent = v_gauge, height = 5), tags$br()),
          # fill = FALSE, fillable = FALSE,
          col(style="height: 100%;position: relative", tags$h6(style = "position: absolute;bottom: 0", "Conectividad Hogar")),
          col(style = "text-align: right",tags$small(coalesce(v1_cat, "-")), tags$h3(formatear_numero(v1))),
          col(horizontal_gauge_html(percent = v1_gauge, height = 5)),
          # tags$br(),
          col(style="height: 100%;position: relative", tags$h6(style = "position: absolute;bottom: 0", "Educación Digital")),
          col(style = "text-align: right",tags$small(coalesce(v3_cat, "-")), tags$h3(formatear_numero(v3))),
          col(horizontal_gauge_html(percent = v3_gauge, height = 5)),
          # tags$br(),
          col(style="height: 100%;position: relative", tags$h6(style = "position: absolute;bottom: 0", "Municipio Digital")),
          col(style = "text-align: right",tags$small(coalesce(v2_cat, "-")), tags$h3(formatear_numero(v2))),
          col(horizontal_gauge_html(percent = v2_gauge, height = 5)),
          # tags$br()
        )
  
        c <- layout_columns(
          col_widths = 12,
          style = str_glue("background-color:{colores$ahuesado}; color: {colores$gris}; border: 0px"),
          tags$h3(tags$strong(str_to_upper(comuna)), tags$small(region)),
          lc2
        )
        
        c
  
    })

    showModal(modalDialog(value_boxes[[1]], size = "m", footer = NULL, easyClose = TRUE, fade = TRUE))

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
