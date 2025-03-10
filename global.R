# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(shinyWidgets)
library(rmarkdown)
library(markdown)
library(highcharter)
library(leaflet)

source("R/helpers.R")

# variables ---------------------------------------------------------------
colores <- list(
  gris = "#5F5758",
  gris2 = "#AEAEB2",
  rojo = "#FF3057",
  ahuesado = "#F6F3DF",
  blanco = "#FFFFFF",

  naranjo = "#FFB030",
  verde = "#0CB639",
  azul = "#4B2597"
)

app_theme <-  bs_theme(
  base_font = font_google("Inria Sans", wght = c(300, 400)),
  primary = colores$gris,
  danger  = colores$rojo,
  info = colores$gris2,
  # "navbar-light-bg" = colores$gris,
  # "navbar-dark-bg" = colores$gris,
  # "navbar-light-color" = colores$ahuesado,
  # "navbar-dark-color" = colores$ahuesado,
  bg = colores$blanco,
  fg = colores$gris,
  "navbar-dark-active-color" = colores$ahuesado,
  "modal-md" = "100%"
) |>
  bs_add_rules(
    # list(
      sass::sass_file("www/custom.css")
      # sass::sass_file("custom.scss"),
      # "body { background-color: $body-bg; }"
    # )
  )

# bslib::bs_theme_preview(app_theme)

options(
  # highcharter.lang = newlang_opts,
  highcharter.theme = hc_theme_smpl(
    # color = parametros$color,
    chart = list(style = list(fontFamily = "Inria Sans")),
    legend = list(
      layout = "horizontal",
      align = "left",
      verticalAlign = "top"
    ),
    plotOptions = list(
      series = list(marker = list(symbol = "circle")),
      line = list(marker = list(symbol = "circle")),
      area = list(marker = list(symbol = "circle"))
    )
  )
)


# partials ----------------------------------------------------------------
col <- partial(shiny::column, width = 12)

card <- purrr::partial(bslib::card, full_screen = TRUE)

# data comunas ------------------------------------------------------------
# data <- read_tsv(
#   "https://docs.google.com/spreadsheets/d/1j-lVC0T7NxXqg66HxFO14KbRB-9Kzz9XTEmQz84Wl3Y/pub?gid=0&single=true&output=tsv",
#   locale = locale(decimal_mark = ",")
#   )
# write_tsv(data, "data/Datos índice de inclusión digital - Índice de inclusión digital.tsv")

data <- read_tsv(
  "data/Datos índice de inclusión digital - Índice de inclusión digital.tsv",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
)

# glimpse(data)

data <- janitor::clean_names(data)

data <- data |>
  rename(
    v = valor_indice_de_inclusion_digital_7,
    v_cat = categoria_indice_de_inclusion_digital,
    v1 = valor_sub_indicador_1_acceso_9,
    v1_cat = categoria_sub_indicador_1_acceso,
    v2 = valor_sub_indicador_2_politico_11,
    v2_cat = categoria_sub_indicador_2_politico,
    v3 = valor_sub_indicador_3_educativo_13,
    v3_cat = categoria_sub_indicador_3_educativo
  )

data <- data |>
  mutate(across(c(v_cat, v1_cat, v2_cat, v3_cat), str_to_upper)) |>
  mutate(across(c(v,v1, v2, v3), as.numeric)) |>
  mutate(
    indice_de_desarrollo_humano = as.numeric(indice_de_desarrollo_humano)
  )

data <- data |>
  select(1:14) |>
  filter(TRUE)

# glimpse(data)



# fix para obtener valores en regiones semaforo ---------------------------
data |>
  group_by(v_cat) |>
  summarise(
    n(), min(v), max(v)
  )


d1 <- data |>
  filter(is.na(v_cat))

d2 <- data |>
  filter(!is.na(v_cat)) |>
  arrange(v) |>
  mutate(v_gauge  = ecdf(v)(v)/4, .by = v_cat, .after = v_cat) |>
  mutate(
    v_gauge = v_gauge +
      case_when(
        v_cat == "BAJO"       ~ 0,
        v_cat == "MEDIO BAJO" ~ .25,
        v_cat == "MEDIO ALTO" ~ .50,
        v_cat == "ALTO"       ~ .75
      ),
  ) |>
  mutate(v3_gauge  = ecdf(v3)(v3)/4, .by = v3_cat, .after = v3_cat) |>
  mutate(
    v3_gauge = v3_gauge +
      case_when(
        v3_cat == "BAJO"       ~ 0,
        v3_cat == "MEDIO BAJO" ~ .25,
        v3_cat == "MEDIO ALTO" ~ .50,
        v3_cat == "ALTO"       ~ .75
      ),
  )


data <- bind_rows(d2, d1)

data <- data |>
  mutate(v1_gauge  = ecdf(v1)(v1)/4, .by = v1_cat, .after = v1_cat) |>
  mutate(
    v1_gauge = v1_gauge +
      case_when(
        v1_cat == "BAJO"       ~ 0,
        v1_cat == "MEDIO BAJO" ~ .25,
        v1_cat == "MEDIO ALTO" ~ .50,
        v1_cat == "ALTO"       ~ .75
      ),
  ) |>
  mutate(v2_gauge  = ecdf(v2)(v2)/4, .by = v2_cat, .after = v2_cat) |>
  mutate(
    v2_gauge = v2_gauge +
      case_when(
        v2_cat == "BAJO"       ~ 0,
        v2_cat == "MEDIO BAJO" ~ .25,
        v2_cat == "MEDIO ALTO" ~ .50,
        v2_cat == "ALTO"       ~ .75
      ),
  )



# generando valueboxes ----------------------------------------------------
cli::cli_inform("Partiendo value_boxes")

if(file.exists("data/data.rds")){
  data <- read_rds("data/data.rds")
} else {

  value_boxes <- data |>
    select(comuna, region, codigo_comuna, v, v_cat, v_gauge, v1, v1_cat, v1_gauge, v2, v2_cat, v2_gauge, v3, v3_cat, v3_gauge) |>
    purrr::pmap(function(comuna, region, codigo_comuna, v, v_cat, v_gauge, v1, v1_cat, v1_gauge, v2, v2_cat, v2_gauge, v3, v3_cat, v3_gauge){

      cli::cli_inform(comuna)

      # comuna <- "Copiapó"
      # region <- "Atacama"
      # codigo_comuna <- "03101"
      # v <- v1 <- v2 <- v3 <- v_gauge <- v1_gauge <- v2_gauge <- v3_gauge <- 0.432
      # v_cat <- v1_cat <- v2_cat <- v3_cat <- "Alto"

      lc1 <- layout_columns(
        col_widths = c(6, 6, 12),
        # fill = FALSE, fillable = FALSE,
        col(
          style="height: 100%;position: relative",
          tags$h5(style = "position: absolute;bottom: 0", "Índice Digitalización")
        ),
        col(
          style = "text-align: right",
          tags$small(coalesce(v_cat, "-")),
          tags$h1(formatear_numero(v))
        ),
        col(horizontal_gauge_html(percent = v_gauge, height = 10), tags$br()),
      )

      lc2 <- layout_columns(
        col_widths = c(6, 6, 12),
        # fill = FALSE, fillable = FALSE,
        col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Conectividad Hogar")),
        col(style = "text-align: right",tags$small(coalesce(v1_cat, "-")), tags$h1(formatear_numero(v1))),
        col(horizontal_gauge_html(percent = v1_gauge, height = 10), tags$br()),
        col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Educación Digital")),
        col(style = "text-align: right",tags$small(coalesce(v3_cat, "-")), tags$h1(formatear_numero(v3))),
        horizontal_gauge_html(percent = v3_gauge, height = 10),
        col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Municipio Digital")),
        col(style = "text-align: right",tags$small(coalesce(v2_cat, "-")), tags$h1(formatear_numero(v2))),
        col(horizontal_gauge_html(percent = v2_gauge, height = 10), tags$br())
      )

      c <- card(
        style = str_glue("background-color:{colores$ahuesado}; color: {colores$gris}"),
        # tags$small(region),
        tags$h2(tags$strong(str_to_upper(comuna))),
        tags$h5(region),
        lc1,
        tags$div(id = str_glue("comuna{codigo_comuna}"), class = "collapse", lc2),
        tags$button(
          "Ver subindicadores",
          onclick = str_glue("$('#comuna{codigo_comuna}').collapse('toggle'); this.textContent = this.textContent === 'Ver subindicadores' ? 'Cerrar detalle' : 'Ver subindicadores';"),
          class = "btn btn-primary btn-md",
          style = "max-width:200px"
        ),
      )

      # lc2 |> as.character() |> cat()
      # c |> as.character() |> cat()
      # htmltools::tagQuery(c)$find(".card-body")$removeClass("bslib-gap-spacing")$allTags()

      c

  })

  data <- data |>
    mutate(value_box = value_boxes)

  rm(value_boxes)

  saveRDS(data, "data/data.rds")

}

cli::cli_inform("Terminando value_boxes")

# data <- data |>
#   sample_n(50)


# data regiones -----------------------------------------------------------
data_regiones <- read_tsv(
  "data/Datos IDC regiones - Índice de inclusión digital.tsv",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
)

# glimpse(data_regiones)

data_regiones <- janitor::clean_names(data_regiones)

data_regiones <- data_regiones |>
  rename(
    v = idc,
    v_cat = tramo_idc
  )

data_regiones <- data_regiones |>
  mutate(v_cat = str_to_upper(v_cat),
         v_cat = str_replace_all(v_cat, "-", " "))

data_regiones <- data_regiones |>
  mutate(v_gauge  = ecdf(v)(v)/4, .by = v_cat, .after = v_cat) |>
  mutate(
    v_gauge = v_gauge +
      case_when(
        v_cat == "BAJO"       ~ 0,
        v_cat == "MEDIO BAJO" ~ .25,
        v_cat == "MEDIO ALTO" ~ .50,
        v_cat == "ALTO"       ~ .75
      ),
  )

data_regiones <- data_regiones |>
  # select(1:14) |>
  filter(TRUE)

cli::cli_inform("Partiendo value_boxes regiones")


value_boxes <- data_regiones |>
  select(region, v, v_cat, v_gauge) |>
  purrr::pmap(function(region, v, v_cat, v_gauge){

    cli::cli_inform(region)

#     region <- "Atacama"
#     v <- v1 <- v2 <- v3 <- 0.432
#     v_cat <- v1_cat <- v2_cat <- v3_cat <- "Alto"

    lc1 <- layout_columns(
      col_widths = c(6, 6, 12),
      # fill = FALSE, fillable = FALSE,
      col(
        style="height: 100%;position: relative",
        tags$h5(style = "position: absolute;bottom: 0", "Índice Digitalización")
      ),
      col(
        style = "text-align: right",
        tags$small(coalesce(v_cat, "-")),
        tags$h1(formatear_numero(v))
      ),
      col(horizontal_gauge_html(percent = v_gauge, height = 10), tags$br()),
    )

    # lc2 <- layout_columns(
    #   col_widths = c(6, 6, 12),
    #   # fill = FALSE, fillable = FALSE,
    #   col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Conectividad Hogar")),
    #   col(style = "text-align: right",tags$small(coalesce(v1_cat, "-")), tags$h1(formatear_numero(v1))),
    #   col(horizontal_gauge_html(percent = v1, height = 10), tags$br()),
    #   col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Municipio Digital")),
    #   col(style = "text-align: right",tags$small(coalesce(v2_cat, "-")), tags$h1(formatear_numero(v2))),
    #   col(horizontal_gauge_html(percent = v2, height = 10), tags$br()),
    #   col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Educación Digital")),
    #   col(style = "text-align: right",tags$small(coalesce(v3_cat, "-")), tags$h1(formatear_numero(v3))),
    #   horizontal_gauge_html(percent = v3, height = 10)
    # )

    c <- card(
      style = str_glue("background-color:{colores$ahuesado}; color: {colores$gris}"),
      # tags$small(region),
      tags$h2(tags$strong(str_to_upper(region))),
      # tags$h5(region),
      lc1,
      # tags$div(id = str_glue("comuna{codigo_comuna}"), class = "collapse", lc2),
      # tags$button(
      #   "Ver subindicadores",
      #   onclick = str_glue("$('#comuna{codigo_comuna}').collapse('toggle'); this.textContent = this.textContent === 'Ver subindicadores' ? 'Cerrar detalle' : 'Ver subindicadores';"),
      #   class = "btn btn-primary btn-md",
      #   style = "max-width:200px"
      # ),
    )

    # lc2 |> as.character() |> cat()
    # c |> as.character() |> cat()
    # htmltools::tagQuery(c)$find(".card-body")$removeClass("bslib-gap-spacing")$allTags()

    c

  })

data_regiones <- data_regiones |>
  mutate(value_box = value_boxes)

rm(value_boxes)

cli::cli_inform("Terminando value_boxes regiones")


# sidebar -----------------------------------------------------------------
opts_region <- data |>
  distinct(region) |>
  pull()

sidebar_app <- sidebar(
  id = "mainsidebar",
  textInput(inputId = "buscar", label = "Buscar"),
  shinyWidgets::pickerInput(
    inputId = "orden",
    label = "Ordenar según",
    choices = c(
      "Alfabéticamente",
      "Alfabéticamente descendente",
      "Índice digitalización ascendente",
      "Índice digitalización descendente"
      ),
    choicesOpt = list(
      icon = c(
        "glyphicon glyphicon-sort-by-alphabet",
        "glyphicon glyphicon-sort-by-alphabet-alt",
        "glyphicon glyphicon-sort-by-order",
        "glyphicon glyphicon-sort-by-order-alt"
        )
      ),
    options = list(`icon-base` = "")
  ),

  conditionalPanel(
    "input.nav !== 'Resultados por región'",
    sliderInput(
      "habitantes",
      label = "Habitantes",
      # min = min(data$habitantes),
      # max = max(data$habitantes),
      # value =  c(min(data$habitantes), max(data$habitantes))
      min = 100,
      max = 700000,
      value = c(100, 700000)
    ),
    sliderInput(
      "indice_desarrollo",
      label = "Índice desarrollo humano",
      # min = min(data$indice_de_desarrollo_humano, na.rm = TRUE),
      # max = max(data$indice_de_desarrollo_humano, na.rm = TRUE),
      # value =  c(min(data$indice_de_desarrollo_humano, na.rm = TRUE), max(data$indice_de_desarrollo_humano, na.rm = TRUE))
      min = 0,
      max = 1,
      value = c(0, 1)
    ),
    selectizeInput(
      "region",
      "Región",
      choices = opts_region,
      # selected = "Metropolitana",
      selected = NULL,
      multiple = TRUE,
      options = list(placeholder = "Todas las regiones")
      )
  ),
  checkboxGroupButtons(
    inputId = "segmento",
    label = "Segmento índice de inclusión digital",
    size = "sm",
    individual = TRUE,
    justified = FALSE,
    # status = "primary",
    choices = c("Alto", "Medio Alto", "Medio Bajo", "Bajo"),
    # checkIcon = list(
    #   yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
    #   no = tags$i(class = "fa fa-square-o", style = "color: steelblue")
    #   )
    ),
  shiny::actionButton("go", "Aplicar filtros", class = "btn btn-danger")
)

