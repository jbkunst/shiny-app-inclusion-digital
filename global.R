# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(purrr)
library(stringr)

source("R/helpers.R")

# variables ---------------------------------------------------------------
colores <- list(
  gris = "#5F5758",
  gris2 = "#AEAEB2",
  rojo = "#FF3057",
  ahuesado = "#F6F3DF",
  blanco = "#FFFFFF"
)

app_theme <-  bs_theme(
  base_font = font_google("Inria Sans", wght = c(300, 400)),
  primary = colores$gris,
  # "navbar-light-bg" = colores$gris,
  # "navbar-dark-bg" = colores$gris,
  # "navbar-light-color" = colores$ahuesado,
  # "navbar-dark-color" = colores$ahuesado,
  bg = colores$blanco,
  fg = colores$gris,
  "navbar-dark-active-color" = colores$ahuesado,
)

# bslib::bs_theme_preview(app_theme)

# data --------------------------------------------------------------------
set.seed(123)
# data <- read_tsv(
#   "https://docs.google.com/spreadsheets/d/1j-lVC0T7NxXqg66HxFO14KbRB-9Kzz9XTEmQz84Wl3Y/pub?gid=0&single=true&output=tsv",
#   locale = locale(decimal_mark = ",")
#   )
# write_tsv(data, "data/Datos índice de inclusión digital - Índice de inclusión digital.tsv")

data <- read_tsv(
  "data/Datos índice de inclusión digital - Índice de inclusión digital.tsv",
  locale = locale(decimal_mark = ",")
)

glimpse(data)

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

# data <- data |>
#   sample_n(50)

glimpse(data)


# sidebar -----------------------------------------------------------------
opts_region <- data |>
  distinct(region) |>
  pull()



sidebar_app <- sidebar(
  id = "mainsidebar",
  textInput(inputId = "buscar", label = "Buscar"),
  shinyWidgets::pickerInput(
    inputId = "orden",
    label = "Ordenar",
    choices = c(
      "Alfabéticamente",
      "Alfabéticamente descendente"
      ),
    choicesOpt = list(
      icon = c(
        "glyphicon glyphicon-sort-by-alphabet",
        "glyphicon glyphicon-sort-by-alphabet-alt"
        )
      ),
    options = list(`icon-base` = "")
  ),
  sliderInput(
    "habitantes",
    label = "Habitantes",
    min = min(data$habitantes),
    max = max(data$habitantes),
    value =  c(min(data$habitantes), max(data$habitantes))
  ),
  # selectizeInput("region", label = "Región", choices = opts_region),
  sliderInput(
    "indice_desarrollo",
    label = "Índice desarrollo humano",
    min = min(data$indice_de_desarrollo_humano, na.rm = TRUE),
    max = max(data$indice_de_desarrollo_humano, na.rm = TRUE),
    value =  c(min(data$indice_de_desarrollo_humano, na.rm = TRUE), max(data$indice_de_desarrollo_humano, na.rm = TRUE))
  ),
  "Segmento índice de inclusión digital"
)

# partials ----------------------------------------------------------------
col <- partial(shiny::column, width = 12)
