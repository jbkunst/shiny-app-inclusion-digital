# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(purrr)
library(stringr)

source("R/helpers.R")

# variables ---------------------------------------------------------------
app_theme <-  bs_theme(
  # bootswatch = "simplex"
)

# data --------------------------------------------------------------------
set.seed(123)
# data <- read_tsv(
#   "https://docs.google.com/spreadsheets/d/1j-lVC0T7NxXqg66HxFO14KbRB-9Kzz9XTEmQz84Wl3Y/pub?gid=0&single=true&output=tsv",
#   locale = locale(decimal_mark = ",")
#   )

data <- read_tsv(
  "data/Datos índice de inclusión digital - Índice de inclusión digital.tsv",
  locale = locale(decimal_mark = ",")
)

glimpse(data)

data <- data |>
  mutate(
    region = Región,
    comuna = Comuna,
    codigo_comuna = `Código Comuna`,
    habitantes = Habitantes,
    indice_de_desarrollo_humano = coalesce(`Índice de desarrollo humano`, runif(nrow(data))),
    indice_de_desarrollo_humano = round(indice_de_desarrollo_humano, 2),
    indice_inclusion_digital = coalesce(`Valor Índice de inclusión digital`, runif(nrow(data))),

    v1 = coalesce(`Valor sub indicador 1 estandarizado_Acceso`, runif(nrow(data))),
    v2 = coalesce(`Valor sub indicador 2 estandarizado`, runif(nrow(data))),
    v3 = coalesce(`Valor sub indicador 3 estandarizado`, runif(nrow(data))),

    v1_cat = categorizar_indicador(v1),
    v2_cat = categorizar_indicador(v2),
    v3_cat = categorizar_indicador(v3)

    ) |>
  # dplyr::sample_n(50) |>
  arrange(codigo_comuna)

glimpse(data)

# sidebar -----------------------------------------------------------------
opts_region <- data |>
  distinct(region) |>
  pull()

sidebar_app <- sidebar(
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
    min = min(data$indice_de_desarrollo_humano),
    max = max(data$indice_de_desarrollo_humano),
    value =  c(min(data$indice_de_desarrollo_humano), max(data$indice_de_desarrollo_humano))
    ),
)

