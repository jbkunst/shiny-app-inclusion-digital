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
data <- read_tsv(
  "https://docs.google.com/spreadsheets/d/1j-lVC0T7NxXqg66HxFO14KbRB-9Kzz9XTEmQz84Wl3Y/pub?gid=0&single=true&output=tsv",
  locale = locale(decimal_mark = ",")
  )

data <- data |>
  mutate(`Valor Índice de inclusión digital` = coalesce(`Valor Índice de inclusión digital`, runif(nrow(data)))) |>
  dplyr::sample_n(50) |>
  arrange(`Código Comuna`)

data

# sidebar -----------------------------------------------------------------
opts_region <- data |>
  distinct(Región) |>
  pull()

sbr <- sidebar(
  selectizeInput("region", label = "Región", choices = opts_region)
)

