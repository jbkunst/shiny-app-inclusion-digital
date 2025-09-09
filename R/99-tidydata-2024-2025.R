library(tidyverse)

data <- read_tsv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSLW2h1GXVL3yOvi2O09x3_lAQ_qkADJvBeXPoTk12upa0ERMaNZzUusK2_Gsvpeg/pub?output=tsv")

glimpse(data)

# hacemos calzar los nombres utilizados anteriormente
data <- data |>
  rename(
    codigo_comuna = id_comuna,
    habitantes = poblacion_censo_2024,
    indice_de_desarrollo_humano = idh_2023,
    indice_pobreza_multidimensional  = pobreza_2022
    )

data <- data |>
  mutate(across(c(indice_de_desarrollo_humano, indice_pobreza_multidimensional), .fns = function(x){round(x, 3)}))

glimpse(data)

# hacemos subcojuntos para cada año, formato tidy
d24 <- data |>
  select(codigo_comuna:indice_pobreza_multidimensional, ends_with("_2024")) |>
  mutate(anio = 2024, .before = 1) |>
  rename_all(str_remove, "_2024")

d25 <- data |>
  select(codigo_comuna:indice_pobreza_multidimensional, ends_with("_2025")) |>
  mutate(anio = 2025, .before = 1) |>
  rename_all(str_remove, "_2025")

data <- bind_rows(d24, d25)
glimpse(data)

data <- data |>
  rename(
    v         = idc,
    v_ranking = idc_ranking,
    v_cat     = idc_tramo,

    # concetividad hogar
    v1         = a_indice,
    v1_ranking = a_ranking,
    v1_cat     = a_tramo,

    # educación digital
    v3         = c_indice,
    v3_ranking = c_ranking,
    v3_cat     = c_tramo,

    # Municipio Digital
    v2         = b_indice,
    v2_ranking = b_ranking,
    v2_cat     = b_tramo,
  )

data <- data |>
  mutate(across(ends_with("_cat"), .fns = str_to_upper))

glimpse(data)

# lo siguiente es para distribuir uniformemente cada valor del indice entre la region que le corresponde
# esto es para el gauge
offsets <- c("BAJO" = 0, "MEDIO BAJO" = 0.25, "MEDIO ALTO" = 0.50, "ALTO" = 0.75)

data <- data |>
  # v
  mutate(v_gauge  = cume_dist(v)/4,  .by = c(v_cat, anio)) |>
  mutate(v_gauge  = v_gauge  + dplyr::recode(v_cat,  !!!offsets, .default = NA_real_)) |>
  # v1
  mutate(v1_gauge = cume_dist(v1)/4, .by = c(v1_cat, anio)) |>
  mutate(v1_gauge = v1_gauge + dplyr::recode(v1_cat, !!!offsets, .default = NA_real_)) |>
  # v2
  mutate(v2_gauge = cume_dist(v2)/4, .by = c(v2_cat, anio)) |>
  mutate(v2_gauge = v2_gauge + dplyr::recode(v2_cat, !!!offsets, .default = NA_real_)) |>
  # v3
  mutate(v3_gauge = cume_dist(v3)/4, .by = c(v3_cat, anio)) |>
  mutate(v3_gauge = v3_gauge + dplyr::recode(v3_cat, !!!offsets, .default = NA_real_))


data <- data |>
  mutate(across(ends_with("_gauge"), .fns = function(x){ round(x, 3)}))

glimpse(data)



# fix codigo comunca para mapa
data <- data |>
  mutate(
    codigo_comuna = as.character(codigo_comuna),
    codigo_comuna = str_pad(codigo_comuna, width = 5, pad = "0")
         )

data |> filter(codigo_comuna == "09118")

write_tsv(data, "data/data_datos_indice_de_inclusion_digital_indice_de_inclusion_digital_2024_2025.tsv")

# Version anterior para mantener ejemplo
#
# v1 Conectividad Hogar
# v3 Educación Digita
# v2 Municipio Digital
#
# Columns: 18
# $ comuna                          <chr> "Tirúa", "Villa Alegre", "Treguaco", "Queilén", "San Pablo", "Alto Biobío", "Quemchi", "Marchihue", "Corral", "Chaitén", "Freirina", "Puerto Octay", "Mar…
# $ codigo_comuna                   <chr> "08207", "07407", "16207", "10207", "10307", "08314", "10209", "06204", "14102", "10401", "03303", "10302", "02302", "07104", "13303", "03302", "09118", …
# $ region                          <chr> "Biobío", "Maule", "Ñuble", "Los Lagos", "Los Lagos", "Biobío", "Los Lagos", "O'Higgins", "Los Ríos", "Los Lagos", "Atacama", "Los Lagos", "Antofagasta",…
# $ habitantes                      <dbl> 11088, 17913, 5752, 5543, 10543, 6803, 8760, 7699, 5432, 5061, 7810, 9089, 6592, 4185, 22262, 5775, 9966, 8743, 4120, 9947, 9524, 15503, 19330, 11541, 47…
# $ indice_de_desarrollo_humano     <dbl> 0.356, 0.574, 0.487, 0.524, 0.568, 0.298, 0.480, 0.558, 0.507, 0.602, 0.604, 0.512, 0.702, 0.453, 0.664, 0.566, 0.444, 0.674, 0.605, 0.441, 0.516, 0.607,…
# $ indice_pobreza_multidimensional <dbl> 0.341, 0.133, 0.188, 0.293, 0.255, 0.389, 0.234, 0.167, 0.232, 0.247, 0.286, 0.307, 0.152, 0.181, 0.289, 0.279, 0.267, 0.125, 0.263, 0.341, 0.239, 0.272,…
# $ v                               <dbl> 0.346, 0.348, 0.350, 0.356, 0.360, 0.367, 0.369, 0.374, 0.375, 0.383, 0.390, 0.391, 0.394, 0.394, 0.394, 0.396, 0.397, 0.398, 0.398, 0.400, 0.406, 0.407,…
# $ v_cat                           <chr> "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "…
# $ v_gauge                         <dbl> 0.003289474, 0.006578947, 0.009868421, 0.013157895, 0.016447368, 0.019736842, 0.023026316, 0.026315789, 0.029605263, 0.032894737, 0.036184211, 0.03947368…
# $ v1                              <dbl> 0.356, 0.415, 0.414, 0.431, 0.463, 0.365, 0.399, 0.391, 0.412, 0.460, 0.361, 0.409, 0.376, 0.341, 0.488, 0.354, 0.396, 0.462, 0.382, 0.360, 0.418, 0.357,…
# $ v1_cat                          <chr> "BAJO", "MEDIO BAJO", "BAJO", "MEDIO BAJO", "MEDIO BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "MEDIO BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "MEDIO BAJO", "BA…
# $ v1_gauge                        <dbl> 0.029411765, 0.261235955, 0.250000000, 0.311797753, 0.421348315, 0.064705882, 0.194117647, 0.155882353, 0.244117647, 0.410112360, 0.050000000, 0.23235294…
# $ v2                              <dbl> 0.409, 0.538, 0.348, 0.191, 0.374, 0.448, 0.422, 0.382, 0.464, 0.366, 0.385, 0.366, 0.366, 0.385, 0.493, 0.366, 0.255, 0.411, 0.438, 0.403, 0.348, 0.517,…
# $ v2_cat                          <chr> "BAJO", "MEDIO ALTO", "BAJO", "BAJO", "BAJO", "MEDIO BAJO", "BAJO", "BAJO", "MEDIO BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "MEDIO ALTO", "BAJO", "…
# $ v2_gauge                        <dbl> 0.178160920, 0.676470588, 0.037356322, 0.002873563, 0.091954023, 0.369186047, 0.244252874, 0.097701149, 0.406976744, 0.080459770, 0.120689655, 0.08045977…
# $ v3                              <dbl> 0.272, 0.090, 0.288, 0.446, 0.242, 0.287, 0.286, 0.349, 0.249, 0.322, 0.423, 0.398, 0.441, 0.456, 0.200, 0.469, 0.541, 0.321, 0.375, 0.438, 0.453, 0.346,…
# $ v3_cat                          <chr> "BAJO", "BAJO", "BAJO", "MEDIO BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "BAJO", "MEDIO BAJO", "BAJO", "MEDIO BAJO", "MEDIO …
# $ v3_gauge                        <dbl> 0.016447368, 0.003289474, 0.029605263, 0.256666667, 0.009868421, 0.026315789, 0.023026316, 0.082236842, 0.013157895, 0.049342105, 0.187500000, 0.12828947…
