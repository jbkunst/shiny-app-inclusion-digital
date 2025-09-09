library(tidyverse)

data <- readxl::read_xlsx("data/BBDD_IDC25_Visualizacion-v2.xlsx")

glimpse(data)

data <- janitor::clean_names(data)

glimpse(data)

# hacemos calzar los nombres utilizados anteriormente
data <- data |>
  rename(
    habitantes_2024 = habitantes_2023_proyeccion_censo_2017_idc_2024,
    habitantes_2025 = habitantes_2024_poblacion_censo_2024_idc_2025,
    categoria_subindice_educacion_2024 = categoria_subindice_educacion,
    categoria_subindice_conectividad_2024 = categoria_subindice_conectividad,
    valor_subindice_educacion_2024 = valor_subindice_educacion_digital_2024
  ) |>
  rename(
    indice_de_desarrollo_humano_2024 = indice_desarrollo_humano_2023_ambos_idc,
    indice_pobreza_multidimensional_2024 = indice_pobreza_multidimensional_2022_amnbos_idc
  ) |>
  # ambos
  mutate(
    indice_de_desarrollo_humano_2025 = indice_de_desarrollo_humano_2024,
    indice_pobreza_multidimensional_2025 = indice_pobreza_multidimensional_2024
    ) |>
  glimpse()

glimpse(data)

data <- data |>
  mutate(across(where(is.numeric), .fns = function(x){round(x, 3)}))

glimpse(data)

# hacemos subcojuntos para cada año, formato tidy
d24 <- data |>
  select(comuna:id_region, ends_with("_2024")) |>
  mutate(anio = 2024, .before = 1) |>
  rename_all(str_remove, "_2024")

d25 <- data |>
  select(comuna:id_region, ends_with("_2025")) |>
  mutate(anio = 2025, .before = 1) |>
  rename_all(str_remove, "_2025")

data <- bind_rows(d24, d25)

glimpse(data)

data |> select(contains("subindice_municipio"))

data <- data |>
  rename(
    v         = valor_idc,
    v_ranking = idc_ranking,
    v_cat     = categoria_idc,

    # concetividad hogar
    v1         = valor_subindice_conectividad_hogar,
    # v1_ranking = a_ranking,
    v1_cat     = categoria_subindice_conectividad,

    # educación digital
    v3         = valor_subindice_educacion,
    # v3_ranking = c_ranking,
    v3_cat     = categoria_subindice_educacion,

    # Municipio Digital
    v2         = valor_subindice_municipio,
    # v2_ranking = b_ranking,
    v2_cat     = categoria_subindice_municipio,
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
  mutate(across(ends_with("_gauge"), .fns = function(x){ round(x, 3) }))

glimpse(data)


# fix codigo comunca para mapa
data <- data |>
  mutate(
    codigo_comuna = as.character(codigo_comuna),
    codigo_comuna = str_pad(codigo_comuna, width = 5, pad = "0")
  )

data |> filter(codigo_comuna == "09118")

write_tsv(data, "data/data_datos_indice_de_inclusion_digital_indice_de_inclusion_digital_2024_2025_v2.tsv")
