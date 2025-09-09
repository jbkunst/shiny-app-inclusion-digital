horizontal_gauge_html <- function(percent = runif(1), height = 10){

  hhalf <- round(height/2)

  cols <- c(
    lvl1 = "#ff2f57",
    lvl2 = "#ffb031",
    lvl3 = "#0eb638",
    lvl4 = "#4b2597"
    )

  p2 <- round(percent * 100, 0)

  # https://www.color-hex.com/color-palette/35021
  tags$div(
    style = stringr::str_glue("width: 100%; height: {height}px; display: flex; position: relative;"),
    tags$div(style = str_glue("background-color: {cols[[1]]}; flex: 1; height: 100%; border-top-left-radius: {hhalf}px; border-bottom-left-radius: {hhalf}px;")),
    tags$div(style = str_glue("background-color: {cols[[2]]}; flex: 1; height: 100%")),
    tags$div(style = str_glue("background-color: {cols[[3]]}; flex: 1; height: 100%")),
    tags$div(style = str_glue("background-color: {cols[[4]]}; flex: 1; height: 100%; border-top-right-radius: {hhalf}px; border-bottom-right-radius: {hhalf}px;")),
    tags$div(style = stringr::str_glue("left: {p2}%; position: absolute; top: 50%; transform: translate(-50%, -50%); width: 15px; height: 15px; background-color: #F4F4F4; border: 2px solid #5D5E60; border-radius: 50%; box-shadow: 0 0 5px rgba(0, 0, 0, 0.5);"))
  )

}

# card(horizontal_gauge_html())

categorizar_indicador <- function(valor) {
  # data |> group_by(v_cat) |> summarise(mi = min(v), ma = max(v)) |> arrange(mi) |> View()
  case_when(
    valor <= 0.457 ~ "BAJO",
    valor <= 0.510 ~ "MEDIO BAJO",
    valor <= 0.555 ~ "MEDIO ALTO",
    TRUE           ~ "ALTO",
  )
}

formatear_numero <- function(valor){
  # valor <- runif(1)
  x <- scales::comma(valor, accuracy = 0.001, big.mark = ".", decimal.mark = ",")
  coalesce(x, "-")
}

str_clean <- function (x)  {
  # assertthat::assert_that(is.character(x) | is.factor(x))
  x |>
    as.character() |>
    stringr::str_trim() |>
    janitor::make_clean_names() |>
    str_replace_all("_", " ")
}

get_vb <- function(comuna, region, codigo_comuna, v, v_cat, v_gauge, v1, v1_cat, v1_gauge, v2, v2_cat, v2_gauge, v3, v3_cat, v3_gauge){

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
    col_widths = c(6, 6, 12, 12),
    # fill = FALSE, fillable = FALSE,
    col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Conectividad Hogar")),
    col(style = "text-align: right",tags$small(coalesce(v1_cat, "-")), tags$h1(formatear_numero(v1))),
    col(horizontal_gauge_html(percent = v1_gauge, height = 10)),
    tags$br(),
    col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Educación Digital")),
    col(style = "text-align: right",tags$small(coalesce(v3_cat, "-")), tags$h1(formatear_numero(v3))),
    col(horizontal_gauge_html(percent = v3_gauge, height = 10)),
    tags$br(),
    col(style="height: 100%;position: relative", tags$h5(style = "position: absolute;bottom: 0", "Municipio Digital")),
    col(style = "text-align: right",tags$small(coalesce(v2_cat, "-")), tags$h1(formatear_numero(v2))),
    col(horizontal_gauge_html(percent = v2_gauge, height = 10)),
    tags$br()
  )

  c <- bslib::card(
    full_screen = FALSE,
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

}
