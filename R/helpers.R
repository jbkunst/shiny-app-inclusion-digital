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
