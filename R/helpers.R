horizontal_gauge_html <- function(percent){

  p2 <- round(percent * 100, 0)

  # https://www.color-hex.com/color-palette/35021
  tags$div(
    style = "width: 100%; height: 10px; display: flex; position: relative;",
    tags$div(style = "background-color: #cc3232; flex: 1; height: 100%"),
    tags$div(style = "background-color: #99c140; flex: 1; height: 100%"),
    tags$div(style = "background-color: #db7b2b; flex: 1; height: 100%"),
    tags$div(style = "background-color: #2dc937; flex: 1; height: 100%"),
    tags$div(style = stringr::str_glue("left: {p2}%; position: absolute; top: 50%; transform: translate(-50%, -50%); width: 15px; height: 15px; background-color: #F4F4F4; border: 2px solid #5D5E60; border-radius: 50%; box-shadow: 0 0 5px rgba(0, 0, 0, 0.5);"))
  )


}
