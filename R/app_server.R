#' The application server-side
#'
#' Schlanke, reduzierte Version: die gesamte Logik lebt in mod_analyse.
#' @noRd
app_server <- function(input, output, session) {
  mod_analyse_server("analyse")
}
