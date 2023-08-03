#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_MainPage_server("MainPage_1")

  mod_ZipCodeReporting_server("ZipCodeReporting_1")
}
