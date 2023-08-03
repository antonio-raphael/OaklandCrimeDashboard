#' ZipCodeReporting UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ZipCodeReporting_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ZipCodeReporting Server Functions
#'
#' @noRd 
mod_ZipCodeReporting_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ZipCodeReporting_ui("ZipCodeReporting_1")
    
## To be copied in the server
# mod_ZipCodeReporting_server("ZipCodeReporting_1")
