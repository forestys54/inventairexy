# Module UI
  
#' @title   mod_prevision_map_ui and mod_prevision_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_prevision_map
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_prevision_map_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_prevision_map
#' @export
#' @keywords internal
    
mod_prevision_map_server <- function(input, output, session){
  ns <- session$ns
  
  react_map_prevision <- reactiveVal(
    base_map(map = "view_map_prevision")
  )
  output$view_map_prevision <- renderLeaflet({
    react_map_prevision()
  })
}
    
## To be copied in the UI
# mod_prevision_map_ui("prevision_map_ui_1")
    
## To be copied in the server
# callModule(mod_prevision_map_server, "prevision_map_ui_1")
 
