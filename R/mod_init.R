# Module UI
  
#' @title   mod_init_ui and mod_init_server
#' @description  Initialisation rv.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_init
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_init_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_init
#' @export
#' @keywords internal
    
mod_init_server <- function(input, output, session){
  ns <- session$ns
  
  # initialise rv
  # (list of reactive values to be passed as output)
  rv <- reactiveValues()
  
  # get server volumes
  volumes <- c("Home" = path.expand("~"), shinyFiles::getVolumes()())
}
    
## To be copied in the UI
# mod_init_ui("init_ui_1")
    
## To be copied in the server
# callModule(mod_init_server, "init_ui_1")
 
