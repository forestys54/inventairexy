# Module UI
  
#' @title   mod_outputoptions_ui and mod_outputoptions_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_outputoptions
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_outputoptions_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 6,
           fluidRow(
             box(
               title = ("Options de sortie"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(
                 width = 12,
                 radioButtons(
                   "select_only",
                   label = span("Reduire les calculs a l'etendue selectionnee ?\u2000"),
                   choiceNames = list("Oui", "Non"),
                   choiceValues = list(TRUE, FALSE),
                   selected = TRUE,
                   inline = TRUE
                 )
               ),
               column(
                 width = 12,
                 radioButtons(
                   "replace",
                   label = span("Remplacer les produits existants ?\u2000"),
                   choiceNames = list("Oui", "Non"),
                   choiceValues = list(TRUE, FALSE),
                   selected = TRUE,
                   inline = TRUE
                 )
               )
             )
           )
    )
           
  )
}
    
# Module Server
    
#' @rdname mod_outputoptions
#' @export
#' @keywords internal
    
mod_outputoptions_server <- function(input, output, session, rv){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_outputoptions_ui("outputoptions_ui_1")
    
## To be copied in the server
# callModule(mod_outputoptions_server, "outputoptions_ui_1")
 
