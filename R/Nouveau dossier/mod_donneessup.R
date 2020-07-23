# Module UI
  
#' @title   mod_donneessup_ui and mod_donneessup_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_donneessup
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_donneessup_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width=4,
           fluidRow(
             box(
               title = ("Donnees supplementaires"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong("Telecharger le parcellaire : \u00a0")),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_parcelles_sel", ("Selec"), ("Selectionner le parcellaire"))
                 ),
                 div(
                   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   textInput("path_parcelles", NULL, "")
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong("Telecharger le maillage : \u00a0")),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_maillage_sel", ("Selec"), ("Selectionner le maillage"))
                 ),
                 div(
                   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   textInput("path_maillage", NULL, "")
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong("Telecharger les classes de peuplements : \u00a0")),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_classes_sel", ("Selec"), ("Selectionner le fichier contenant les classes de peuplements"))
                 ),
                 div(
                   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   textInput("path_classes", NULL, "")
                 )
               )
             )
           )
    )
  )
}
    
# Module Server
    
#' @rdname mod_donneessup
#' @export
#' @keywords internal
    
mod_donneessup_server <- function(input, output, session, rv){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_donneessup_ui("donneessup_ui_1")
    
## To be copied in the server
# callModule(mod_donneessup_server, "donneessup_ui_1")
 
