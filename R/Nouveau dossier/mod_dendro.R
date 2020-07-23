# Module UI
  
#' @title   mod_dendro_ui and mod_dendro_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dendro
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_dendro_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 4,
           fluidRow(
             box(
               title = ("Donnees dendrometriques"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong("Telecharger les placettes inventaires : \u00a0")),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_placettes_sel", ("Selec"), ("Selectionner le fichier contenant les placettes"))
                 ),
                 div(
                   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   textInput("path_placettes", NULL, "")
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong("Telecharger les donnees dendrometriques : \u00a0")),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_dendro_sel", ("Selec"), ("Selectionner les donnees dendrometriques"))
                 ),
                 div(
                   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   textInput("path_dendro", NULL, "")
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong("Telecharger la localisation des arbres : \u00a0")),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_arbres_sel", ("Selec"), ("Selectionner le fichier contenant la geolocalisation des arbres"))
                 ),
                 div(
                   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   textInput("path_arbres", NULL, "")
                 )
               )
             )
           )
    )
  )
}
    
# Module Server
    
#' @rdname mod_dendro
#' @export
#' @keywords internal
    
mod_dendro_server <- function(input, output, session, rv){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_dendro_ui("dendro_ui_1")
    
## To be copied in the server
# callModule(mod_dendro_server, "dendro_ui_1")
 
