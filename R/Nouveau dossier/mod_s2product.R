# Module UI
  
#' @title   mod_s2product_ui and mod_s2product_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_s2product
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @☺importFrom 
#' 
mod_s2product_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width=4,
           fluidRow(
             box(
               title = ("Tuiles Sentinel 2"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(
                 width = 12,
                 radioButtons(
                   "provenance", NULL,
                   choiceNames = list(
                     span("En ligne"),
                     span("Hors ligne ")),
                   choiceValues = list(TRUE, FALSE),
                   selected = TRUE,
                   inline = TRUE,
                   # conditionalPanel(
                   #   condition = "input.provenance == 'TRUE'"),
                   # div(
                   #   style = "padding-bottom:10px;",
                   #   actionButton(
                   #     "login",
                   #     label = ("\u2000Se connecter sur THEIA"),
                   #     icon = icon("user-circle") # To change
                   #   )), # end div actionButton
                   # conditionPanel(
                   #   condition = "input.provenance == 'FALSE'"),
                   # div(
                   #   style = "display.inline-block;vertical-align:top;width:50px;",
                   #   shinyDirButton("path_tuiles", ("Selectionner"), ("Selectionner les tuiles a ajouter"))
                   # ),
                   # div(
                   #   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   #   textInput("path_mnt", NULL, "")
                   # )
                 ) # end radioButtons
               ), # end column
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong("Telecharger un MNT : \u00a0")),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_mnt_sel", ("Selec"), ("Selectionner le MNT a ajouter"))
                 ),
                 div(
                   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   textInput("path_mnt", NULL, "")
                 )
               )
             )
           ) # end box fluidRow
    )
  )
}
    
# Module Server
    
#' @rdname mod_s2product
#' @export
#' @keywords internal
    
mod_s2product_server <- function(input, output, session, rv){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_s2product_ui("s2product_ui_1")
    
## To be copied in the server
# callModule(mod_s2product_server, "s2product_ui_1")
 
