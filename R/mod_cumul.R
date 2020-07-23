# Module UI
  
#' @title   mod_cumul_ui and mod_cumul_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_cumul
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_cumul_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    br(),
    column(width=12,
           fluidRow(
             box(
               title = ("Selection de l'entite"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 6,
               column(
                 width = 12,
                 radioButtons(
                   "entite", NULL,
                   choiceNames = list(
                     span("Parcelles"),
                     span("Hectares"),
                     span("Classes")),
                   choiceValues = list("parcelles", "classes", "hectares"), 
                   selected = 1,
                   inline = TRUE
                 )
               )
             ), # end box
             box(
               title = ("Selection personnalisee"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 6,
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong("Selectionner les entites : \u00a0")),
                 div(
                   style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
                   textInput("par_selec", NULL, "")
                 )
               )
             ) # end box
           )
    ), # end column
    
    br(),
    column(width=12,
           fluidRow(
             box(
               title = ("Apercu des resultats sur les entites selectionnees"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               dataTableOutput("cumul_preview"),
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             ) # end box
           ) 
    ) # end column
  )
}
    
# Module Server
    
#' @rdname mod_cumul
#' @export
#' @keywords internal
    
mod_cumul_server <- function(input, output, session, rv){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_cumul_ui("cumul_ui_1")
    
## To be copied in the server
# callModule(mod_cumul_server, "cumul_ui_1")
 
