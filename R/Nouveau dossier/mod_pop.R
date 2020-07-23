# Module UI
  
#' @title   mod_pop_ui and mod_pop_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_pop
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_pop_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    br(),
    column(width=6,
           fluidRow(
             box(
               title = ("Carte des essences"), ## Le but c'est de pouvoir le faire pour n'importe quel parametre,
               ## la c'est juste un exemple mais il faut l'automatiser pour que ca soit dispo pour tous les parametres necessaires
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("listimage01", "Tuiles telechargees :", c("No tile" = "")),
               div(
                 style = "display:inline-block;horizontal-align:center;",
                 imageOutput("image01", height = 400, width = 500)
               ), # end div
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             )
           )
    ), 
    column(width=6,
           fluidRow(
             box(
               title = ("Incertitude sur les essences"), ## Le but c'est de pouvoir le faire pour n'importe quel parametre,
               ## la c'est juste un exemple mais il faut l'automatiser pour que ca soit dispo pour tous les parametres necessaires
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("listimage01", "Tuiles telechargees :", c("No tile" = "")),
               div(
                 style = "display:inline-block;horizontal-align:center;",
                 imageOutput("image01", height = 400, width = 500)
               ), # end div
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             )
           )
    ), # end column# end column
    br(),
    br(),
    column(width=6,
           fluidRow(
             box(
               title = ("Carte du G/ha"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("listimage02", "Tuiles telechargees :", c("No tile" = "")),
               div(
                 style = "display:inline-block;horizontal-align:center;",
                 imageOutput("image02", height = 400, width = 500)
               ), # end div
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             )
           )
    ), # end column
    column(width=6,
           fluidRow(
             box(
               title = ("Incertitude sur le G/ha"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("listimage02", "Tuiles telechargees :", c("No tile" = "")),
               div(
                 style = "display:inline-block;horizontal-align:center;",
                 imageOutput("image02", height = 400, width = 500)
               ), # end div
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             )
           )
    ), # end column
    br(),
    br(),
    column(width=6,
           fluidRow(
             box(
               title = ("Carte du V/ha"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("listimage03", "Tuiles telechargees :", c("No tile" = "")),
               div(
                 style = "display:inline-block;horizontal-align:center;",
                 imageOutput("image03", height = 400, width = 500)
               ), # end div
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             )
           )
    ), # end column
    column(width=6,
           fluidRow(
             box(
               title = ("Incertitude sur le V/ha"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("listimage03", "Tuiles telechargees :", c("No tile" = "")),
               div(
                 style = "display:inline-block;horizontal-align:center;",
                 imageOutput("image03", height = 400, width = 500)
               ), # end div
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             )
           )
    ), # end column
    br(),
    br(),
    column(width=6,
           fluidRow(
             box(
               title = ("Carte de la densite de tiges"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("listimage04", "Tuiles telechargees :", c("No tile" = "")),
               div(
                 style = "display:inline-block;horizontal-align:center;",
                 imageOutput("image04", height = 400, width = 500)
               ), # end div
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             )
           )
    ),
    column(width=6,
           fluidRow(
             box(
               title = ("Incertitude sur la densite de tiges"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               selectInput("listimage04", "Tuiles telechargees :", c("No tile" = "")),
               div(
                 style = "display:inline-block;horizontal-align:center;",
                 imageOutput("image04", height = 400, width = 500)
               ), # end div
               actionButton("export", "Exporter la carte", style = "display:inline-block;horizontal-align:right;")
             )
           )
    )# end column
  )
  
  
  
}

# Module Server
    
#' @rdname mod_pop
#' @export
#' @keywords internal
    
mod_pop_server <- function(input, output, session, rv){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_pop_ui("pop_ui_1")
    
## To be copied in the server
# callModule(mod_pop_server, "pop_ui_1")
 
