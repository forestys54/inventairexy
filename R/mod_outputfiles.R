# Module UI
  
#' @title   mod_outputfiles_ui and mod_outputfiles_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_outputfiles
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_outputfiles_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(width = 6,
           fluidRow(
             box(
               title = ("Fichiers de sortie"),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(
                 width = 12,
                 radioButtons(
                   "save_files",
                   label = span("Enregistrer les fichiers de sortie ?\u2000"),
                   choiceNames = list("Oui", "Non"),
                   choiceValues = list(TRUE, FALSE),
                   selected = TRUE,
                   inline = TRUE
                 )
               ),
               column (
                 width = 12,
                 selectInput("outformatraster",
                             label = ("Format de sortie des raster"),
                             choices = list(
                               "GeoTiff" = "GTiff",
                               "ENVI" = "ENVI",
                               "Bitmap" = "BMP"),
                             selected = "GTiff"
                 )
               ),
               column (
                 width = 12,
                 selectInput("outformatnum",
                             label = ("Format de sortie des donnnees numeriques"),
                             choices = list(
                               "Excel" = "XLS",
                               "CSV" = "CSV"),
                             selected = "XLS"
                 )
               )
             )
           )
    )
  )
}
    
# Module Server
    
#' @rdname mod_outputfiles
#' @export
#' @keywords internal
    
mod_outputfiles_server <- function(input, output, session, rv){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_outputfiles_ui("outputfiles_ui_1")
    
## To be copied in the server
# callModule(mod_outputfiles_server, "outputfiles_ui_1")
 
