# Module UI
  
#' @title   mod_modal_dialog_ui and mod_modal_dialog_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_modal_dialog
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_modal_dialog_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_modal_dialog
#' @export
#' @keywords internal
    
mod_modal_dialog_server <- function(input, output, session){
  ns <- session$ns
  
  # build the modal dialog preprocessing
  cnes_download_modal <- reactive({
    modalDialog(
      title = "Telechargement des produits",
      size = "s",
      uiOutput("cnes_download_message"),
      easyClose = FALSE,
      footer = NULL
    )
  })
  
  # build the modal dialog prediction
  cnes_prediction_modal <- reactive({
    modalDialog(
      title = "Prediction",
      size = "s",
      uiOutput("cnes_prediction_message"),
      easyClose = FALSE,
      footer = NULL
    )
  })
}
    
## To be copied in the UI
# mod_modal_dialog_ui("modal_dialog_ui_1")
    
## To be copied in the server
# callModule(mod_modal_dialog_server, "modal_dialog_ui_1")
 
