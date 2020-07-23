# Module UI
  
#' @title   mod_paths_update_ui and mod_paths_update_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_paths_update
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_paths_update_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_paths_update
#' @export
#' @keywords internal
    
mod_paths_update_server <- function(input, output, session){
  ns <- session$ns
  
  ###### Path module ######
  
  # Accessory functions to check that the new directory exists and is writable
  path_check <- function(path) {
    if (length(path) > 0 & path[1] != "") {
      if (!dir.exists(path)) {
        return(renderUI(span(
          style = "color:red",
          "\u2718 (la direction n'existe pas)"
        )))
      } else if (file.access(path, mode = 2) < 0) {
        return(renderUI(span(
          style = "color:red",
          "\u2718 (certains caracteres ne sont pas pris en compte)" #the directory is not writable)")
        )))
      } else {
        return(renderUI(span(
          style = "color:darkgreen",
          "\u2714"
        )))
      }
      #
    } else {
      return(renderText(""))
    }
  }
  
  shinyDirChoose(input, "path_project_sel", roots = volumes)
  shinyDirChoose(input, "path_model_sel", roots = volumes)
  
  # if paths change after using the shinyDirButton, update the values and the textInput
  observe({
    path_project_string <- parseDirPath(volumes, input$path_project_sel)
    updateTextInput(session, "path_project_textin", value = path_project_string)
  })
  observe({
    path_model_string <- parseDirPath(volumes, input$path_model_sel)
    updateTextInput(session, "path_model_textin", value = path_model_string)
  })
  
  
  # if path changes after using the textInput, update the value
  observe({
    output$path_project_errormess <- path_check(input$path_project_textin)
  })
  observe({
    output$path_model_errormess <- path_check(input$path_model_textin)
  })
}
    
## To be copied in the UI
# mod_paths_update_ui("paths_update_ui_1")
    
## To be copied in the server
# callModule(mod_paths_update_server, "paths_update_ui_1")
 
