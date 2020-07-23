# Module UI
  
#' @title   mod_draw_mode_ui and mod_draw_mode_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_draw_mode
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_draw_mode_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_draw_mode
#' @export
#' @keywords internal
    
mod_draw_mode_server <- function(input, output, session){
  ns <- session$ns
  
  ################### - Draw mode spatio-temporal -######################"
  
  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_draw, {
    
    # create a new namespace every time the button is pushed,
    # in order not to make mess between modules
    extent_ns_name <- paste0("editor_", sample(1E9, 1))
    extent_ns <- NS(extent_ns_name)
    rv$extent_edits <- callModule(editModPoly, extent_ns_name, base_map())
    
    # show the modal dialog
    showModal(load_extent_draw(extent_ns_name))
  })
  
  # use bbox
  observeEvent(input$save_extent_draw, {
    withProgress(message = "Creation de l'etendue ...", value = 0, {
      drawn_valid <- update_extent(extent_source = "draw")
      if (drawn_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Veuillez dessiner une etendue valide.",
          text = NULL,
          type = "error",
          btn_labels = "Ok"
        )
      }
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })
  
  
  #- Refresh the extent map if required -#
  observeEvent(input$button_refresh_map, {
    withProgress(message = "Refreshing the map", value = 0, {
      update_extent(extent_source = "fake", map = "view_map")
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })
  
  observeEvent(input$button_refresh_map_pa, {
    withProgress(message = "Rafraichir la carte", value = 0, {
      update_extent(extent_source = "fake", map = "view_map_presabs")
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })
  
  observeEvent(input$button_refresh_map_mask, {
    withProgress(message = "Rafraichir la carte", value = 0, {
      update_extent(extent_source = "fake", map = "view_map_mask")
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })
  
  observeEvent(input$button_refresh_map_prevision, {
    withProgress(message = "Rafraichir la carte", value = 0, {
      update_extent(extent_source = "fake", map = "view_map_prevision")
      for (i in 1:10) {
        incProgress(1 / 10)
        Sys.sleep(0.1)
      }
    })
  })
}
    
## To be copied in the UI
# mod_draw_mode_ui("draw_mode_ui_1")
    
## To be copied in the server
# callModule(mod_draw_mode_server, "draw_mode_ui_1")
 
