# Module UI
  
#' @title   mod_forest_mode_ui and mod_forest_mode_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_forest_mode
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_forest_mode_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_forest_mode
#' @export
#' @keywords internal
    
mod_forest_mode_server <- function(input, output, session){
  ns <- session$ns
  
  ############### - Forest mode spatio-temporal -####################
  
  # create a new map (to be shown in modal dialog)
  react_map_forest <- reactiveVal(
    base_map()
  )
  output$view_map_forest <- renderLeaflet({
    react_map_forest()
  })
  
  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_forest, {
    showModal(load_extent_forest())
  })
  
  # load the forest on the map
  observeEvent(input$forest, {
    # Check that the forest is valid
    frt <- str_sub(input$forest, 6)
    agc <- str_sub(input$forest, 1, 4)
    rv$forest_polygon <- if (str_length(frt) == 0) {
      x <- st_polygon()
      attr(x, "valid") <- FALSE
      x
    } else {
      tryCatch({
        x <- forestdata %>%
          filter(ccod_frt == frt, ccod_cact == agc) %>%
          st_transform(4326)
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      },
      error = function(e) {
        x <- st_polygon()
        attr(x, "valid") <- FALSE
        x
      }
      )
    }
    
    if (attr(rv$forest_polygon, "valid")) {
      # if the forest is valid, update the map
      rv$forest_polygon_ll <- st_transform(rv$forest_polygon, 4326)
      leafletProxy("view_map_forest") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = st_bbox(rv$forest_polygon_ll)$xmin[[1]],
          lat1 = st_bbox(rv$forest_polygon_ll)$ymin[[1]],
          lng2 = st_bbox(rv$forest_polygon_ll)$xmax[[1]],
          lat2 = st_bbox(rv$forest_polygon_ll)$ymax[[1]]
        ) %>%
        addPolygons(
          data = rv$forest_polygon_ll,
          group = "Extent",
          # label = ~tile_id,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        ) # %>%
    } else {
      # if the forest is not valid, reset the map
      react_map_forest(base_map())
    }
  })
  
  # use forest
  observeEvent(input$save_extent_forest, {
    withProgress(message = "Creation de l'etendue ...", value = 0, {
      forest_valid <- update_extent(extent_source = "forest")
      if (forest_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Veuillez choisir une foret valide.",
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
# mod_forest_mode_ui("forest_mode_ui_1")
    
## To be copied in the server
# callModule(mod_forest_mode_server, "forest_mode_ui_1")
 
