# Module UI
  
#' @title   mod_bbox_mode_ui and mod_bbox_mode_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_bbox_mode
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_bbox_mode_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_bbox_mode
#' @export
#' @keywords internal
    
mod_bbox_mode_server <- function(input, output, session){
  ns <- session$ns
  
  ############### - Bbox mode spatio-temporal -####################
  
  # message for bboxproj
  output$bboxproj_message <- renderUI({
    bboxproj_validated <- tryCatch(
      st_crs2(input$bboxproj),
      error = function(e) {
        st_crs(NA)
      }
    )$proj4string
    if (input$bboxproj == "") {
      rv$bboxproj <- NA
      ""
    } else if (is.na(bboxproj_validated)) {
      rv$bboxproj <- NA
      span(
        style = "color:red",
        "Insert a valid projection (EPSG code)."
      )
    } else {
      rv$bboxproj <- bboxproj_validated
      # span(style="color:darkgreen", "\u2714") # check
      div(strong("Selected projection:"),
          br(),
          bboxproj_validated,
          style = "color:darkgreen"
      )
    }
  })
  
  # create a new map (to be shown in modal dialog)
  react_map_bbox <- reactiveVal(
    base_map()
  )
  output$view_map_bbox <- renderLeaflet({
    react_map_bbox()
  })
  
  # Open modal dialog to edit bbox
  observeEvent(input$button_extent_bbox, {
    showModal(load_extent_bbox())
  })
  
  # update the map dynamically
  observeEvent(c(
    input$bbox_xmin, input$bbox_xmax,
    input$bbox_ymin, input$bbox_ymax,
    rv$bboxproj
  ), {
    
    # Check that the bounding box is valid
    if (!anyNA(c(
      input$bbox_xmin, input$bbox_xmax,
      input$bbox_ymin, input$bbox_ymax
    )) &
    !(is.null(rv$bboxproj) || is.na(rv$bboxproj))) {
      if (input$bbox_xmin != input$bbox_xmax &
          input$bbox_ymin != input$bbox_ymax) {
        # create the polygon
        rv$bbox_polygon <- st_as_sfc(
          st_bbox(
            c(
              "xmin" = input$bbox_xmin,
              "ymin" = input$bbox_ymin,
              "xmax" = input$bbox_xmax,
              "ymax" = input$bbox_ymax
            ),
            crs = rv$bboxproj
          )
        ) %>% st_transform(4326)
        attr(rv$bbox_polygon, "valid") <- TRUE
      } else {
        rv$bbox_polygon <- st_polygon()
        attr(rv$bbox_polygon, "valid") <- FALSE
      }
    } else {
      rv$bbox_polygon <- st_polygon()
      attr(rv$bbox_polygon, "valid") <- FALSE
    }
    
    # if bbox is valid, update the map
    if (attr(rv$bbox_polygon, "valid")) {
      rv$bbox_ll <- st_bbox(st_transform(rv$bbox_polygon, 4326))
      leafletProxy("view_map_bbox") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = as.numeric(rv$bbox_ll$xmin - (rv$bbox_ll$xmax - rv$bbox_ll$xmin) / 3),
          lat1 = as.numeric(rv$bbox_ll$ymin - (rv$bbox_ll$ymax - rv$bbox_ll$ymin) / 3),
          lng2 = as.numeric(rv$bbox_ll$xmax + (rv$bbox_ll$xmax - rv$bbox_ll$xmin) / 3),
          lat2 = as.numeric(rv$bbox_ll$ymax + (rv$bbox_ll$ymax - rv$bbox_ll$ymin) / 3)
        ) %>%
        addPolygons(
          data = rv$bbox_polygon,
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
      # if bbox is not valid, reset the map
      react_map_bbox(base_map())
    }
  })
  
  # use bbox
  observeEvent(input$save_extent_bbox, {
    # Add a progress bar while update_extent is running
    withProgress(message = "Creation de l'etendue ...", value = 0, {
      bbox_valid <- update_extent(extent_source = "bbox")
      if (bbox_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Invalid bounding box",
          text = paste(
            "Veuillez inserer une bbox valide."
          ),
          type = "error",
          btn_labels = "Ok"
        )
      }
      # Fake progress
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
# mod_bbox_mode_ui("bbox_mode_ui_1")
    
## To be copied in the server
# callModule(mod_bbox_mode_server, "bbox_mode_ui_1")
 
