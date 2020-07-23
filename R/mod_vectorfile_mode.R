# Module UI
  
#' @title   mod_vectorfile_mode_ui and mod_vectorfile_mode_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_vectorfile_mode
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_vectorfile_mode_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_vectorfile_mode
#' @export
#' @keywords internal
    
mod_vectorfile_mode_server <- function(input, output, session){
  ns <- session$ns
  
  ############# - Vector file mode spatio-temporal -###############
  
  observeEvent(input$path_vectfile_sel, {
    uploaded_exts <- gsub("^.+\\.(.+)$", "\\1", input$path_vectfile_sel$name)
    # checks
    if (length(unique(gsub("\\..+$", "", input$path_vectfile_sel$name))) > 1) {
      # if more than one vector were chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = "Invalid vector",
        text = paste(
          "Please select a single vector",
          "(multiple selection is allowed only for shapefiles)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_path <- ""
    } else if (length(uploaded_exts) == 1 && !uploaded_exts %in% c("shp", "shx", "dbf", "prj")) {
      # if a single file was chosen and it is not a shapefile, use it
      rv$vectfile_path <- input$path_vectfile_sel$datapath
    } else if (anyNA(match(c("shp", "shx", "dbf", "prj"), uploaded_exts))) {
      # if a shapefile was chosen but some files are missing, do not use it
      sendSweetAlert(
        session,
        title = "Incomplete shapefile",
        text = paste(
          "Please select all the files of the shapefile",
          "(at most .shp, .shx, .prj, .dbf)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_path <- ""
    } else {
      # if a shapefile was chosen and all the files are present,
      # rename the uploaded files in order to have the same filename and use them
      path_vectfile_sel_new_datapath <- file.path(
        dirname(input$path_vectfile_sel$datapath), input$path_vectfile_sel$name
      )
      for (i in seq_len(nrow(input$path_vectfile_sel))) {
        file.rename(input$path_vectfile_sel$datapath[i], path_vectfile_sel_new_datapath[i])
      }
      rv$vectfile_path <- path_vectfile_sel_new_datapath[
        input$path_vectfile_sel$type == "application/x-esri-shape"
        ]
    }
  })
  
  # create a new map (to be shown in modal dialog)
  react_map_vectfile <- reactiveVal(
    base_map()
  )
  output$view_map_vectfile <- renderLeaflet({
    react_map_vectfile()
  })
  
  # Open modal dialog to load the vector file
  observeEvent(input$button_extent_vectfile, {
    rv$vectfile_path <- ""
    showModal(load_extent_vectfile())
  })
  
  # load the vector on the map
  observeEvent(rv$vectfile_path, {
    
    # Check that the vector is valid
    rv$vectfile_polygon <- tryCatch({
      x <- st_read(rv$vectfile_path, quiet = TRUE) %>%
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
    
    if (attr(rv$vectfile_polygon, "valid")) {
      # if the vector is valid, update the map
      rv$vectfile_polygon_ll <- st_transform(rv$vectfile_polygon, 4326)
      leafletProxy("view_map_vectfile") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$vectfile_polygon_ll)[, "X"]),
          lat1 = min(st_coordinates(rv$vectfile_polygon_ll)[, "Y"]),
          lng2 = max(st_coordinates(rv$vectfile_polygon_ll)[, "X"]),
          lat2 = max(st_coordinates(rv$vectfile_polygon_ll)[, "Y"])
        ) %>%
        addPolygons(
          data = rv$vectfile_polygon_ll,
          group = "Extent",
          # label = ~tile_id,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        )
    } else {
      # if the vector is not valid, reset the map
      react_map_vectfile(base_map())
    }
  })
  
  # use bbox
  observeEvent(input$save_extent_vectfile, {
    withProgress(message = "Creation de l'etendue ...", value = 0, {
      vectfile_valid <- update_extent(extent_source = "vectfile")
      if (vectfile_valid) {
        removeModal()
      } else {
        sendSweetAlert(
          session,
          title = "Veuillez specifier un fichier vectoriel valide.",
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
# mod_vectorfile_mode_ui("vectorfile_mode_ui_1")
    
## To be copied in the server
# callModule(mod_vectorfile_mode_server, "vectorfile_mode_ui_1")
 
