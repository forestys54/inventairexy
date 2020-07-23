# Module UI
  
#' @title   mod_mask_module_ui and mod_mask_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_mask_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_mask_module_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_mask_module
#' @export
#' @keywords internal
    
mod_mask_module_server <- function(input, output, session){
  ns <- session$ns
  
  ######## extent module mask ############
  
  # create a new map for prevision view
  react_map_mask <- reactiveVal(
    base_map(map = "view_map_presabs")
  )
  output$view_map_mask <- renderLeaflet({
    react_map_mask()
  })
  
  observeEvent(input$path_vectfile_mask_sel, {
    uploaded_exts <- gsub("^.+\\.(.+)$", "\\1", input$path_vectfile_mask_sel$name)
    # checks
    if (length(unique(gsub("\\..+$", "", input$path_vectfile_mask_sel$name))) > 1) {
      # if more than one vector were chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = "Vecteur invalide",
        text = paste(
          "Veuillez selectionner un vecteur unique",
          "(la selection multiple n'est disponible que pour les fichiers shapefiles)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_mask_path <- ""
    } else if (length(uploaded_exts) == 1 && !uploaded_exts %in% c("shp", "shx", "dbf", "prj")) {
      # if a single file was chosen and it is not a shapefile, use it
      rv$vectfile_mask_path <- input$path_vectfile_mask_sel$datapath
    } else if (anyNA(match(c("shp", "shx", "dbf", "prj"), uploaded_exts))) {
      # if a shapefile was chosen but some files are missing, do not use it
      sendSweetAlert(
        session,
        title = "Shapefile incomplet",
        text = paste(
          "Veuillez selectionner tous les fichiers du shapefile",
          "(at most .shp, .shx, .prj, .dbf)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_mask_path <- ""
    } else {
      # if a shapefile was chosen and all the files are present,
      # rename the uploaded files in order to have the same filename and use them
      path_vectfile_mask_sel_new_datapath <- file.path(
        dirname(input$path_vectfile_mask_sel$datapath), input$path_vectfile_mask_sel$name
      )
      for (i in seq_len(nrow(input$path_vectfile_mask_sel))) {
        file.rename(input$path_vectfile_mask_sel$datapath[i], path_vectfile_mask_sel_new_datapath[i])
      }
      rv$vectfile_mask_path <- path_vectfile_mask_sel_new_datapath[
        input$path_vectfile_mask_sel$type == "application/x-esri-shape"
        ]
    }
  }) # possible de coupler avec module_presabs puisque elements communs jusque la ? 
  
  # create a new map for mask view
  react_map_vectfile_mask <- reactiveVal(
    base_map(map = "view_map_mask")
  )
  output$view_map_vectfile_mask <- renderLeaflet({
    react_map_vectfile_mask()
  })
  
  # Open modal dialog to load the vector file
  observeEvent(input$button_extent_vectfile_mask, {
    rv$vectfile_path_mask <- ""
    showModal(load_extent_vectfile_mask())
  })
  
  # load the vector on the map
  observeEvent(rv$vectfile_mask_path, {
    
    # Check that the vector is valid
    rv$vectfile_mask <- tryCatch({
      x <- st_read(rv$vectfile_mask_path, quiet = TRUE) %>%
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
    
    if (attr(rv$vectfile_mask, "valid")) {
      # if the vector is valid, update the map
      rv$vectfile_mask_ll <- st_transform(rv$vectfile_mask, 4326)
      leafletProxy("view_map_vectfile_mask") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$vectfile_mask_ll)[, "X"]),
          lat1 = min(st_coordinates(rv$vectfile_mask_ll)[, "Y"]),
          lng2 = max(st_coordinates(rv$vectfile_mask_ll)[, "X"]),
          lat2 = max(st_coordinates(rv$vectfile_mask_ll)[, "Y"])
        ) %>%
        addPolygons(
          data = rv$vectfile_mask_ll,
          group = "Extent",
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        )
    } else {
      # if the vector is not valid, reset the map
      react_map_vectfile_mask(base_map(map = "view_map_mask"))
    }
  })
  
  # use bbox
  observeEvent(input$save_extent_vectfile_mask, {
    withProgress(message = "Creation de l'etendue", value = 0, {
      vectfile_mask_valid <- update_extent(extent_source = "mask", map = "view_map_mask")
      if (vectfile_mask_valid) {
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
}
    
## To be copied in the UI
# mod_mask_module_ui("mask_module_ui_1")
    
## To be copied in the server
# callModule(mod_mask_module_server, "mask_module_ui_1")
 
