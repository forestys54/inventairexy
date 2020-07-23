# Module UI
  
#' @title   mod_presabs_ui and mod_presabs_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_presabs
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_presabs_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_presabs
#' @export
#' @keywords internal
    
mod_presabs_server <- function(input, output, session){
  ns <- session$ns
  
  ######## extent module pa ############
  
  # create a new map for view_map_presabs
  react_map_presabs <- reactiveVal(
    base_map(map = "view_map_presabs")
  )
  output$view_map_presabs <- renderLeaflet({
    react_map_presabs()
  })
  
  observeEvent(input$path_vectfile_pa_sel, {
    uploaded_exts <- gsub("^.+\\.(.+)$", "\\1", input$path_vectfile_pa_sel$name)
    # checks
    if (length(unique(gsub("\\..+$", "", input$path_vectfile_pa_sel$name))) > 1) {
      # if more than one vector were chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = "Vecteur invalide",
        text = paste(
          "Veuillez choisir un vecteur unique",
          "(la selection multiple n'est possible que pour les shapefiles)."
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$vectfile_pa_path <- ""
    } else if (length(uploaded_exts) == 1 && !uploaded_exts %in% c("shp", "shx", "dbf", "prj")) {
      # if a single file was chosen and it is not a shapefile, use it
      rv$vectfile_pa_path <- input$path_vectfile_pa_sel$datapath
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
      rv$vectfile_pa_path <- ""
    } else {
      # if a shapefile was chosen and all the files are present,
      # rename the uploaded files in order to have the same filename and use them
      path_vectfile_pa_sel_new_datapath <- file.path(
        dirname(input$path_vectfile_pa_sel$datapath), input$path_vectfile_pa_sel$name
      )
      for (i in seq_len(nrow(input$path_vectfile_pa_sel))) {
        file.rename(input$path_vectfile_pa_sel$datapath[i], path_vectfile_pa_sel_new_datapath[i])
      }
      rv$vectfile_pa_path <- path_vectfile_pa_sel_new_datapath[
        input$path_vectfile_pa_sel$type == "application/x-esri-shape"
        ]
    }
  })
  
  # # create a new map for presence absence view
  # react_map_vectfile_pa <- reactiveVal(
  #   base_map(map = "view_map_presabs")
  # )
  # output$view_map_vectfile_pa <- renderLeaflet({
  #   react_map_vectfile_pa()
  # })
  
  # Open modal dialog to load the vector file
  observeEvent(input$button_extent_vectfile_pa, {
    rv$vectfile_path_pa <- ""
    showModal(load_extent_vectfile_pa())
  }) # WILL GO TO UI ?
  
  # loaddirNd the vector on the map
  observeEvent(rv$vectfile_pa_path, {
    
    # Check that the vector is valid
    rv$vectfile_pa_point <- tryCatch({
      x <- st_read(rv$vectfile_pa_path, quiet = TRUE) %>%
        st_transform(4326)
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    },
    error = function(e) {
      x <- st_point()
      attr(x, "valid") <- FALSE
      x
    }
    )
    
    if (attr(rv$vectfile_pa_point, "valid")) {
      # if the vector is valid, update the map
      leafletProxy("view_map_vectfile_pa") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$vectfile_pa_point)[, "X"]),
          lat1 = min(st_coordinates(rv$vectfile_pa_point)[, "Y"]),
          lng2 = max(st_coordinates(rv$vectfile_pa_point)[, "X"]),
          lat2 = max(st_coordinates(rv$vectfile_pa_point)[, "Y"])
        ) %>%
        addCircleMarkers(
          data = rv$vectfile_pa_point %>%
            filter(obs == 1), 
          color= "red") %>%
        addCircleMarkers(
          data = rv$vectfile_pa_point %>%
            filter(obs == 0), 
          color= "green") 
    } else {
      # if the vector is not valid, reset the map
      react_map_vectfile_pa(base_map(map = "view_map_presabs"))
    }
  })
  
  # use bbox
  observeEvent(input$save_extent_vectfile_pa, {
    withProgress(message = "Creation de l'etendue ...", value = 0, {
      vectfile_pa_valid <- update_extent(extent_source = "presabs", map = "view_map_presabs")
      if (vectfile_pa_valid) {
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
# mod_presabs_ui("presabs_ui_1")
    
## To be copied in the server
# callModule(mod_presabs_server, "presabs_ui_1")
 
