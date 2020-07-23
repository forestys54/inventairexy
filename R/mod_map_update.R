# Module UI
  
#' @title   mod_map_update_ui and mod_map_update_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_map_update
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
#'  #-- Function to update the map and the list of tiles --#
#' it returns TRUE if the input extent source was correctly read, FALSE elsewhere.
#' argument extent_source determines which source to be used:
#' "bbox", "vectfile", "draw" from selection buttons, "imported" from parameter;
#' in this case, the argument "custom_source" is the source to be passed.
#'
mod_map_update_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_map_update
#' @export
#' @keywords internal
    
mod_map_update_server <- function(extent_source, custom_service = NA, map = "view_map") {
  ns <- session$ns
  
  # 1. Define rv$extent
  if (extent_source == "forest") {
    # Forest mode #
    # check that the polygon is valid
    if (attr(rv$forest_polygon, "valid")) { # get or set specific attributes on an object
      rv$extent <- rv$forest_polygon
      attr(rv$extent, "new") <- TRUE
    } else {
      return(FALSE)
    }
  } else if (extent_source == "bbox") {
    # Bbox mode #
    # check that the polygon is valid
    if (attr(rv$bbox_polygon, "valid")) {
      rv$extent <- rv$bbox_polygon
      attr(rv$extent, "new") <- TRUE
    } else {
      return(FALSE)
    }
  } else if (extent_source == "vectfile") {
    # Vectfile mode #
    # check that the polygon is valid
    if (attr(rv$vectfile_polygon, "valid")) {
      rv$extent <- rv$vectfile_polygon
      attr(rv$extent, "new") <- TRUE
    } else {
      return(FALSE)
    }
  } else if (extent_source == "presabs") {
    # Pointfile mode #
    # check that the point is valid
    if (attr(rv$vectfile_pa_point, "valid")) {
      rv$extent_pa <- rv$vectfile_pa_point
      attr(rv$extent_pa, "new") <- TRUE
    } else {
      return(FALSE)
    }
  } else if (extent_source == "mask") {
    # Vectfile mode #
    # check that the polygon is valid
    if (attr(rv$vectfile_mask, "valid")) {
      rv$extent_mask <- rv$vectfile_mask
      attr(rv$extent_mask, "new") <- TRUE
    } else {
      return(FALSE)
    }
  } else if (extent_source == "draw") {
    # Drawn mode #
    # namespace for extent selection
    sel_drawn <- if (!is.null(rv$extent_edits()$finished)) {
      x <- rv$extent_edits()$finished
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    } else {
      x <- st_polygon()
      attr(x, "valid") <- FALSE
      x
    }
    if (!attr(sel_drawn, "valid")) {
      return(FALSE)
    }
    rv$extent <- sel_drawn
  } else if (extent_source == "imported") {
    # Imported from parameters #
    sel_imported_extent <- if (is.null(custom_source) | anyNA(custom_source)) {
      x <- st_polygon()
      attr(x, "valid") <- FALSE
      x
    } else {
      x <- st_read(custom_source, quiet = TRUE) %>%
        st_transform(4326) # 4326 = le port ?
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    }
    if (!attr(sel_imported_extent, "valid")) {
      return(FALSE)
    }
    rv$extent <- sel_imported_extent
  } else if (extent_source == "importedmask") {
    # Imported from parameters #
    sel_imported_extent_mask <- if (is.null(custom_source) | anyNA(custom_source)) {
      x <- st_polygon()
      attr(x, "valid") <- FALSE
      x
    } else {
      x <- st_read(custom_source, quiet = TRUE) %>%
        st_transform(4326)
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    }
    if (!attr(sel_imported_extent_mask, "valid")) {
      return(FALSE)
    }
    rv$extent_mask <- sel_imported_extent_mask
  } else if (extent_source == "importedpa") {
    # Imported from parameters #
    sel_imported_extent_pa <- if (is.null(custom_source) | anyNA(custom_source)) {
      x <- st_point()
      attr(x, "valid") <- FALSE
      x
    } else {
      x <- st_read(custom_source, quiet = TRUE) %>%
        st_transform(4326)
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    }
    if (!attr(sel_imported_extent_pa, "valid")) {
      return(FALSE)
    }
    rv$extent_pa <- sel_imported_extent_pa
  } else {
    # For any other value of extent_source, use the existing rv$extent and
    # rv$extent_pa
    if (is.null(rv$extent)) {
      return(FALSE)
    } else if (!attr(rv$extent, "valid")) {
      return(FALSE)
    } else {
      attr(rv$extent, "new") <- FALSE
    }
    if (is.null(rv$extent_pa)) {
      return(FALSE)
    } else if (!attr(rv$extent_pa, "valid")) {
      return(FALSE)
    } else {
      attr(rv$extent_pa, "new") <- FALSE
    }
  }
  
  
  
}
    
## To be copied in the UI
# mod_map_update_ui("map_update_ui_1")
    
## To be copied in the server
# callModule(mod_map_update_server, "map_update_ui_1")
 
