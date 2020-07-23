# Module UI
  
#' @title   mod_overlap_ui and mod_overlap_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_overlap
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_overlap_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_overlap
#' @export
#' @keywords internal
    
mod_overlap_server <- function(input, output, session){
  ns <- session$ns
  
  
  # 2. Update the list of overlapping tiles and the tiles on the map view_map
  if (map == "view_map") {
    if (length(rv$extent) > 0) {
      rv$draw_tiles_overlapping <- s2tiles[unique(unlist(suppressMessages(st_intersects(st_transform(rv$extent, 4326), s2tiles)))), ]
      
      if (attr(rv$extent, "new")) {
        # update the list of tiles
        updateCheckboxGroupInput(
          session, "tiles_checkbox",
          choiceNames = lapply(rv$draw_tiles_overlapping$tile_id, span, style = "family:monospace;"),
          choiceValues = rv$draw_tiles_overlapping$tile_id,
          selected = rv$draw_tiles_overlapping$tile_id,
          inline = nrow(rv$draw_tiles_overlapping) > 8 # inline if they are many
        )
      }
      
      # reset and update the map
      react_map(base_map())
      rv$draw_tiles_overlapping_ll <- st_transform(rv$draw_tiles_overlapping, 4326)
      rv$extent_ll <- st_transform(rv$extent, 4326)
      leafletProxy("view_map") %>%
        clearShapes() %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[, "X"]),
          lat1 = min(st_coordinates(rv$draw_tiles_overlapping_ll)[, "Y"]),
          lng2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[, "X"]),
          lat2 = max(st_coordinates(rv$draw_tiles_overlapping_ll)[, "Y"])
        ) %>%
        addPolygons(
          data = rv$draw_tiles_overlapping,
          group = "S2 tiles",
          label = ~tile_id,
          labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "orange",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "red"
        ) %>%
        # add extent
        addPolygons(
          data = rv$extent_ll,
          group = "Extent",
          # label = ~ccod_frt,
          # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
          fill = TRUE,
          fillColor = "blue",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        )
    } else {
      rv$draw_tiles_overlapping <- NULL
      # empty the list of tiles
      updateCheckboxGroupInput(session, "tiles_checkbox",
                               choices = NULL
      )
      # reset the map
      react_map(base_map())
    }
  } else if (map == "view_map_presabs") {
    if (length(rv$extent_pa) > 0) {
      # reset and update the map view_map_presabs
      react_map_presabs(base_map(map = "view_map_presabs"))
      rv$extent_pa_ll <- st_transform(rv$extent_pa, 4326)
      
      leafletProxy("view_map_presabs") %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$extent_pa_ll)[, "X"]),
          lat1 = min(st_coordinates(rv$extent_pa_ll)[, "Y"]),
          lng2 = max(st_coordinates(rv$extent_pa_ll)[, "X"]),
          lat2 = max(st_coordinates(rv$extent_pa_ll)[, "Y"])
        ) %>%
        clearShapes() %>%
        # add extent
        addCircleMarkers(
          data = rv$extent_pa_ll %>%
            filter(obs == 0),
          group = "Extent",
          fill = TRUE,
          fillColor = "green",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        ) %>%
        addCircleMarkers(
          data = rv$extent_pa_ll %>%
            filter(obs == 1),
          group = "Extent",
          fill = TRUE,
          fillColor = "red",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkred"
        )
    }
  } else if (map == "view_map_mask") {
    if (length(rv$extent_mask) > 0) {
      # reset and update the map view_map_mask
      react_map_mask(base_map(map = "view_map_mask"))
      rv$extent_mask_ll <- st_transform(rv$extent_mask, 4326)
      
      leafletProxy("view_map_mask") %>%
        fitBounds(
          lng1 = min(st_coordinates(rv$extent_mask_ll)[, "X"]),
          lat1 = min(st_coordinates(rv$extent_mask_ll)[, "Y"]),
          lng2 = max(st_coordinates(rv$extent_mask_ll)[, "X"]),
          lat2 = max(st_coordinates(rv$extent_mask_ll)[, "Y"])
        ) %>%
        clearShapes() %>%
        # add extent
        addPolygons(
          data = rv$extent_mask_ll,
          group = "Extent",
          fill = TRUE,
          fillColor = "blue",
          fillOpacity = .3,
          stroke = TRUE,
          weight = 3,
          color = "darkgreen"
        )
    }
  } else if (map == "view_map_prevision") {
    react_map_prevision(base_map(map = "view_map_prevision"))
    rf_predict_files <-  list.files(file.path(paste0(input$path_project_textin, "/projets/", input$project_name, "/pred/sdm")),
                                    pattern = 'rf_predict_point_extent_mask.gpkg$',
                                    full.names = TRUE,
                                    recursive = TRUE)
    rf_predict_tbl <- suppressMessages(
      purrr::map(
        rf_predict_files,
        ~sf::st_read(., quiet = TRUE) %>%
          st_transform(4326) %>%
          st_coordinates() %>%
          as_tibble() %>%
          dplyr::select(X,Y) %>%
          dplyr::filter(!is.na(X))) %>% 
        magrittr::set_names(basename(dirname(rf_predict_files)))
    )
    
    leaf <- leafletProxy("view_map_prevision")
    
    purrr::walk(
      names(rf_predict_tbl),
      function(day) {
        leaf <<- leaf %>%
          addHeatmap(
            data = rf_predict_tbl[[day]],
            layerId = day, group = day,
            lng=~X, lat=~Y,
            blur = 20, 
            max = 0.05, 
            radius = 10,
            gradient = 'red')
      }
    )
    
    extent_pre_ll <- rf_predict_tbl[[1]]
    
    leaf %>%
      fitBounds(
        lng1 = min(extent_pre_ll$X),
        lat1 = min(extent_pre_ll$Y),
        lng2 = max(extent_pre_ll$X),
        lat2 = max(extent_pre_ll$Y)
      ) %>%
      addLayersControl(
        overlayGroups = names(rf_predict_tbl),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  }
  
  return(TRUE)
}
    
## To be copied in the UI
# mod_overlap_ui("overlap_ui_1")
    
## To be copied in the server
# callModule(mod_overlap_server, "overlap_ui_1")
 
