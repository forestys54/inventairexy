# Module UI
  
#' @title   mod_map_creation_ui and mod_map_creation_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_map_creation
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_map_creation_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_map_creation
#' @export
#' @keywords internal
    
mod_map_creation_server <- function(input, output, session){
  ns <- session$ns
  
  #-- Create the map (once) --#
  base_map <- function(map = "view_map") {
    if (map == "view_map_presabs" || map == "view_map_mask") {
      leaflet() %>%
        # add tiles
        addTiles(group = "OpenStreetMap") %>%
        addTiles("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
                 group = "OpenTopoMap"
        ) %>%
        addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
                 group = "CartoDB"
        ) %>%
        addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                 group = "Satellite"
        ) %>%
        addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_only_labels/{z}/{x}/{y}.png",
                 group = "Light names"
        ) %>%
        addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_only_labels/{z}/{x}/{y}.png",
                 group = "Dark names"
        ) %>%
        # view and controls
        addLayersControl(
          baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
          overlayGroups = c("Light names", "Dark names", "Extent"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("Light names", "Dark names"))
    } else if (map == "view_map_prevision") {
      leaflet() %>%
        # addProviderTiles(providers$GeoportailFrance.orthos)
        addProviderTiles(providers$Esri.WorldImagery)
    } else {
      leaflet() %>%
        # add tiles
        addTiles(group = "OpenStreetMap") %>%
        addTiles("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
                 group = "OpenTopoMap"
        ) %>%
        addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
                 group = "CartoDB"
        ) %>%
        addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
                 group = "Satellite"
        ) %>%
        addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_only_labels/{z}/{x}/{y}.png",
                 group = "Light names"
        ) %>%
        addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_only_labels/{z}/{x}/{y}.png",
                 group = "Dark names"
        ) %>%
        # view and controls
        addLayersControl(
          baseGroups = c("OpenStreetMap", "OpenTopoMap", "CartoDB", "Satellite"),
          overlayGroups = c("Light names", "Dark names", "Extent", "S2 tiles"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("Light names", "Dark names"))
    }
  }
  
  # create a new map for principal view
  react_map <- reactiveVal(
    base_map()
  )
  output$view_map <- renderLeaflet({
    react_map()
  })
  
}
    
## To be copied in the UI
# mod_map_creation_ui("map_creation_ui_1")
    
## To be copied in the server
# callModule(mod_map_creation_server, "map_creation_ui_1")
 
