# Module UI
  
#' @title   mod_object_to_return_ui and mod_object_to_return_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_object_to_return
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_object_to_return_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_object_to_return
#' @export
#' @keywords internal
    
mod_object_to_return_server <- function(input, output, session){
  ns <- session$ns
  
  # function to create a list to objects to be returned C'est ici qu'on a toutes les constantes utilisees partout (repertoires etc)
  create_return_list <- function() {
    rl <- list()
    
    # processing steps #
    rl$project_name <- input$project_name
    # set directories #
    rl$path_project <- input$path_project_textin
    rl$path_model <- input$path_model_textin
    res <- paste0(input$path_project_textin, "/projets/", input$project_name)
    if (!dir.exists(res)) {
      dir.create(res, showWarnings = FALSE, recursive = TRUE)
    }
    # name of path are paste from path_project + project_name
    rl$path_data <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/data")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res 
    } else {
      NA
    } # path of entire tiled products
    rl$path_tiles <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/tiles")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of entire tiled products
    rl$path_pred <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/pred/sdm")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE, recursive = TRUE)
      }
      res
    } else {
      NA
    } # path of entire pred products
    rl$path_mosaic <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/mosaic")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of mosaic tiled products
    rl$path_translate <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/translate")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of translate tiled products
    rl$path_merged <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/merged")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of merged tiled products
    rl$path_tif <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/tif")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of tif products
    rl$path_warped <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/warped")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of warped tiled products
    rl$path_masked <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/masked")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of masked tiled products
    rl$path_out <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/projets/", input$project_name, "/out")
      if (!dir.exists(res)) {
        dir.create(res, showWarnings = FALSE)
      }
      res
    } else {
      NA
    } # path of output products
    rl$path_rgb <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/rgb")
      res2 <- paste0(input$path_project_textin, "/projets/", input$project_name, "/rgb/jpg")
      if (!dir.exists(res) | !dir.exists(res2)) {
        dir.create(res, showWarnings = FALSE, recursive = TRUE)
        dir.create(res2, showWarnings = FALSE, recursive = TRUE)
      }
      res
    } else {
      NA
    } # path of rgb products
    rl$path_indices <- if (rl$project_name != "") {
      res <- paste0(input$path_project_textin, "/indices")
      res2 <- paste0(input$path_project_textin, "/projets/", input$project_name, "/indices/jpg")
      if (!dir.exists(res) | !dir.exists(res2)) {
        dir.create(res, showWarnings = FALSE, recursive = TRUE)
        dir.create(res2, showWarnings = FALSE, recursive = TRUE)
      }
      res
    } else {
      NA
    } # path of spectral indices
    
    ## product
    rl$product <- input$product # theia to download THEIA product peps to download PEPS product
    rl$theiacollection <- if (rl$product == "theia") {
      input$theiacollection
    } else {
      NA
    } 
    rl$theiaplatformsentinel <- if (rl$product == "theia" & rl$theiacollection == "sentinel2") {
      input$theiaplatformsentinel
    } else {
      NA
    } # s2a, s2b
    
    # level
    rl$theiaplatformsentinellevel <- if (rl$product == "theia" & rl$theiacollection == "sentinel2") {
      input$theiaplatformsentinellevel
    } else {
      NA
    } # sentinel level
    
    
    rl$online <- as.logical(input$online) # TRUE if online mode, FALSE if offline mode
    rl$downloader <- input$downloader # downloader ("wget" or "aria2")
    rl$overwrite_product <- as.logical(input$overwrite_product) # TRUE to overwrite existing product, FALSE not to
    
    # spatio-temporal selection #
    rl$timewindow <- if (input$query_time == TRUE) { # range of dates
      input$timewindow
    } else {
      NA
    }
    rl$timeperiod <- if (input$query_time == TRUE) { # range of dates
      input$timeperiod # "full" or "seasonal"
    } else {
      "full"
    }
    
    # polygons extent
    rl$extent <- if (input$query_space == TRUE & !is.null(rv$extent)) {
      rv$extent %>%
        st_transform(4326) %>%
        geojson_json(pretty = TRUE)
    } else {
      NA
    }
    
    # polygons extent_pa
    rl$extent_pa <- if (input$query_space == TRUE & !is.null(rv$extent_pa)) {
      rv$extent_pa %>%
        st_transform(4326) %>%
        geojson_json(pretty = TRUE)
    } else {
      NA
    }
    
    # polygons extent_mask
    rl$extent_mask <- if (input$query_space == TRUE & !is.null(rv$extent_mask)) {
      rv$extent_mask %>%
        st_transform(4326) %>%
        geojson_json(pretty = TRUE)
    } else {
      NA
    }
    
    rl$s2tiles_selected <- if (input$query_space == TRUE & !is.null(input$tiles_checkbox)) {
      input$tiles_checkbox
    } else {
      NA
    } # selected tile IDs
    
    # product selection #
    rl$verified_indices <- input$verified_indices
    rl$list_indices_checked <- indices_rv$checked # index names
    rl$index_source <- input$index_source # reflectance band for computing indices ("BOA" or "TOA")
    rl$mask_type <- if (input$atm_mask == FALSE) {
      NA
    } else if (input$atm_mask_type == "custom") {
      paste0("scl_", paste(input$atm_mask_custom, collapse = "_"))
    } else {
      input$atm_mask_type
    } # atmospheric masking (accepted types as in s2_mask())
    rl$max_mask <- input$max_masked_perc
    rl$mask_smooth <- if (input$mask_apply_smooth) {
      input$mask_smooth
    } else {
      0
    }
    rl$clip_on_extent <- as.logical(input$clip_on_extent) # TRUE to clip (and warp) on the selected extent, FALSE to work at tiles/merged level
    rl$extent_as_mask <- as.logical(input$extent_as_mask) # TRUE to mask outside the polygons of extent, FALSE to use as boundig box
    rl$mask_buffer <- if (input$mask_apply_smooth) {
      input$mask_buffer
    } else {
      0
    }
    
    # rgb
    rl$rgb_out <- input$rgb_out
    
    # output format (GDAL format name)
    rl$outformat <- input$outformat
    rl$index_datatype <- input$index_datatype
    # output compression ("LZW", "DEFLATE" etc.)
    rl$compression <- ifelse(rl$outformat == "GTiff",
                             input$compression,
                             NA
    )
    # overwrite or skip existing files (logical)
    rl$overwrite <- as.logical(input$overwrite)
    rl$thumbnails <- if (rl$product == "theia") {
      as.logical(input$check_thumbnails)
    } else {
      NA
    } # logical (create thumbnails)
    
    # save apitheia.txt path if it was customly set
    if (!is.null(NULL) & !anyNA(NULL)) {
      rl$apitheia_path <- rv$apitheia_path
    }
    
    # information about package version
    rl$pkg_version <- packageVersion("shinycnes") %>% as.character()
    
    return(rl)
  }
}
    
## To be copied in the UI
# mod_object_to_return_ui("object_to_return_ui_1")
    
## To be copied in the server
# callModule(mod_object_to_return_server, "object_to_return_ui_1")
 
