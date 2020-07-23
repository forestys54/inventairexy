# Module UI
  
#' @title   mod_param_list_ui and mod_param_list_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_param_list
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_param_list_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_param_list
#' @export
#' @keywords internal
    
mod_param_list_server <- function(input, output, session){
  ns <- session$ns
  
  # function to import saved parameters
  import_param_list <- function(pl) {
    
    # Add a progress bar while importing
    withProgress(message = "Chargement des parametres", value = 0, {
      
      # set directories
      updateTextInput(session, "project_name", value = pl$project_name)
      updateTextInput(session, "path_project_textin", value = pl$path_project)
      updateTextInput(session, "path_model_textin", value = pl$path_model)
      updateSelectInput(session, "listimage01", choices = c("Choose a picture" = "", limage()))
      updateSelectInput(session, "listimage02", choices = c("Choose a picture" = "", limagergb()))
      updateSelectInput(session, "listimage03", choices = c("Choose a picture" = "", limageind()))
      updateRadioButtons(session, "check_thumbnails", selected = pl$thumbnails)
      setProgress(0.2)
      
      # processing steps
      # product
      updateRadioButtons(session, "product", selected = pl$product)
      if (pl$product == "theia") {
        updateRadioButtons(session, "theiacollection", selected = pl$theiacollection)
      } else {
        updateRadioButtons(session, "pepscollection", selected = pl$pepscollection)
      }
      # theiaplatform
      if (pl$theiacollection == "sentinel2") {
        updateRadioButtons(session, "theiaplatformsentinel", selected = pl$theiaplatformsentinel)
        updateRadioButtons(session, "theiaplatformsentinellevel", selected = pl$theiaplatformsentinellevel)
      } 
      # saving options
      updateRadioButtons(session, "online", selected = pl$online)
      updateRadioButtons(session, "downloader", selected = pl$downloader)
      updateRadioButtons(session, "overwrite_product", selected = pl$overwrite_product)
      setProgress(0.3)
      
      # spatio-temporal selection
      if (anyNA(pl$timewindow)) {
        updateRadioButtons(session, "query_time", selected = FALSE)
      } else {
        updateRadioButtons(session, "query_time", selected = TRUE)
        updateDateRangeInput(session, "timewindow", start = pl$timewindow[1], end = pl$timewindow[2])
        updateRadioButtons(session, "timeperiod", selected = pl$timeperiod)
      }
      if (anyNA(pl$extent) & pl$online == FALSE) {
        updateRadioButtons(session, "query_space", selected = FALSE)
      } else {
        updateRadioButtons(session, "query_space", selected = TRUE)
      }
      setProgress(0.4)
      
      # indices
      updateCheckboxInput(session, "verified_indices", value = pl$verified_indices)
      indices_rv$checked <- pl$list_indices_checked
      updateCheckboxGroupInput(session, "list_indices", selected = pl$list_indices)
      
      # rgb
      updateCheckboxGroupInput(session, "rgb_out", selected = pl$rgb_out)
      
      updateRadioButtons(session, "atm_mask",
                         selected = ifelse(is.na(pl$mask_type), FALSE, TRUE)
      )
      updateSliderInput(session, "max_masked_perc",
                        value = ifelse(is.na(pl$mask_type), 100, pl$max_mask)
      )
      updateNumericInput(session, "mask_apply_smooth",
                         value = if (all(c(pl$mask_smooth, pl$mask_buffer) == 0)) {
                           FALSE
                         } else {
                           TRUE
                         }
      )
      updateNumericInput(session, "mask_smooth", value = pl$mask_smooth)
      updateNumericInput(session, "mask_buffer", value = pl$mask_buffer)
      updateRadioButtons(session, "atm_mask_type",
                         selected = ifelse(is.na(pl$mask_type), "cloud_medium_proba", pl$mask_type)
      )
      updateRadioButtons(session, "atm_mask_custom",
                         selected = ifelse(grepl("^scl\\_", pl$mask_type), strsplit(pl$mask_type, "_")[[1]][-1], c(0, 8:9))
      )
      updateRadioButtons(session, "index_source", selected = pl$index_source)
      updateRadioButtons(session, "clip_on_extent", selected = pl$clip_on_extent)
      updateRadioButtons(session, "keep_tiles", selected = ifelse(is.na(pl$path_tiles), FALSE, TRUE))
      updateRadioButtons(session, "keep_merged", selected = ifelse(is.na(pl$path_merged), FALSE, TRUE))
      setProgress(0.6)
      
      # update apihub path
      rv$apitheia_path <- pl$apitheia_path
      
      updateRadioButtons(session, "outformat", selected = pl$outformat)
      updateRadioButtons(session, "index_datatype", selected = pl$index_datatype)
      updateRadioButtons(session, "compression", selected = ifelse(pl$outformat == "GTiff",
                                                                   pl$compression,
                                                                   character(0)
      ))
      updateRadioButtons(session, "overwrite", selected = pl$overwrite)
      
      setProgress(0.8)
      
      # update extent (at the end, not to interfer with other events
      # (the delay is required to update the map after the map is charged)
      shinyjs::delay(5E3, {
        update_extent(extent_source = "imported", custom_source = pl$extent)
        update_extent(extent_source = "importedpa", custom_source = pl$extent_pa)
        update_extent(extent_source = "importedmask", custom_source = pl$extent_mask)
        updateCheckboxGroupInput(session, "tiles_checkbox",
                                 selected = pl$s2tiles_selected
        )
      })
      setProgress(1)
    })
  }
}
    
## To be copied in the UI
# mod_param_list_ui("param_list_ui_1")
    
## To be copied in the server
# callModule(mod_param_list_server, "param_list_ui_1")
 
