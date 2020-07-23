# Module UI

#' @title   mod_dendro_ui and mod_dendro_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dendro
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_dendro_ui <- function(id){
  ns <- NS(id)
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  
  tagList(
    column(width = 6,
           fluidRow(
             box(
               title = (i18n$t("Dendrometric datas")),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong(i18n$t("Load the file containing inventory plots : \u00a0"))),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton(ns("path_placettes"), ("Selec"), (i18n$t("Select the file containing plots location")))
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong(i18n$t("Load the dendrometric datas : \u00a0"))),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton(ns("path_dendro"), ("Selec"), (i18n$t("Select the file containing the dendrometric datas")))
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong(i18n$t("Load the file containing trees location : \u00a0"))),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton(ns("path_arbres"), ("Selec"), (i18n$t("Select the file containing trees location")))
                 )
               )
             )
           )
    )
  )
}

# Module Server

#' @rdname mod_dendro
#' @export
#' @keywords internal
#' @importFrom sf st_geometry_type st_as_sf
#' @importFrom shinyjs enable disable
#' @importFrom leaflet removeShape leafletProxy removeLayersControl

mod_dendro_server <- function(input, output, session, rv){
  ns <- session$ns
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  
  # Set directory
  observe({
    shinyDirChoose(input, "path_placettes", roots = rv$volumes)
  })
  
  observeEvent(input$path_placettes_sel, {
    uploaded_placettes <- gsub(
      "^.+\\.(.+)$", "\\1",
      input$path_placettes_sel$name
    )
    # checks if it is a gpkg file
    if (uploaded_placettes %not_in% c("gpkg")) {
      # if a not gpkg file was chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = i18n$t("Invalid file"),
        text = paste(
          i18n$t("Please select a gpkg file")
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$placettes_path <- ""
    } else if (length(uploaded_placettes) == 1 && uploaded_placettes %in% c("gpkg")) {
      # if a single file was chosen and it is a gpkg file, use it
      rv$placettes_path <- input$path_placettes_sel$datapath
    }
  })
  
  # Open modal dialog to load the dendrometric datas
  observeEvent(input$path_placettes, {
    rv$placettes_path <- ""
    showModal(load_plots(rv, session))
  })

    # rv$placettes_path <- st_transform(rv$placettes_path, 2154)
   
  # Set directory
  observe({
    shinyDirChoose(input, "path_dendro", roots = rv$volumes)
  })
  
  observeEvent(input$path_dendro_sel, {
    uploaded_dendro <- gsub(
      "^.+\\.(.+)$", "\\1",
      input$path_dendro_sel$name
    )
    # checks if it is a csv or xlsx file
    if (uploaded_dendro %not_in% c("xlsx", "csv")) {
      # if a not csv or xlsx file was chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = i18n$t("Invalid file"),
        text = paste(
          i18n$t("Please select a csv or xlsx file")
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$dendro_path <- ""
    } else if (length(uploaded_dendro) == 1 && uploaded_dendro %in% c("xlsx", "csv")) {
      # if a single file was chosen and it is a xlsx or csv file, use it
      rv$dendro_path <- input$path_dendro_sel$datapath
    }
  })
  
  # Open modal dialog to load the dendrometric datas
  observeEvent(input$path_dendro, {
    rv$dendro_path <- ""
    showModal(load_dendro(rv, session))
  })
  
  
  observe({
    shinyDirChoose(input, "path_arbres", roots = rv$volumes)
  })
 
  observeEvent(input$path_arbres_sel, {
    uploaded_arbres <- gsub("^.+\\.(.+)$", "\\1", input$path_arbres_sel$name)
    # checks
    if (uploaded_arbres %not_in% c("shp", "shx", "dbf", "prj")) {
      # if a not shapefile was chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = i18n$t("Invalid file"),
        text = paste(
          i18n$t("Please select a shapefile")
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$arbres_path <- ""
    } else if (anyNA(match(c("shp", "shx", "dbf", "prj"), uploaded_arbres))) {
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
      rv$arbres_path <- ""
    } else {
      # if a shapefile was chosen and all the files are present,
      # rename the uploaded files in order to have the same filename and use them
      path_arbres_sel_new_datapath <- file.path(
        dirname(input$path_arbres_sel$datapath), input$path_arbres_sel$name
      )
      for (i in seq_len(nrow(input$path_arbres_sel))) {
        file.rename(input$path_arbres_sel$datapath[i], path_arbres_sel_new_datapath[i])
      }
      rv$arbres_path <- path_arbres_sel_new_datapath[
        input$path_arbres_sel$type == "application/x-esri-shape"
        ]
    }
  })
  
  # Open modal dialog to load the forest inventory plot
  observeEvent(input$path_arbres, {
    rv$arbres_path <- ""
    showModal(load_arbres(rv, session))
  })
   
}

## To be copied in the UI
# mod_dendro_ui("dendro_ui_1")

## To be copied in the server
# callModule(mod_dendro_server, "dendro_ui_1")

