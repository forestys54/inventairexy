# Module UI

#' @title   mod_donneessup_ui and mod_donneessup_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_donneessup
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' 
mod_donneessup_ui <- function(id){
  ns <- NS(id)
  
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    column(width=6,
           fluidRow(
             box(
               title = (i18n$t("Additional datas")),
               status = "primary",
               solidHeader = TRUE,
               collapsible = TRUE,
               width = 12,
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong(i18n$t("Load forest plot : \u00a0"))),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_parcellaire", ("Selec"), (i18n$t("Select file containing forest plot")))
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong(i18n$t("Define forest mesh : \u00a0"))),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   radioButtons(
                     ns("mesh"), NULL,
                     choiceNames = list(
                       span("Mesh of the raster (10m or 20m)"),
                       span("Mesh thiner (raster's mesh divided)"),
                       span("Mesh matching forest plot's size")),
                     choiceValues = list("raster", "thiner", "plot"),
                     selected = "raster",
                     inline = FALSE
                   )
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong(i18n$t("Load stand classes : \u00a0"))),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_classes", ("Selec"), (i18n$t("Select file containing stand classes")))
                 )
               ),
               column(
                 width = 12,
                 div(
                   style = "display:inline-block;vertical-align:top;",
                   strong(i18n$t("Load raster : \u00a0"))),
                 br(),
                 br(),
                 div(
                   style = "display:inline-block;vertical-align:top;width:50pt;",
                   shinyDirButton("path_mnt", ("Selec"), (i18n$t("Select other raster files (DEM ...)")))
                 )
               ) # end column
             )
           )
    )
  )
}

# Module Server

#' @rdname mod_donneessup
#' @export
#' @keywords internal

mod_donneessup_server <- function(input, output, session, rv){
  ns <- session$ns
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  observe({
    rv$mesh <- input$mesh
  })
  
  # Set directory
  observe({
    shinyDirChoose(input, "path_parcellaire", roots = rv$volumes)
  })
  
  observeEvent(input$path_parcellaire_sel, {
    uploaded_parcellaire <- gsub("^.+\\.(.+)$", "\\1", input$path_parcellaire_sel$name)
    # checks
    if (uploaded_parcellaire %not_in% c("shp", "shx", "dbf", "prj")) {
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
      rv$parcellaire_path <- ""
    } else if (anyNA(match(c("shp", "shx", "dbf", "prj"), uploaded_parcellaire))) {
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
      rv$parcellaire_path <- ""
    } else {
      # if a shapefile was chosen and all the files are present,
      # rename the uploaded files in order to have the same filename and use them
      path_parcellaire_sel_new_datapath <- file.path(
        dirname(input$path_parcellaire_sel$datapath), input$path_parcellaire_sel$name
      )
      for (i in seq_len(nrow(input$path_parcellaire_sel))) {
        file.rename(input$path_parcellaire_sel$datapath[i], path_parcellaire_sel_new_datapath[i])
      }
      rv$parcellaire_path <- path_parcellaire_sel_new_datapath[
        input$path_parcellaire_sel$type == "application/x-esri-shape"
        ]
    }
  })
  
  # Open modal dialog to load the forest inventory plot
  observeEvent(input$path_parcellaire, {
    rv$parcellaire_path <- ""
    showModal(load_parcellaire(rv, session))
  })
  
  
  
  # Set directory
  observe({
    shinyDirChoose(input, "path_classes", roots = rv$volumes)
  })
  
  observeEvent(input$path_classes_sel, {
    uploaded_classes <- gsub(
      "^.+\\.(.+)$", "\\1",
      input$path_classes_sel$name
    )
    # checks if it is a csv or xlsx file
    if (uploaded_classes %not_in% c("xlsx", "csv")) {
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
      rv$classes_path <- ""
    } else if (length(uploaded_classes) == 1 && uploaded_classes %in% c("xlsx", "csv")) {
      # if a single file was chosen and it is a xlsx or csv file, use it
      rv$classes_path <- input$path_classes_sel$datapath
    }
  })
  
  # Open modal dialog to load the stand classes
  observeEvent(input$path_classes, {
    rv$classes_path <- ""
    showModal(load_classes(rv, session))
  })
  
  
  
  # Set directory
  observe({
    shinyDirChoose(input, "path_mnt", roots = rv$volumes)
  })
  
  observeEvent(input$path_mnt_sel, {
    uploaded_classes <- gsub(
      "^.+\\.(.+)$", "\\1",
      input$path_mnt_sel$name
    )
    # checks if it is a csv or xlsx file
    if (uploaded_mnt %not_in% c("tiff", "bmp", "tif")) {
      # if a not csv or xlsx file was chosen, give an alert and do not use the file
      sendSweetAlert(
        session,
        title = i18n$t("Invalid file"),
        text = paste(
          i18n$t("Please select a tiff or bmp file")
        ),
        type = "error",
        btn_labels = "Ok"
      )
      rv$mnt_path <- ""
    } else if (uploaded_mnt %in% c("tiff", "bmp", "tif")) {
      # if a single file was chosen and it is a xlsx or csv file, use it
      rv$mnt_path <- input$path_mnt_sel$datapath
    }
  })
  
  # Open modal dialog to load the DEM
  observeEvent(input$path_mnt, {
    rv$mnt_path <- ""
    showModal(load_mnt(rv, session))
  })
  
}

## To be copied in the UI
# mod_donneessup_ui("donneessup_ui_1")

## To be copied in the server
# callModule(mod_donneessup_server, "donneessup_ui_1")

