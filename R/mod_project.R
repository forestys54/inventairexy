# Module UI
  
#' @title   mod_project_ui and mod_project_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_project
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @importFrom shiny.i18n Translator
#' 
mod_project_ui <- function(id){
  ns <- NS(id)
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "tab_project",
    h3(i18n$t("Project options")),
    fluidRow(
      box(
        title = i18n$t("Project"), 
        # title = textOutput(ns("title_project")), 
        status = "primary", 
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        column(
          width = 2,
          div(
            style = "display:inline-block;vertical-align:top;",
            strong(i18n$t("Name of project: \u00a0"))
          ),
          div(
            style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
            textInput(ns("project_name"), NULL, "")
          )
        ),
        column(
          width = 8,
          div(
            style = "display:inline-block;vertical-align:top;",
            strong(i18n$t("Directory for project: \u00a0"))
          ),
          div(
            style = "display:inline-block;vertical-align:top;",
            htmlOutput(ns("path_project_errorness"))
          ),
          div(
            div(
              style = "display:inline-block;vertical-align:top;width:50pt;",
              shinyDirButton(ns("path_project_sel"), "Select", "Specify directory for project")
            ),
            div(
              style = "display:inline-block;vertical-align:top;width:calc(90% - 50pt - 3px);",
              textInput(ns("path_project_textin"), NULL, "")
            )
          )
        ),
        column(
          width = 2,
          div(
            style = "display:inline-block;vertical-align:top;",
            strong(i18n$t("Choice a plot: \u00a0"))
          ),
          div(
            style = "display:inline-block;vertical-align:top;width:calc(100% - 50pt - 3px);",
            selectInput(ns("plotin"), NULL, c("Choisir" = ""))
          )
        ) # end column
      ) # end box
    ) # end fluidrow
  ) # end taglist
}
    
# Module Server
    
#' @rdname mod_project
#' @export
#' @keywords internal
#' @importFrom shiny.i18n Translator
#' @importFrom shiny updateTextInput
    
mod_project_server <- function(input, output, session, rv){
  ns <- session$ns
  
  volumes <- c("Home" = path.expand("~"), shinyFiles::getVolumes()())
  
  observe({
    shinyDirChoose(input, "path_project_sel", roots = volumes)
  })
  
  observe({
    updateSelectInput(session, "plotin", choices = c("Choisir" = "", rv$nom))
  })
  
  observe({
    project_name <- input$project_name
    rv$project_name <- project_name
    updateTextInput(session, "project_name", value = project_name)
  })

  observe({
    path_project_string <- parseDirPath(volumes, input$path_project_sel)
    rv$path_project <- path_project_string
    updateTextInput(session, "path_project_textin", value = path_project_string)
  })

  observe({
    output$path_project_errorness <- path_check(input$path_project_textin)
  })
  
}
    
## To be copied in the UI
# mod_project_ui("project_ui_1")
    
## To be copied in the server
# callModule(mod_project_server, "project_ui_1")
 
