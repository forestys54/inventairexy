#' # Module UI
#' 
#' #' @title   mod_pop_ui and mod_pop_server
#' #' @description  A shiny Module.
#' #'
#' #' @param id shiny id
#' #' @param input internal
#' #' @param output internal
#' #' @param session internal
#' #'
#' #' @rdname mod_pop
#' #'
#' #' @keywords internal
#' #' @export
#' #' @importFrom shiny NS tagList
#' #'
#' mod_pop_ui <- function(id){
#'   ns <- NS(id)
#' 
#'   i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
#'   i18n$set_translation_language("fr")
#' 
#'   tagList(
#'     br(),
#'     br(),
#'     br(),
#'     column(width=6,
#'            fluidRow(
#'              box(
#'                title = (i18n$t("Species card")), ## Le but c'est de pouvoir le faire pour n'importe quel parametre,
#'                ## la c'est juste un exemple mais il faut l'automatiser pour que ca soit dispo pour tous les parametres necessaires
#'                status = "primary",
#'                solidHeader = TRUE,
#'                collapsible = TRUE,
#'                width = 12,
#'                selectInput("listimage01", i18n$t ("Downloaded tiles :"), c("No tile" = "")),
#'                uiOutput(ns("checkbox_list_rgb")),
#'                div(
#'                  style = "display:inline-block;horizontal-align:center;",
#'                  actionButton(
#'                    ns("new_rgb"),
#'                    label = i18n$t("\u2000Define custom RGB image"),
#'                    icon = icon("plus"),
#'                    imageOutput("image01", height = "400px", width = "500px")
#'                  )
#'                ), # end div
#'                actionButton("export", i18n$t("Export the map"), style = "display:inline-block;horizontal-align:right;")
#'              )
#'            )
#'     ), # end column
#'     column(width=6,
#'            fluidRow(
#'              box(
#'                title = (i18n$t("Uncertainties about species")), ## Le but c'est de pouvoir le faire pour n'importe quel parametre,
#'                ## la c'est juste un exemple mais il faut l'automatiser pour que ca soit dispo pour tous les parametres necessaires
#'                status = "primary",
#'                solidHeader = TRUE,
#'                collapsible = TRUE,
#'                width = 12,
#'                selectInput("listimage01", i18n$t("Tiles downloaded :"), c("No tile" = "")),
#'                div(
#'                  style = "display:inline-block;horizontal-align:center;",
#'                  imageOutput("image01", height = "400px", width = "500px")
#'                ), # end div
#'                actionButton("export", i18n$t("Export the map"), style = "display:inline-block;horizontal-align:right;")
#'              )
#'            )
#'     ), # end column
#'     br(),
#'     br(),
#'     column(width=6,
#'            fluidRow(
#'              box(
#'                title = i18n$t(("Map of G/ha")),
#'                status = "primary",
#'                solidHeader = TRUE,
#'                collapsible = TRUE,
#'                width = 12,
#'                selectInput("listimage02", i18n$t("Tiles downloaded :"), c("No tile" = "")),
#'                div(
#'                  style = "display:inline-block;horizontal-align:center;",
#'                  imageOutput("image02", height = "400px", width = "500px")
#'                ), # end div
#'                actionButton("export", i18n$t("Export the map"), style = "display:inline-block;horizontal-align:right;")
#'              )
#'            )
#'     ), # end column
#'     column(width=6,
#'            fluidRow(
#'              box(
#'                title = (i18n$t("Uncertainties about G/ha")),
#'                status = "primary",
#'                solidHeader = TRUE,
#'                collapsible = TRUE,
#'                width = 12,
#'                selectInput("listimage02", i18n$t("Tiles downloaded :"), c("No tile" = "")),
#'                div(
#'                  style = "display:inline-block;horizontal-align:center;",
#'                  imageOutput("image02", height = "400px", width = "500px")
#'                ), # end div
#'                actionButton("export", i18n$t("Export the map", style = "display:inline-block;horizontal-align:right;")
#'                )
#'              )
#'            )), # end column
#'     br(),
#'     br(),
#'     column(width=6,
#'            fluidRow(
#'              box(
#'                title = (i18n$t("Map of V/ha")),
#'                status = "primary",
#'                solidHeader = TRUE,
#'                collapsible = TRUE,
#'                width = 12,
#'                selectInput("listimage03", i18n$t("Tiles downloaded :"), c("No tile" = "")),
#'                div(
#'                  style = "display:inline-block;horizontal-align:center;",
#'                  imageOutput("image03", height = "400px", width = "500px")
#'                ), # end div
#'                actionButton("export", i18n$t("Export the map"), style = "display:inline-block;horizontal-align:right;")
#'              )
#'            )
#'     ), # end column
#'     column(width=6,
#'            fluidRow(
#'              box(
#'                title = (i18n$t("Uncertainties on V/ha")),
#'                status = "primary",
#'                solidHeader = TRUE,
#'                collapsible = TRUE,
#'                width = 12,
#'                selectInput("listimage03", i18n$t("Tiles downloaded :"), c("No tile" = "")),
#'                div(
#'                  style = "display:inline-block;horizontal-align:center;",
#'                  imageOutput("image03", height = "400px", width = "500px")
#'                ), # end div
#'                actionButton("export", i18n$t("Export the map"), style = "display:inline-block;horizontal-align:right;")
#'              )
#'            )
#'     ), # end column
#'     br(),
#'     br(),
#'     column(width=6,
#'            fluidRow(
#'              box(
#'                title = (i18n$t("Map of stem density")),
#'                status = "primary",
#'                solidHeader = TRUE,
#'                collapsible = TRUE,
#'                width = 12,
#'                selectInput("listimage04", i18n$t("Tiles downloaded :"), c("No tile" = "")),
#'                div(
#'                  style = "display:inline-block;horizontal-align:center;",
#'                  imageOutput("image04", height = "400px", width = "500px")
#'                ), # end div
#'                actionButton("export", i18n$t("Export the map"), style = "display:inline-block;horizontal-align:right;")
#'              )
#'            )
#'     ), # end column
#'     column(width=6,
#'            fluidRow(
#'              box(
#'                title = (i18n$t("Uncertainties about stem density")),
#'                status = "primary",
#'                solidHeader = TRUE,
#'                collapsible = TRUE,
#'                width = 12,
#'                selectInput("listimage04", i18n$t("Tiles downloaded :"), c("No tile" = "")),
#'                div(
#'                  style = "display:inline-block;horizontal-align:center;",
#'                  imageOutput("image04", height = "400px", width = "500px")
#'                ), # end div
#'                actionButton("export", i18n$t("Export the map"), style = "display:inline-block;horizontal-align:right;")
#'              )
#'            )
#'     )) # end taglist
#' }
#' 
#' 
#' # Module Server
#' 
#' #' @rdname mod_pop
#' #' @export
#' #' @keywords internal
#' 
#' mod_pop_server <- function(input, output, session, rv){
#'   ns <- session$ns
#' }
#' 
#' ## To be copied in the UI
#' # mod_pop_ui("pop_ui_1")
#' 
#' ## To be copied in the server
#' # callModule(mod_pop_server, "pop_ui_1")
#' 
