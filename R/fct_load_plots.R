#' load_inventory_plots
#'
#' @name load_plots
#'
#' @param rv Reactive values
#' @param session Session
#'
#' @importFrom shiny htmlOutput modalButton modalDialog helpText fileInput
#' actionButton
#' @importFrom leaflet leafletOutput
#' @export
#' 
load_plots <- function(rv, session) {
  ns <- session$ns
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  modalDialog(
    title = i18n$t("Select vector gpkg file"),
    size = "m",
    helpText(em(
      p(i18n$t("Choose the vector gpkg file containing inventory plots location.")),
      p(
        i18n$t("To upload a geopackage, select the related file"),
        i18n$t("(at most the .gpkg must be present).")
      )
    )),
    fileInput(ns("path_placettes_sel"),
              i18n$t("Select"),
              multiple = FALSE
    ),
    easyClose = FALSE,
    modalButton(i18n$t("\u2000Cancel"), icon = icon("ban"))
  )
}