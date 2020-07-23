#' load_dendrometric_datas
#'
#' @name load_dendro
#'
#' @param rv Reactive values
#' @param session Session
#'
#' @importFrom shiny htmlOutput modalButton modalDialog helpText fileInput
#' actionButton
#' @importFrom leaflet leafletOutput
#' @export
#' 
load_dendro <- function(rv, session) {
  ns <- session$ns
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  modalDialog(
    title = i18n$t("Select a file containing dendrometric datas"),
    size = "m",
    helpText(em(
      p(i18n$t("Choose the file containing the dendrometric datas to be used (.xlsx or .csv).")),
    )
    ),
    fileInput(ns("path_dendro_sel"),
              i18n$t("Select"),
              multiple = FALSE
    ),
    easyClose = FALSE,
  )
}