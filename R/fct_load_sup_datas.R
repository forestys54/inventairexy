#' load_supplementary_datas
#'
#' @name load_sup_datas
#'
#' @param rv Reactive values
#' @param session Session
#'
#' @importFrom shiny htmlOutput modalButton modalDialog helpText fileInput
#' actionButton
#' @importFrom leaflet leafletOutput
#' @export
#' 
load_parcellaire <- function(rv, session) {
  ns <- session$ns
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  modalDialog(
    title = i18n$t("Select a file containing forest plot"),
    size = "m",
    helpText(em(
      p(i18n$t("Choose the files containing the forest plot to be used (select all files).")),
    )
    ),
    fileInput(ns("path_parcellaire_sel"),
              i18n$t("Select"),
              multiple = TRUE
    ),
    easyClose = FALSE,
  )
}


load_classes <- function(rv, session) {
  ns <- session$ns
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  modalDialog(
    title = i18n$t("Select a file containing forest stand classes"),
    size = "m",
    helpText(em(
      p(i18n$t("Choose the file containing the stand classes to be used (.xlsx or .csv).")),
    )
    ),
    fileInput(ns("path_classes_sel"),
              i18n$t("Select"),
              multiple = FALSE
    ),
    easyClose = FALSE,
  )
}


load_mnt <- function(rv, session) {
  ns <- session$ns
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  modalDialog(
    title = i18n$t("Select files containing DEM (or additional raster files)"),
    size = "m",
    helpText(em(
      p(i18n$t("Choose the files containing the DEM to be used (or any other raster file).")),
    )
    ),
    fileInput(ns("path_mnt_sel"),
              i18n$t("Select"),
              multiple = TRUE
    ),
    easyClose = FALSE,
  )
}

load_arbres <- function(rv, session) {
  ns <- session$ns
  
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  modalDialog(
    title = i18n$t("Select a file containing trees map"),
    size = "m",
    helpText(em(
      p(i18n$t("Choose the files containing the trees map to be used (select all files).")),
    )
    ),
    fileInput(ns("path_arbres_sel"),
              i18n$t("Select"),
              multiple = TRUE
    ),
    easyClose = FALSE,
  )
}