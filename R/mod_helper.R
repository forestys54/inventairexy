# Module UI
  
#' @title   mod_helper_ui and mod_helper_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_helper
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_helper_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_helper
#' @export
#' @keywords internal
    
mod_helper_server <- function(input, output, session){
  ns <- session$ns
  
  ####### message help #############
  observeEvent(input$help_time_period, {
    showModal(modalDialog(
      title = "Fenetre temporelle",
      p(HTML(
        "<strong>Full</strong> :",
        "La fenetre temporelle specifiee est <strong>entierement</strong> traitee",
        "(e.g., ce qui signifie que pour une periode allant du 2016-05-01 au 2018-09-30,",
        "tous les produits de cette fenetre qui correspondent aux autres parametres seront renvoyes)."
      )),
      p(HTML(
        "<strong>Seasonal</strong> :",
        "la fenetre temporelle specifiee est traitee depuis la premiere annee",
        "jusqu'a la derniere annee, dans le fenetre temporelle saisonniere depuis le premier",
        "jour calendaire jusqu'au second jour calendaire",
        "(e.g., specifier un intervalle du 2016-05-01 au 2018-09-30 va renvoyer",
        "tous les produits du 2016-05-01 au 2016-09-30, du 2017-05-01 au",
        "2017-09-30 et du 2018-05-01 au 2018-09-30,",
        "qui correspondent aux autres parametres)."
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$help_clip_on_extent, {
    showModal(modalDialog(
      title = "Utiliser l'etendue selectionnee pour les sorties ?",
      p(HTML(
        "<strong>Oui</strong> :",
        "l'etendue selectionnee dans \"Selection spatio-temporelle\"",
        "est utilisee comme etendue pour les produits de sortie.",
        "L'utilisateur peut choisir d'autres parametres geometriques dans la box",
        "\"Sorties geometriques\"."
      )),
      p(HTML(
        "<strong>Non</strong> :",
        "l'etendue selectionnee dans \"Selection spatio-temporelle\"",
        "est utilisee pour selectionner des tuiles qui se chevauchent;",
        "les produits de sortie conservent l'etendue complete et la geometrie",
        "des tuiles d'entree Sentinel-2."
      )),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}
    
## To be copied in the UI
# mod_helper_ui("helper_ui_1")
    
## To be copied in the server
# callModule(mod_helper_server, "helper_ui_1")
 
