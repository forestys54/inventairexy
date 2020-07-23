# Module UI
  
#' @title   mod_all_correctly_set_ui and mod_all_correctly_set_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_all_correctly_set
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_all_correctly_set_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_all_correctly_set
#' @export
#' @keywords internal
    
mod_all_correctly_set_server <- function(input, output, session){
  ns <- session$ns
  
  # functions to check that all is correctly set TODO
  # return TRUE if check passes, FALSE if errors occur
  check_param <- function(param_list) {
    error_list <- check_param_list(param_list, type = "string", correct = FALSE)
    if (!is.null(error_list)) {
      # if errors occur:
      # build modal dialog
      check_param_modal <- modalDialog(
        title = "Erreurs de parametrage",
        size = "m",
        if (length(error_list) == 1) {
          tagList(
            p(
              ("Un parametre n'a pas ete mis en place correctement :"),
              br(), error_list
            ),
            p("Veuillez l'editer en utilisant l'interface utilisateur avant de continuer.")
          )
        } else {
          tagList(
            p(HTML(
              "Certains parametres n'ont pas ete mis en place correctement :",
              "<ul><li>",
              paste(error_list, collapse = "</li><li>"),
              "</li></ul>"
            )),
            p("Veuillez les editer en utilisant l'interface utilisateur avant de continuer.")
          )
        },
        easyClose = TRUE,
        footer = NULL
      )
      # show modal dialog
      showModal(check_param_modal)
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  
}
    
## To be copied in the UI
# mod_all_correctly_set_ui("all_correctly_set_ui_1")
    
## To be copied in the server
# callModule(mod_all_correctly_set_server, "all_correctly_set_ui_1")
 
