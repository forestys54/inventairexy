#' @import shiny
#' @importFrom stringr str_extract
#' @importFrom tools file_path_sans_ext
#' @importFrom shiny.i18n Translator
#'
app_server <- function(input, output, session) {
  # List the first level callModules here

  # create reative values
  r <- reactiveValues()

  observe({
    # internationalize
    r$i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  })

  observe({
    # get plot directory
    r$nom <- "test de nom"
  })

  callModule(mod_project_server, "project", rv = r)
  # callModule(mod_s2product_server, "s2product", rv = r)
  # callModule(mod_dendro_server, "dendro", rv = r)
  # callModule(mod_donneessup_server, "donneessup", rv = r)
  # callModule(mod_outputoptions_server, "outputoptions", rv = r)
  # callModule(mod_outputfiles_server, "outputfiles", rv = r)
  # callModule(mod_pop_server, "pop", rv = r)
  # callModule(mod_cumul_server, "cumul", rv = r)
  # callModule(mod_login_theia_server, "login_theia", rv = r)
  # callModule(mod_param_server, "param", rv = r)
}
