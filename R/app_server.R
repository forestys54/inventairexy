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
  callModule(mod_s2product_server, "project", rv = r)
  callModule(mod_dendro_server, "project", rv = r)
  callModule(mod_donneessup_server, "project", rv = r)
  callModule(mod_outputfiles_server, "project", rv = r)
  # callModule(mod_pop_server, "pop", rv = r)
  callModule(mod_cumul_server, "project", rv = r)
  callModule(mod_rgb_image_server, "project", rv = r)
  # callModule(mod_product_selection_server, "project", rv = r)
  callModule(mod_spectral_indice_server, "project", rv = r)
  callModule(mod_processing_options_server, "project", rv = r)
  # callModule(mod_temporal_map_server, "project", rv = r)
  callModule(mod_param_server, "project", rv = r)
  
  }

