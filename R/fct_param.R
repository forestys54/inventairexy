#' import_param_list
#'
#' @param rv Reactivevalue list
#' @param session Shiny session
#' @param module Module name
#'
#' @return A param list
#' @importFrom shiny updateTextInput setProgress withProgress NS
#' @importFrom shiny.i18n Translator
#' @export
#'
import_param_list <- function(module, rv, session) {
  ns <- NS(module)
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  # Add a progress bar while importing
  withProgress(message = i18n$t("Loading parameters"), value = 0, {
    print(rv$project_name)
    setProgress(0.8)
    # set directory and name of project
    shiny::updateTextInput(session = session, ns("project_name"), value = rv$project_name)
    shiny::updateTextInput(session = session, ns("path_project_textin"), value = rv$path_project)
    setProgress(1)
  })
}


#' create_return_list
#'
#' @param rv Reactive value
#'
#' @return List
#' @export
#'
create_return_list <- function(rv) {
  rl <- list()
  # processing steps #
  rl$project_name <- rv$project_name
  # set directories #
  rl$path_project <- rv$path_project
  return(rl)
}

#' check_param
#'
#' @param param_list Param list
#' @param rv Reactive value
#'
#' @return Check param
#' @importFrom shiny.i18n Translator
#' @export
#'
check_param <- function(param_list, rv) {
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  error_list <- check_param_list(param_list, type = "string", correct = FALSE)
  if (!is.null(error_list)) {
    # if errors occur:
    # build modal dialog
    check_param_modal <- modalDialog(
      title = i18n$t("Parameter errors"),
      size = "m",
      if (length(error_list) == 1) {
        tagList(
          p(
            i18n$t("A parameter has not been correctly set:"),
            br(), error_list
          ),
          p(i18n$t("Please edit it using the GUI before continuing."))
        )
      } else {
        tagList(
          p(HTML(
            i18n$t("Some parameters have not been correctly set:"),
            "<ul><li>",
            paste(error_list, collapse = "</li><li>"),
            "</li></ul>"
          )),
          p(i18n$t("Please edit them using the GUI before continuing."))
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

#' @title Check a parameter list
#' @description Check that the parameter list (or JSON parameter file)
#'  is in the correct format, and then speficied values are coherent with
#'  parameters.
#' @param pm List of parameters or path of a JSON parameter file.
#' @param type Type of the output (see [print_message] for details).
#' @param correct Logical: if TRUE (default), the function corrects
#'  some incoherences (e.g. timewindow of length 1 is transformed in length 2)
#'  and returns the corrected list as output; if false, only checking is
#'  performed, and the output is NULL if no errors occur.
#' @return In case of errors, depending on `type` argument, output can be
#'  a vector of errors (if `type = 'string'`),
#'  the first error occurred (if `type = 'error'`)
#'  or a set of warnings (if `type = 'warning'`).
#'  If no errors occur, output is the corrected parameter list if
#'  `correct = TRUE` or NULL otherwise.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom shiny showModal
#' @importFrom methods is
#' @importFrom stringr str_pad
#' @importFrom shiny.i18n Translator
#' @author Pascal Obstetar (2020) \email{pascal.obstetar@@onf.fr}
#' @note License: GPL 3.0
#' @export
check_param_list <- function(pm, type = "string", correct = TRUE) {
  i18n <- Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")

  # to avoid NOTE on check
  . <- NULL

  # check the format of pm object
  if (is(pm, "character")) {
    if (file.exists(pm)) {
      # load json parameter file
      pm <- jsonlite::fromJSON(pm)
    } else {
      shiny::showModal(modalDialog(
        title = i18n$t("Unable to download"),
        paste0(i18n$t("The file "), pm, i18n$t(" does not exist.")),
        easyClose = TRUE,
        footer = NULL
      ))
      correct <- FALSE
    }
  } else if (!is(pm, "list")) {
    shiny::showModal(modalDialog(
      title = i18n$t("Unable to download"),
      paste0("\"", deparse(substitute(pm)), "\"", i18n$t("must be a list or a path of a JSON parameter file.")),
      easyClose = TRUE,
      footer = NULL
    ))
    correct <- FALSE
  }

  # TODO check the names of the content of the list

  # check is empty project name
  if (pm$project_name == "") {
    shiny::showModal(modalDialog(
      title = i18n$t("Unable to download"),
      i18n$t("The name of project is empty!"),
      easyClose = TRUE,
      footer = NULL
    ))
    correct <- FALSE
  }

  if (correct == TRUE) {
    return(pm)
  } else {
    return(invisible(NULL))
  }
}
