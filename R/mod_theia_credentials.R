# Module UI
  
#' @title   mod_theia_credentials_ui and mod_theia_credentials_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_theia_credentials
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_theia_credentials_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_theia_credentials
#' @export
#' @keywords internal
    
mod_theia_credentials_server <- function(input, output, session){
  ns <- session$ns
  
  ############### Edit theia credentials login ###############
  observeEvent(input$theia, {
    # open the modalDialog
    showModal(theia_modal(
      username = if (!is.null(input$theia_username)) {
        input$theia_username
      } else {
        NA
      },
      password = if (!is.null(input$theia_password)) {
        input$theia_password
      } else {
        NA
      }
    ))
    # dummy variable to define which save button has to be used
    output$switch_save_apitheia <- renderText({
      if (is.null(input$apitheia_default)) {
        ""
      } else if (input$apitheia_default) {
        "default"
      } else {
        "custom"
      }
    })
    outputOptions(output, "switch_save_apitheia", suspendWhenHidden = FALSE)
    # initialise the shinyFiles Save as button
    observe({
      apitheia_path_prev <- rv$apitheia_path
      shinyFileSave(input, "apitheia_path_sel", roots = volumes, session = session)
      apitheia_path_raw <- parseSavePath(volumes, input$apitheia_path_sel)
      rv$apitheia_path <- if (nrow(apitheia_path_raw) > 0) {
        as.character(apitheia_path_raw$datapath)
      } else {
        NA
      }
      if (!is.na(rv$apitheia_path)) {
        if (!rv$apitheia_path %in% apitheia_path_prev) {
          # if a change in the path is detected (= the button has been used),
          # close the modalDialog
          # FIXME if a user re-open the modalDialog and does not change
          # user nor password, the "Save as" button will not close the dialog
          shinyjs::click("save_apitheia")
        }
      }
    })
  })
  # save user/password
  observeEvent(input$save_apitheia, {
    write_theia_login(
      input$theia_username, input$theia_password,
      apitheia_path = if (!is.na(rv$apitheia_path)) {
        as.character(rv$apitheia_path)
      } else {
        NA
      }
    )
    removeModal()
  })
}
    
## To be copied in the UI
# mod_theia_credentials_ui("theia_credentials_ui_1")
    
## To be copied in the server
# callModule(mod_theia_credentials_server, "theia_credentials_ui_1")
 
