# Module UI
  
#' @title   mod_login_theia_ui and mod_login_theia_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_login_theia
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_login_theia_ui <- function(id){
  ns <- NS(id)
  tagList(#' @title theia_modal
    #'
    #' @param username Theia login
    #' @param password Theia password
    #' @export
    theia_modal <- function(username = NA, password = NA) {
      # read theia user/password
      if (anyNA(c(username, password))) {
        apitheia <- read_theia_login()
        username <- apitheia[1]
        password <- apitheia[2]
      }
      modalDialog(title = "Renseignez votre identifiant et votre mot de passe THEIA", size = "s", textInput("theia_username", "Nom d'utilisateur", username), passwordInput(
        "theia_password", "Mot de passe", 
        password
      ), a("Enregistrer un nouveau compte", href = "https://sso.theia-land.fr/theia/register/register.xhtml", target = "_blank"), "\u2000—\u2000", a("Mot de passe oublie ?",
                                                                                                                                                     href = "https://sso.theia-land.fr/theia/profile/recovery.xhtml;jsessionid=49E3F76B9E96191C4ADDD3EE5298E366", target = "_blank"
      ), checkboxInput("apitheia_default",
                       label = span(i18n$t("Store inside the package\u2000"), actionLink("help_apitheia", icon("question-circle"))), value = TRUE
      ), easyClose = FALSE, footer = tagList(div(
        style = "display:inline-block;vertical-align:top;",
        conditionalPanel(condition = "output.switch_save_apihub == 'custom'", shinySaveButton("apitheia_path_sel", i18n$t("Save as..."), "Specify path for apitheia text file",
                                                                                              filetype = list(plain = "txt"), class = "theia_savebutton"
        ))
      ), div(style = "display:inline-block;vertical-align:top;", conditionalPanel(
        condition = "output.switch_save_apitheia == 'default'",
        actionButton("save_apitheia", "\u2000Sauver", icon = icon("save"), class = "theia_savebutton")
      )), div(style = "display:inline-block;vertical-align:top;", modalButton("\u2000Annuler",
                                                                              icon = icon("ban")
      ))))
    }
  
   )
}

# Module Server
    
#' @rdname mod_login_theia
#' @export
#' @keywords internal
    
mod_login_theia_server <- function(input, output, session){
  ns <- session$ns
  
  #' @title read_theia_login
  #'
  #' @param apitheia_path Path api THEIA
  #' @export
  read_theia_login <- function(apitheia_path = NA) {
    # if apitheia_path is not specified, retrieve from the current theia_download installation
    if (is.na(apitheia_path)) {
      # import theia_download
      theia_download <- import_theia_download(convert = FALSE)
      apitheia_path <- file.path(theia_download$inst_path, "auth_theia.txt")
      # apitheia_path <- file.path(py_to_r(theia_download$inst_path),'auth_theia.txt')
    }
    # return user and password
    if (file.exists(apitheia_path)) {
      readLines(apitheia_path)[1] %>% strsplit(" ") %>% unlist()
    } else {
      # if apitheia does not exists, return default credentials
      c("email", "password")
    }
  }
  
  
  
  #' @title write_theia_login
  #'
  #' @param username Theia login
  #' @param password Theia password
  #' @param apitheia_path Path theia
  #' @export
  write_theia_login <- function(username, password, apitheia_path = NA) {
    # if apitheia_path is not specified, retrieve from the current theia_download installation
    if (is.na(apitheia_path)) {
      # import theia_download
      theia_download <- import_theia_download(convert = FALSE)
      apitheia_path <- file.path(theia_download$inst_path, "auth_theia.txt")
    }
    # write credentials
    writeLines(paste(username, password), apitheia_path) # permet d'ecrire des lignes de texte pour se connecter
    # replace username and password in config file
    command1 <- paste0("sed -i '/login_theia =/a \\login_theia = ", username, "' ", theia_download$inst_path, "/config_theia.cfg")
    try(system(command1, intern = TRUE))
    command2 <- paste0("sed -i '/password_theia =/a \\password_theia = ", password, "' ", theia_download$inst_path, "/config_theia.cfg")
    try(system(command2, intern = TRUE))
    command3 <- paste0("sed -i '4d;6d' ", theia_download$inst_path, "/config_theia.cfg")
    try(system(command3, intern = TRUE))
  }
  
}
    
## To be copied in the UI
# mod_login_theia_ui("login_theia_ui_1")
    
## To be copied in the server
# callModule(mod_login_theia_server, "login_theia_ui_1")
 
