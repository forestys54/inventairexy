#' @import shiny
#' @import shinythemes
#' @import shinyWidgets
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyFiles
#'
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    # ui = tags$body(class="skin-red sidebar-mini control-sidebar-open",

    dashboardPagePlus(
      skin = "blue",
      header = dashboardHeaderPlus(
        fixed = TRUE,
        title = tagList(
          span(class = "logo-lg", "Inventaire XY"),
          img(src = "www/ShinyDashboardPlus_FINAL.svg")
        ),
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "bars"
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("Selection des entrees", tabName = "project", icon = icon("gears")),
          menuItem("Carte des parametres", tabName = "pop", icon = icon("clone")),
          menuItem("Cumul par entites", tabName = "cumul", icon = icon("calculator"))


          # menuItem("Exploration", tabName = "exploration", icon = icon("dashboard")),
        )
      ),
      body = dashboardBody(
        tabItems(
          # First tab content
          tabItem(
            tabName = "project",
            mod_project_ui("project"),
            # mod_s2product_ui("s2product"),
            # mod_login_theia_ui("login_theia"),
            # mod_dendro_ui("dendro"),
            # mod_donneessup_ui("donneessup"),
            # mod_outputoptions_ui("outputoptions"),
            # mod_outputfiles_ui("outputfiles"),
            br(),
            br(),
            actionButton("launch", "Lancer les calculs !", icon = icon("tree"), style = "display:inline-block;horizontal-align:right;") ## Bouton pour lancer l'app, en bas et centre
          ),
          tabItem(
            tabName = "pop",
            mod_pop_ui("pop")
          ),
          tabItem(
            tabName = "cumul",
            mod_cumul_ui("cumul")
          )
          # second tab content
          # tabItem(
          #   tabName = "param",
          #   mod_retiming_ui("param")
          # )
        )
      ),
      rightsidebar = rightSidebar(
        background = "dark",
        rightSidebarTabContent(
          id = 1,
          icon = "map-marked-alt",
          active = TRUE,
          mod_param_ui("param")
        )
      ),
      footer = dashboardFooter(
        left_text = "By FORESTYS",
        right_text = "Nancy, 2020"
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function() {
  addResourcePath(
    "www", system.file("app/www", package = "inventoryxy")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
