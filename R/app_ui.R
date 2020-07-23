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
    # ui = tags$body(class="skin-blue sidebar-mini control-sidebar-open", 
    
    dashboardPagePlus(
      header = dashboardHeaderPlus(
        fixed = TRUE,
        title = tagList(
          span(class = "logo-lg", "Inventory XY"),
          img(src = "www/ShinyDashboardPlus_FINAL.svg")
        ),
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "bars"
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("Project", tabName = "project", icon = icon("gears"))
          # menuItem("Exploration", tabName = "exploration", icon = icon("dashboard")),
        )
      ),
      body = dashboardBody(
        tabItems(
          # First tab content
          tabItem(
            tabName = "project",
            mod_project_ui("project")
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
        left_text = "By Forestys",
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
