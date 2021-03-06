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
          menuItem("Selection des indices spectraux", tabName = "project", icon = icon("")),
          menuItem("Creation de carte RGB", tabName = "project", icon = icon("tree")),
          menuItem("Carte des parametres", tabName = "project", icon = icon("clone")),
          menuItem("Cumul par entites", tabName = "project", icon = icon("calculator"))
          
          
          # menuItem("Exploration", tabName = "exploration", icon = icon("dashboard")),
        )
      ),
      body = dashboardBody(
        tabItems(
          # First tab content
          tabItem(
            tabName = "project",
            mod_project_ui("project"),
            mod_s2product_ui("s2product"),
            mod_product_selection_ui("product_selection"),
            mod_dendro_ui("dendro"),
            mod_donneessup_ui("donneessup"),
            # mod_processing_options_ui("processing_options"),
            # mod_outputfiles_ui("outputfiles"),
            mod_temporal_map_ui("temporal_map"),
            br(),
            br(),
            actionButton("launch", "Lancer les calculs !", icon = icon("tree"), style = "display:inline-block;horizontal-align:right;") ## Bouton pour lancer l'app, en bas et centre
          ),
          tabItem(
            tabName = "indices",
            mod_spectral_indice_ui("spectral_indice")
          ),
          tabItem(
            tabName = "rgbmap",
            mod_rgb_image_ui("rgbmap")
          ),
          # tabItem(
          #   tabName = "pop",
          #   mod_pop_ui("pop")
          # ),
          tabItem(
            tabName = "cumul",
            mod_cumul_ui("cumul")
          )
        )
      ),
      rightsidebar = rightSidebar(
        background = "dark",
        rightSidebarTabContent(
          id = 1,
          icon = "map-marked-alt",
          active = TRUE,
          mod_param_ui("product")
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
    "www", system.file("app/www", package = "inventairexy")
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
