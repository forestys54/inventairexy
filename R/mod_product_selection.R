#' product_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_product_selection_ui <- function(id){
  ns <- NS(id)
  i18n <- shiny.i18n::Translator$new(translation_json_path = "./inst/translations/translation.json")
  i18n$set_translation_language("fr")
  
  tagList(
    tabName = "product_selection",
    h3(i18n$t("Product selection")),
    fluidRow(
      box(
        title = i18n$t("Type of products"),
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        column(
          width = 3,
          radioButtons(
            ns("product"), NULL,
            choiceNames = list(
              span(
                i18n$t("THEIA product "),
                a("Pleiades, Spots, Sentinelle, Venus",
                  href = "http://www.theia-land.fr/fr",
                  target = "_blank"
                ),
                i18n$t(" (download)")
              ),
              span(
                i18n$t("PEPS product "),
                a("Sentinelle (S1, S2, S2ST, S3)",
                  href = "https://peps.cnes.fr/rocket/#/home",
                  target = "_blank"
                ),
                i18n$t(" (download)")
              )
            ),
            choiceValues = list("theia", "peps"),
            selected = "theia",
            inline = FALSE
            
          ) # end radiobutton
        ), # end column
        
        column(
          width = 3,
          ### theia ###
          conditionalPanel(
            condition = sprintf("input['%s'] == 'theia'", ns("product")),
            radioButtons(
              ns("theiacollection"), NULL,
              choiceNames = list(
                span(
                  a("Landsat",
                    href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=3487",
                    target = "_blank"
                  )
                ),
                span(
                  a("SpotWorldHeritage",
                    href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=12923",
                    target = "_blank"
                  )
                ),
                span(
                  a("SENTINEL2",
                    href = "https://theia.cnes.fr/atdistrib/documents/PSC-NT-411-0362-CNES_01_00_SENTINEL-2A_L2A_Products_Description.pdf",
                    target = "_blank"
                  )
                ),
                span(
                  a("Snow",
                    href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=10748#fr",
                    target = "_blank"
                  )
                ),
                span(
                  a("VENUS",
                    href = "http://www.cesbio.ups-tlse.fr/multitemp/?page_id=12984",
                    target = "_blank"
                  )
                )
              ),
              choiceValues = list("landsat", "spotworldheritage", "sentinel2", "snow", "venus"),
              selected = "sentinel2",
              inline = FALSE
            ) # end radiobutton
          ), # end conditionalpanel
          
          ### peps ###
          conditionalPanel(
            condition = sprintf("input['%s'] == 'peps'", ns("product")),
            # condition = "input.product == 'peps'",
            radioButtons(
              ns("pepscollection"), NULL,
              choiceNames = list(
                span(
                  a("S1",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                    target = "_blank"
                  )
                ),
                span(
                  a("S2",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                ),
                span(
                  a("S2ST",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                ),
                span(
                  a("S3",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel3",
                    target = "_blank"
                  )
                )
              ),
              choiceValues = list("s1", "s2", "s2st", "s3"),
              selected = "s2",
              inline = FALSE
            ) # end radiobutton
          ) # end conditionalpanel
        ), # end column
        
        column(
          width = 3,
          
          #### LANDSAT ####
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'landsat'", ns("product"), ns("theiacollection")),
            radioButtons(
              ns("theiaplatformlandsat"), NULL,
              choiceNames = list(
                span(
                  a("LANDSAT5",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                    target = "_blank"
                  )
                ),
                span(
                  a("LANDSAT7",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                ),
                span(
                  a("LANDSAT8",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                )
              ),
              choiceValues = list("landsat5", "landsat7", "landsat8"),
              selected = "landsat8",
              inline = FALSE
            ) # end radiobutton
          ), # end conditionalpanel
          
          #### SPOT WORLD HERITAGE ####
          conditionalPanel(
            condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'spotworldheritage'", ns("product"), ns("theiacollection")),
            radioButtons(
              ns("theiaplatformspotworldheritage"), NULL,
              choiceNames = list(
                span(
                  a("SPOT1",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                    target = "_blank"
                  )
                ),
                span(
                  a("SPOT2",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                ),
                span(
                  a("SPOT3",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                ),
                span(
                  a("SPOT4",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                ),
                span(
                  a("SPOT5",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                )
              ),
              choiceValues = list("spot1", "spot2", "spot3", "spot4", "spot5"),
              selected = "spot5",
              inline = FALSE
            ) # end radiobutton
          ), # end conditionalPanel
          
          #### SENTINEL 2 ####
          conditionalPanel(
            condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'sentinel2'", ns("product"), ns("theiacollection")),
            radioButtons(
              ns("theiaplatformsentinel"), NULL,
              choiceNames = list(
                span(
                  a("SENTINEL2A",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel1",
                    target = "_blank"
                  )
                ),
                span(
                  a("SENTINEL2B",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                )
              ),
              choiceValues = list("s2a", "s2b"),
              selected = "s2b",
              inline = FALSE
            ) # end radiobutton
          ), # end conditionalpanel
          
          #### VENUS ####
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'venus'", ns("product"), ns("theiacollection")),
            radioButtons(
              ns("theiaplatformvenus"), NULL,
              choiceNames = list(
                span(
                  a("VENUS",
                    href = "https://peps.cnes.fr/rocket/plus/plus.htm#sentinel2",
                    target = "_blank"
                  )
                )
              ),
              choiceValues = list("venus"),
              selected = "venus",
              inline = FALSE
            ) # end radiobutton
          ) # end conditionalpanel
        ), # end column
        
        column(
          width = 3,
          
          #### SENTINEL2 ####
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'theia' && input['%s'] == 'sentinel2'", ns("product"), ns("theiacollection")),
            radioButtons(
              ns("theiaplatformsentinellevel"), NULL,
              choiceNames = list(
                span(
                  a("LEVEL1C",
                    href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                    target = "_blank"
                  )
                ),
                span(
                  a("LEVEL2A",
                    href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                    target = "_blank"
                  )
                ),
                span(
                  a("LEVEL3A",
                    href = "http://www.cesbio.ups-tlse.fr/multitemp/?p=2766",
                    target = "_blank"
                  )
                )
              ),
              choiceValues = list("l1c", "l2a", "l3a"),
              selected = "l2a",
              inline = FALSE
            ) # end radiobutton
          ) # end conditionnalPanel
        ) # end column
      ) # end box
    ) # end fluidrow
  ) # end taglist
  
}

#' product_selection Server Function
#'
#' @noRd 
mod_product_selection_server <- function(input, output, session){
  ns <- session$ns
  
  # for save parameters
  observe({
    rv$product <- input$product
    rv$theiacollection <- input$theiacollection
    rv$pepscollection <- input$pepscollection
    rv$theiaplatformsentinellevel <- input$theiaplatformsentinellevel
    rv$theiaplatformlandsat <- input$theiaplatformlandsat
    rv$theiaplatformspotworldheritage <- input$theiaplatformspotworldheritage
    rv$theiaplatformsentinel <- input$theiaplatformsentinel
    rv$theiaplatformvenus <- input$theiaplatformvenus
  })
  
  
}

## To be copied in the UI
# mod_product_selection_ui("product_selection_ui_1")

## To be copied in the server
# callModule(mod_product_selection_server, "product_selection_ui_1")

