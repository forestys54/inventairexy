#### Créer la matrice des corrélations ####

tagList(
  fluidRow(
    box(
      title = i18n$t("Ascending hierarchical classification"),
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      column(
        width = 6,
        pickerInput(
          ns("ahc_bands"),
          i18n$t("Bands selected"),
          choices = list(s2_bands[["BOA"]]),
          multiple = TRUE
        )),
      column(
        width = 6,
        radioButtons(
          ns("num_dates"), NULL,
          choiceNames = list(
            span("1"),
            span("2"),
            span("3")),
          choiceValues = list("1", "2", "3"),
          selected = "2",
          inline = TRUE
        )
      ),
      if (num_dates == "1") {
        column(
          width = 12,
          dateInput("date1", "Date :"))
      }
      else if (num_dates == "2") {
        column(
          width = 12,
          dateInput("date1", "Date :"),
          dateInput("date2", "Date :"))
      } 
      else if (num_dates == "3") {
        column(
          width = 12,
          dateInput("date1", "Date :"),
          dateInput("date2", "Date :"),
          dateInput("date3", "Date :"))
      }
    )
  )
)

# Objets : date1(facteur) date2(facteur) date3(facteur) ahcbands(liste)
# On va chercher pour les dates sélectionnées les bandes dont on a besoin et on les place dans un objet nommé A(n lignes=nombre de pixels et 6 colonnes) VOIR PASCAL

matcor <- cor(A)

# On obtient la matrice des corrélations pour notre classification hiérarchisée ascendante(ce seront nos distances) 
# Il ne faut garder que les pixels qui recoupent nos placettes VOIR PASCAL

#### Classification hiérarchique ascendante à partir de la matrice des corrélations ####

AHC <- HCPC (matcor, consol = TRUE, iter.max = 10, min = 3, max = 10) # Manque le fait d'avoir 20 placettes minimum par catégorie 

#### Moyenne - écart-type pour chaque classe ####

#### Combiner variables et données à expliquer ####

