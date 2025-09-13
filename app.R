# ==============================================================================
# app.R : Point d'entrée de l'application.
# Définit la structure globale de l'UI et orchestre l'appel aux modules serveur.
# ==============================================================================

# Chargement de tout l'environnement préparé (données, librairies, fonctions).
source("global.R", local = TRUE)

# ==============================================================================
# INTERFACE UTILISATEUR (UI)
# ==============================================================================
ui <- fluidPage(
  # Utilisation du package `bslib` pour un thème moderne et lisible.
  theme = bslib::bs_theme(
    version = 5,
    bg = "#F0F2F5",
    fg = "#1E2A3A",
    primary = "#7B61FF",
    secondary = "#495057",
    base_font = bslib::font_google("Inter", local = TRUE),
    "font-size-base" = "0.95rem"
  ),
  # Liaison à une feuille de style CSS (dans le dossier /www) personnalisée pour des ajustements fins.
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Baromètre de la Parité en Entreprise – Région Île-de-France"),
  
  
  shiny::uiOutput("data_freshness_banner_ui"),
  
  # Structure de navigation principale par onglets.
  navbarPage("Navigation", id = "main_navbar",
             tabPanel("Carte & Territoires", icon = icon("map-marked-alt"), carte_ui("carte", master_df_historique)),
             tabPanel("Analyse Sectorielle", icon = icon("industry"), sectoriel_ui("sectoriel", master_df_historique)),
             tabPanel("Analyse des Indicateurs", icon = icon("magnifying-glass-chart"), indicateurs_ui("indicateurs", master_df_historique)),
             tabPanel("Socio-démographique", icon = icon("users"), socio_dem_ui("socio_dem", master_df_historique)),
             tabPanel("Historique & Évolutions", icon = icon("chart-line"), historique_ui("historique", master_df_historique)),
             tabPanel("Documentation", icon = icon("book"), documentation_content)
  )
)

# ==============================================================================
# SERVEUR (LOGIQUE DE L'APPLICATION)
# ==============================================================================
server <- function(input, output, session) {
  
  # Logique pour afficher la bannière de données non à jour.
  output$data_freshness_banner_ui <- shiny::renderUI({
    if (!data_status$is_fresh) {
      shiny::div(class = "alert alert-warning", role = "alert",
                 style = "margin: 15px; text-align: center; font-weight: bold;",
                 shiny::icon("triangle-exclamation"),
                 data_status$message
      )
    } else {
      NULL 
    }
  })
  
 
  
  # Création d'un objet `reactiveValues` pour la communication entre modules.
  shared_state <- shiny::reactiveValues()
  
  
  # Lancement des serveurs de chaque module.
  # On passe `shared_state` aux modules qui ont besoin de communiquer.
  carte_server("carte", master_df_historique, map_epci, map_dep, map_ze, palette_accessible, shared_state)
  sectoriel_server("sectoriel", master_df_historique, palette_accessible, shared_state)
  

  indicateurs_server("indicateurs", master_df_historique, map_epci, map_dep, map_ze)
  socio_dem_server("socio_dem", master_df_historique, socio_variable_labels)
  historique_server("historique", master_df_historique, palette_accessible)
  
}

# ==============================================================================
# LANCEMENT DE L'APPLICATION
# ==============================================================================
shinyApp(ui, server)