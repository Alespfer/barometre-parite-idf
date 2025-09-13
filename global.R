# ==============================================================================
# global.R : Script de configuration de l'environnement Shiny.
# Prépare l'environnement de l'application avant son lancement.
# ==============================================================================

# --- Chargement des librairies essentielles ---
library(shiny); library(sf); library(leaflet); library(bslib); library(dplyr)
library(ggplot2); library(DT); library(htmltools); library(scales); library(stringr)
library(ggrepel); library(tidyr); library(plotly); library(shinyWidgets); library(jsonlite)
library(viridis); library(shinydashboard); library(writexl); library(purrr); library(weights)


# --- Chargement des sources ---
# Chaque module de l'application (correspondant à un onglet) est "sourcé".
source("R/utils.R", local = TRUE); source("R/documentation.R", local = TRUE)
source("R/carte_module.R", local = TRUE); source("R/sectoriel_module.R", local = TRUE)
source("R/socio_dem_module.R", local = TRUE); source("R/historique_module.R", local = TRUE)
source("R/indicateurs_module.R", local = TRUE)

# --- Chargement des Données  ---
# Lecture des fichiers .RDS préparés par le pipeline. 
master_df_historique <- readRDS("data_shiny/master_df_historique.RDS")
map_dep <- readRDS("data_shiny/map_dep.RDS")
map_epci <- readRDS("data_shiny/map_epci.RDS")
map_ze <- readRDS("data_shiny/map_ze.RDS")

# --- Pré-calculs ---
map_dep <- sf::st_transform(map_dep, crs = 4326)
map_epci <- sf::st_transform(map_epci, crs = 4326)
map_ze <- sf::st_transform(map_ze, crs = 4326)


# Création de la variable de pondération.
eff_key <- c("50 à 250" = 150, "251 à 999" = 625, "1000 et plus" = 1500)
master_df_historique <- master_df_historique %>%
  mutate(poids = unname(eff_key[tranche_effectifs]))


# Vérification de la fraîcheur des données pour afficher une bannière d'alerte si besoin.
data_status <- check_data_freshness()

# --- Objets statiques ---
palette_accessible <- c("#0072B2","#F0E442", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#CC79A7")
socio_variable_labels <- c("part_femmes_cadres" = "Part de femmes cadres (%)", "part_femmes_prof_inter" = "Part de femmes prof. inter. (%)", "taux_femmes_parmi_cadres" = "Taux de féminisation des postes de cadres (%)", "taux_activite_femmes" = "Taux d'activité des femmes 15-64 ans (%)")

indicateur_labels <- c(
  "Écart de Rémunération (sur 40)" = "note_remuneration",
  "Écart d'Augmentations (harmonisé sur 20)" = "note_augmentation_harmonisee",
  "Écart de Promotions (sur 15)" = "note_promotion",
  "Augmentation au retour de congé maternité (sur 15)" = "note_conge_mat",
  "Part de femmes dans les 10 plus hautes rémunérations (sur 10)" = "note_hautes_rem"
)

indicateur_descriptions <- list(
  "note_remuneration" = "<strong>Écart de rémunération (40 pts) :</strong> Compare la rémunération moyenne des femmes et des hommes, par tranche d'âge et par catégorie de postes équivalents. C'est l'indicateur avec le plus de poids dans l'Index.",
  "note_augmentation_harmonisee" = "<strong>Écart d'augmentations individuelles (harmonisé sur 20 pts) :</strong> Compare le % de femmes et d'hommes augmentés. La note originale (sur 20 ou 35 pts selon la taille de l'entreprise) a été normalisée sur une échelle de 20 points pour permettre une comparaison juste.",
  "note_promotion" = "<strong>Écart de promotions (15 pts) :</strong> Compare le pourcentage de femmes et d'hommes ayant été promus. Cet indicateur ne concerne que les entreprises de plus de 250 salariés.",
  "note_conge_mat" = "<strong>Augmentations au retour de congé maternité (15 pts) :</strong> Vérifie que les salariées, à leur retour de congé maternité, ont bien bénéficié des augmentations perçues par les autres salariés pendant leur absence.",
  "note_hautes_rem" = "<strong>Hautes rémunérations (10 pts) :</strong> Mesure la parité parmi les 10 salariés ayant perçu les plus hautes rémunérations dans l'entreprise."
)

secteur_labels_courts <- c(
  "Hébergement et restauration" = "Hébergement & Restauration",
  "Administration publique" = "Administration Publique",
  "Activités immobilières" = "Immobilier",
  "Production et distribution d'électricité, de gaz, de vapeur..." = "Énergie (Électricité, Gaz)", # Plus clair
  "Santé humaine et action sociale" = "Santé & Action Sociale",
  "Enseignement" = "Enseignement",
  "Autres activités de services" = "Autres Services",
  "Transports et entreposage" = "Transports & Entreposage",
  "Activités de services administratifs et de soutien" = "Services Administratifs",
  "Production et distribution d'eau, assainissement..." = "Services (Eau, Déchets)", # Plus clair
  "Industrie manufacturière" = "Industrie Manufacturière",
  "Commerce, réparation d'automobiles et de motocycles" = "Commerce & Réparation Auto",
  "Arts, spectacles et activités récréatives" = "Arts & Spectacles",
  "Activités spécialisées, scientifiques et techniques" = "Services Spécialisés / Tech.",
  "Activités financières et d'assurance" = "Finance & Assurance",
  "Information et communication" = "Information & Communication",
  "Construction" = "Construction",
  "Industries extractives" = "Industries Extractives",
  "Agriculture, sylviculture et pêche" = "Agriculture"
)
