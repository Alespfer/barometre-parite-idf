# ==============================================================================
# SCRIPT D'EXÉCUTION DU PIPELINE DE DONNÉES 
#
# Ce script constitue le cœur de la chaîne de traitement des données.
# Il est conçu pour être exécuté de manière automatisée afin de mettre à jour 
# l'ensemble des données utilisées par l'application. 
#
# Dans le cadre de ce projet, il peut être lancé manuellement ou via une tâche 
# planifiée (CRON). 
# ==============================================================================

# --- Préparation de l'environnement de travail ---
message("--- DÉBUT DU PIPELINE DE MISE À JOUR ---")
library(dplyr)
library(sf)
library(stringr)
library(readxl)
library(httr)
library(jsonlite)
library(janitor)
library(tidyr)
library(purrr)
library(writexl)

# Chargement des fonctions métier définies dans R/utils.R.
source("R/utils.R", local = TRUE)

# --- Importation des données brutes ---
message("--> Étape 1/5 : Importation des données brutes...")
raw_egapro <- import_latest_egapro() # Données Egapro depuis data.gouv.fr
raw_sirene <- import_sirene_idf() # Données SIRENE depuis OpenDataSoft

# Si une des APIs a échoué, on arrête le processus avec un message d'erreur
if (is.null(raw_egapro) || is.null(raw_sirene)) {
  stop("Échec de l'importation depuis une API. Le pipeline est arrêté.")
}

# Extraction des données depuis des fichiers locaux (Recensement INSEE).
zip_path_pop_structure <- "data/raw/base-cc-evol-struct-pop-2021_xlsx.zip"
zip_path_activite_reside <- "data/raw/base-ic-activite-residents-2021_xlsx (1).zip"
zip_path_act5 <- "data/raw/TD_ACT5_2021_xlsx.zip"
raw_pop_structure <- import_xlsx_from_zip(zip_path_pop_structure)
raw_ic_activite <- import_xlsx_from_zip(zip_path_activite_reside)
raw_act5 <- import_xlsx_from_zip(zip_path_act5, skip = 9) %>% rename_to_com()


# --- Préparation et Nettoyage ---
message("--> Étape 2/5 : Préparation et nettoyage...")

# Chaque jeu de données est nettoyé et standardisé via une fonction dédiée.
egapro_prepared <- prepare_egapro_data(raw_egapro)
sirene_clean <- clean_sirene_data(raw_sirene)

# Harmonisation du code commune de Paris.
# Les données SIRENE utilisent les codes d'arrondissement (751xx) tandis que
# les référentiels géographiques et INSEE utilisent le code commune (75056).
# Cette étape permet de joindre correctement toutes les entreprises parisiennes.
sirene_clean <- sirene_clean %>%
  mutate(code_commune = if_else(str_starts(code_commune, "751"), "75056", code_commune))
communes_features <- create_socio_features(raw_pop_structure, raw_ic_activite, raw_act5)


# Normalisation de l'indicateur d'augmentation.
# Le barème de cet indicateur Egapro diffère selon la taille de l'entreprise (sur 20 ou 35 points).
# Pour permettre des comparaisons et des agrégations statistiquement valides,
# nous le ramenons à une base commune (20 points).
egapro_prepared <- egapro_prepared %>%
  mutate(
    note_augmentation_harmonisee = case_when(
      tranche_effectifs == "50 à 250" ~ (note_augmentation / 35) * 20,
      TRUE ~ note_augmentation
    )
  )

# --- Préparation des données géographiques ---
message("--> Étape 3/5 : Préparation géographique...")
map_com_prepared <- load_and_prepare_map() %>%
  mutate(ze_code = as.character(ze_code))

# --- Création de la table finale enrichie ---
message("--> Étape 4/5 : Création de la table master...")

# Agrégation des indicateurs socio-démographiques à la maille des Zones d'Emploi.
ze_socio_features <- aggregate_socio_to_ze(communes_features)

# Dédoublonnage des tables de référence pour garantir l'unicité avant jointure.
sirene_deduplicated <- sirene_clean %>% distinct(siren, .keep_all = TRUE)
map_deduplicated <- map_com_prepared %>% distinct(com_code, .keep_all = TRUE)

# Construction de la table "master" par jointures successives.
master_df_historique <- egapro_prepared %>%
  inner_join(sirene_deduplicated, by = "siren") %>%
  inner_join(
    st_drop_geometry(map_deduplicated) %>% select(com_code, dep_name, epci_name, ze_name, ze_code),
    by = c("code_commune" = "com_code"),
    relationship = "many-to-one" # Cardinalité des données : plusieurs entreprises par commune
  ) %>%
  left_join(ze_socio_features %>% select(-ze_name), by = "ze_code")

# --- Sauvegarde des fichiers pour l'application ---
message("--> Étape 5/5 : Sauvegarde des fichiers pour Shiny...")
output_dir <- "data_shiny"
if (!dir.exists(output_dir)) dir.create(output_dir)

saveRDS(master_df_historique, file.path(output_dir, "master_df_historique.RDS"))
saveRDS(map_com_prepared, file.path(output_dir, "map_com.RDS"))
map_epci_prepared <- aggregate_map(map_com_prepared, level = "epci")
saveRDS(map_epci_prepared, file.path(output_dir, "map_epci.RDS"))
map_dep_prepared <- aggregate_map(map_com_prepared, level = "dep")
saveRDS(map_dep_prepared, file.path(output_dir, "map_dep.RDS"))
map_ze_prepared <- aggregate_map(map_com_prepared, level = "ze")
saveRDS(map_ze_prepared, file.path(output_dir, "map_ze.RDS"))

message("--- ✅ PIPELINE TERMINÉ AVEC SUCCÈS ---")
