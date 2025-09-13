# ==============================================================================
# FONCTIONS UTILITAIRES (R/utils.R)
#
# Ce script centralise l'ensemble des fonctions métier et techniques
# réutilisées à travers le projet, principalement dans le pipeline de
# préparation des données.
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1 : FONCTIONS D'IMPORTATION DES DONNÉES BRUTES 
# ------------------------------------------------------------------------------


#' @title Importe la dernière version des données Egapro.
#' @description Interroge dynamiquement l'API de data.gouv.fr pour trouver le jeu de
#'   données de l'Index Égapro, identifie la ressource la plus récente, et la télécharge.
#' @return Un dataframe contenant les données Egapro brutes, ou NULL en cas d'échec.
import_latest_egapro <- function() {
  message("--- Importation des données Egapro depuis data.gouv.fr ---")
  
  # 'tryCatch` pour rendre la fonction robuste en cas d'erreur.
  tryCatch({
    # Recherche du jeu de données via son titre.
    res <- httr::GET(
      "https://www.data.gouv.fr/api/1/datasets/",
      query = list(q = "Index Egalite Professionnelle F/H", page_size = 1)
    )
    # S'arrête si la requête initiale à l'API échoue
    httr::stop_for_status(res)
    
    ds_list <- httr::content(res, as = "parsed", encoding = "UTF-8")$data
    if (length(ds_list) == 0) {
      stop("Le dataset Egapro est introuvable sur data.gouv.fr.")
    }
    ds <- ds_list[[1]]
    
    message("-> Dataset trouvé : '", ds$title, "' (modifié le ", ds$last_modified, ")")
    
    # Identification d'un fichier téléchargeable.
    resources <- ds$resources
    formats_ok <- c("json", "csv", "xlsx", "xls")
    idx <- which(tolower(sapply(resources, `[[`, "format")) %in% formats_ok)[1]
    if (is.na(idx)) {
      stop("Aucune ressource téléchargeable (JSON, CSV, XLSX) trouvée.")
    }
    
    chosen <- resources[[idx]]
    url <- chosen$url
    fmt <- tolower(chosen$format)
    
    # Téléchargement et lecture en fonction du format.
    message("-> Téléchargement de la ressource au format '", fmt, "'...")
    
    if (fmt == "json") {
      req <- httr::GET(url)
      httr::stop_for_status(req)
      data <- jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"))
    } else if (fmt == "csv") {
      data <- readr::read_csv2(url, show_col_types = FALSE)
    } else {
      # Gestion des fichiers Excel (xls/xlsx)
      tmp <- tempfile(fileext = paste0(".", fmt))
      httr::GET(url, httr::write_disk(tmp, overwrite = TRUE))
      data <- readxl::read_excel(tmp, sheet = 1)
    }
    message("✅ Import Egapro terminé : ", nrow(data), " lignes.")
    return(data)
    
  }, error = function(e) {
    # Bloc exécuté en cas d'erreur dans le `try`.
    warning(paste("ÉCHEC de l'importation Egapro. L'API est peut-être indisponible ou le format a changé. Erreur :", e$message))
    return(NULL)
  })
}



#' @title Importe les données SIRENE pour les entreprises de plus de 50 salariés en IDF.
#' @description Interroge l'API OpenDataSoft en effectuant des appels distincts pour
#'   chaque tranche d'effectifs. 
#' @return Un dataframe des données SIRENE brutes, ou NULL en cas d'échec total.
import_sirene_idf <- function() {
  message("--- Importation des données SIRENE (IDF, +50 salariés) ---")
  # Définition des URLs pour chaque tranche d'effectifs ciblée.
  tranches_urls <- list(
    "50-99"     = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%2250%20%C3%A0%2099%20salari%C3%A9s%22",
    "100-199"   = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%22100%20%C3%A0%20199%20salari%C3%A9s%22",
    "200-249"   = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%22200%20%C3%A0%20249%20salari%C3%A9s%22",
    "250-499"   = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%22250%20%C3%A0%20499%20salari%C3%A9s%22",
    "500-999"   = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%22500%20%C3%A0%20999%20salari%C3%A9s%22",
    "1000-1999" = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%221%20000%20%C3%A0%201%20999%20salari%C3%A9s%22",
    "2000-4999" = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%222%20000%20%C3%A0%204%20999%20salari%C3%A9s%22",
    "5000-9999" = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%225%20000%20%C3%A0%209%20999%20salari%C3%A9s%22",
    "10000+"    = "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/economicref-france-sirene-v3/exports/json?lang=fr&refine=regionetablissement%3A%22%C3%8Ele-de-France%22&refine=trancheeffectifsunitelegale%3A%2210%20000%20salari%C3%A9s%20et%20plus%22"
  )
  
  # Logique de téléchargement avec gestion d'erreur 
  liste_de_dataframes <- lapply(names(tranches_urls), function(nom_tranche) {
    url <- tranches_urls[[nom_tranche]]
    message("-> Chargement de la tranche : ", nom_tranche, "...")
    tryCatch({
      jsonlite::fromJSON(url)
    }, error = function(e) {
      warning(paste("  /!\\ Erreur pour la tranche '", nom_tranche, "'. Elle sera ignorée. Erreur:", e$message))
      return(NULL) 
    })
  })
  
  # Combianison de tous les dataframes téléchargés en un seul.
  data_final <- dplyr::bind_rows(liste_de_dataframes)
  
  # On vérifie que le téléchargement n'a pas complètement échoué
  if (nrow(data_final) == 0) {
    warning("Échec de l'importation SIRENE. Aucune donnée n'a pu être collectée sur les différentes tranches.")
    return(NULL)
  }
  
  message("✅ Import SIRENE terminé : ", nrow(data_final), " lignes collectées.")
  return(data_final)
}


#' @title Importe un fichier Excel contenu dans une archive ZIP.
#' @description Fonction générique pour extraire et lire un fichier Excel sans
#'   avoir à décompresser manuellement l'archive.
#' @param zip_path Chemin vers le fichier .zip.
#' @param ... Arguments additionnels passés à `readxl::read_xlsx`.
#' @return Un dataframe.
import_xlsx_from_zip <- function(zip_path, sheet = 1, file_pattern = "\\.xlsx$", skip = 5) {
  message("--- Importation des données du Recensement depuis un ZIP ---")
  if (!file.exists(zip_path)) stop("Le fichier ZIP est introuvable : ", zip_path)
  
  tmp_dir <- tempfile()
  utils::unzip(zip_path, exdir = tmp_dir)
  
  xlsx_file <- list.files(tmp_dir, pattern = file_pattern, full.names = TRUE, recursive = TRUE)[1]
  if (is.na(xlsx_file)) stop("Aucun fichier Excel trouvé dans : ", zip_path)
  
  df <- readxl::read_xlsx(xlsx_file, sheet = sheet, skip = skip) %>% janitor::clean_names()
  
  unlink(tmp_dir, recursive = TRUE)
  
  message("✅ Import terminé : ", nrow(df), " lignes.")
  df
}

# ------------------------------------------------------------------------------
# SECTION 2 : FONCTIONS DE PRÉPARATION ET DE NETTOYAGE 
# ------------------------------------------------------------------------------

#' @title Convertit un code NAF en grand secteur d'activité.
#' @description Utilise la nomenclature d'activités française (NAF) pour regrouper
#'   les codes détaillés en 19 grands secteurs pertinents pour l'analyse.
#' @param code_naf Un vecteur de codes NAF (ex: "85.32Z").
#' @return Un vecteur de libellés de secteurs d'activité.
get_secteur_from_naf <- function(code_naf) {
  section_num <- as.integer(substr(code_naf, 1, 2))
  
  dplyr::case_when(
    is.na(section_num) ~ "Non défini",
    section_num >= 1 & section_num <= 3 ~ "Agriculture, sylviculture et pêche",
    section_num >= 5 & section_num <= 9 ~ "Industries extractives",
    section_num >= 10 & section_num <= 33 ~ "Industrie manufacturière",
    section_num == 35 ~ "Production et distribution d'électricité, de gaz, de vapeur...",
    section_num >= 36 & section_num <= 39 ~ "Production et distribution d'eau, assainissement...",
    section_num >= 41 & section_num <= 43 ~ "Construction",
    section_num >= 45 & section_num <= 47 ~ "Commerce, réparation d'automobiles et de motocycles",
    section_num >= 49 & section_num <= 53 ~ "Transports et entreposage",
    section_num >= 55 & section_num <= 56 ~ "Hébergement et restauration",
    section_num >= 58 & section_num <= 63 ~ "Information et communication",
    section_num >= 64 & section_num <= 66 ~ "Activités financières et d'assurance",
    section_num == 68 ~ "Activités immobilières",
    section_num >= 69 & section_num <= 75 ~ "Activités spécialisées, scientifiques et techniques",
    section_num >= 77 & section_num <= 82 ~ "Activités de services administratifs et de soutien",
    section_num == 84 ~ "Administration publique",
    section_num == 85 ~ "Enseignement",
    section_num >= 86 & section_num <= 88 ~ "Santé humaine et action sociale",
    section_num >= 90 & section_num <= 93 ~ "Arts, spectacles et activités récréatives",
    section_num >= 94 & section_num <= 96 ~ "Autres activités de services",
    TRUE ~ "Non défini"
  )
}

#' @title Prépare et nettoie les données Egapro.
#' @description Cette fonction standardise les données brutes :
#'   - Renomme les colonnes de manière cohérente.
#'   - Convertit les types de données (numérique, texte).
#'   - Gère les variations de noms de colonnes.
#'   - Filtre pour ne garder que les déclarations pertinentes.
#'   - Dédoublonne les déclarations pour n'avoir qu'une ligne par entreprise et par an.
#' @param raw_egapro_df Le dataframe brut issu de `import_latest_egapro`.
#' @return Un dataframe Egapro propre et standardisé.
prepare_egapro_data <- function(raw_egapro_df) {
  message("--- Préparation et standardisation des données Egapro ---")
  
  # `janitor::clean_names` pour normaliser les noms de colonnes.
  df_clean <- raw_egapro_df %>% janitor::clean_names()
  
  # Gestion des noms de colonnes 
  naf_col <- intersect(c("code_naf", "code_naf_ape"), names(df_clean))[1]
  effectifs_col <- intersect(c("tranche_deffectifs", "tranche_d_effectifs", "tranche_effectifs"), names(df_clean))[1]
  if (is.na(naf_col) || is.na(effectifs_col)) stop("Colonnes NAF ou effectifs introuvables.")
  
  cols_aug_existantes <- intersect(c("note_ecart_taux_daugmentation_hors_promotion", "note_ecart_taux_daugmentation"), names(df_clean))
  
  df_prepared <- df_clean %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("note"), as.character),
      dplyr::across(dplyr::starts_with("note"), as.numeric), # Conversion des notes en numérique.
      siren = stringr::str_pad(siren, 9, side = "left", pad = "0"),
      annee = as.integer(annee)
    )
  
  # `coalesce` pour fusionner les colonnes de note d'augmentation en une seule.
  if (length(cols_aug_existantes) > 0) {
    df_prepared <- df_prepared %>% dplyr::mutate(note_augmentation_unifiee = dplyr::coalesce(!!!rlang::syms(cols_aug_existantes)))
  } else {
    df_prepared$note_augmentation_unifiee <- NA_real_
  }
  
  
  # Sélection finale et renommage pour obtenir un schéma de données propre.
  df_prepared <- df_prepared %>%
    dplyr::filter(structure == "Entreprise", !is.na(note_index)) %>%
    dplyr::rename(
      index = note_index, tranche_effectifs = dplyr::all_of(effectifs_col), code_naf = dplyr::all_of(naf_col), 
      note_remuneration = note_ecart_remuneration, note_augmentation = dplyr::any_of("note_augmentation_unifiee"),
      note_promotion = note_ecart_taux_de_promotion, note_conge_mat = note_retour_conge_maternite,
      note_hautes_rem = note_hautes_remunerations
    ) %>%
    dplyr::mutate(secteur_activite = get_secteur_from_naf(code_naf)) %>%
    dplyr::select(siren, annee, index, raison_sociale, tranche_effectifs, code_naf, secteur_activite,
                  dplyr::any_of(c("note_remuneration", "note_augmentation", "note_promotion", "note_conge_mat", "note_hautes_rem"))) %>%
    dplyr::distinct(siren, annee, .keep_all = TRUE)
  
  message("✅ Préparation Egapro terminée : ", nrow(df_prepared), " observations standardisées.")
  return(df_prepared)
}


#' @title Nettoie les données SIRENE.
#' @description Filtre pour ne garder que les sièges sociaux, sélectionne les
#'   colonnes utiles (SIREN, code commune, coordonnées GPS) et formate les données.
#' @param raw_sirene_df Le dataframe brut issu de `import_sirene_idf`.
#' @return Un dataframe SIRENE propre, avec une ligne par siège social.
clean_sirene_data <- function(raw_sirene_df) {
  message("--- Nettoyage des données SIRENE ---")
  sirene_clean <- raw_sirene_df %>%
    janitor::clean_names() %>%
    dplyr::filter(etablissementsiege == "oui") %>%
    dplyr::select(siren, code_commune = codecommuneetablissement, geolocetablissement) %>%
    dplyr::distinct(siren, .keep_all = TRUE) %>%
    tidyr::unnest(geolocetablissement) %>%     # Transformation de la colonne `geolocetablissement` en deux colonnes distinctes `lat` et `lon`.
    dplyr::filter(!is.na(lat) & !is.na(lon)) %>%
    dplyr::rename(latitude = lat, longitude = lon)
  
  message("✅ Nettoyage SIRENE OK : ", nrow(sirene_clean), " sièges sociaux géolocalisés.")
  return(sirene_clean)
}

#' @title Standardise le nom de la colonne du code commune.
#' @param df Un dataframe, issu des données de l'INSEE.
#' @return Le même dataframe avec la colonne du code commune renommée.
rename_to_com <- function(df) {
  col_names <- names(df)
  commune_col <- intersect(c("codgeo", "com", "cod_com"), col_names)[1]
  
  if (is.na(commune_col)) {
    warning("Impossible de trouver une colonne de code commune ('codgeo', 'com', 'cod_com'). Le dataframe est retourné tel quel.")
    return(df)
  }
  
  df %>%
    dplyr::rename(code_commune = !!rlang::sym(commune_col))
}


#' @title Calcule les indicateurs socio-démographiques au niveau communal.
#' @param df_ic Dataframe "base-ic-activite-residents-2021".
#' @param df_act5 Dataframe "TD_ACT5_2021" avec le détail par sexe, âge et CSP.
#' @return Un dataframe contenant les indicateurs socio-démographiques calculés pour chaque commune.
create_socio_features <- function(df_pop_structure, df_ic, df_act5) {
  
  message("-> Démarrage du calcul des indicateurs socio-démographiques...")
  
  df_ic <- df_ic %>% janitor::clean_names()
  df_act5 <- df_act5 %>% janitor::clean_names()
  
  # 1. Calcul du taux d'activité des femmes de 15-64 ans.
  taux_activite <- df_ic %>%
    dplyr::select(code_commune = com, p21_fact1564, p21_f1564) %>%
    dplyr::group_by(code_commune) %>%
    dplyr::summarise(
      total_femmes_actives = sum(p21_fact1564, na.rm = TRUE),
      total_femmes_pop = sum(p21_f1564, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      taux_activite_femmes = (total_femmes_actives / total_femmes_pop) * 100
    ) %>%
    dplyr::select(code_commune, taux_activite_femmes)
  
  # 2. Calcul des indicateurs de structure de l'emploi.
  structure_emploi <- df_act5 %>%
    rename_to_com() %>%
    dplyr::mutate(
      # Femmes (sexe2)
      femmes_cadres = rowSums(dplyr::select(., dplyr::starts_with("cs1_63") & dplyr::ends_with("_sexe2")), na.rm = TRUE),
      femmes_prof_inter = rowSums(dplyr::select(., dplyr::starts_with("cs1_64") & dplyr::ends_with("_sexe2")), na.rm = TRUE),
      femmes_actives_occupees_total = rowSums(dplyr::select(., dplyr::starts_with("cs1_") & dplyr::ends_with("_sexe2")), na.rm = TRUE),
      # Ensemble (sexe1 + sexe2)
      ensemble_cadres = rowSums(dplyr::select(., dplyr::starts_with("cs1_63")), na.rm = TRUE)
    ) %>%
    # Calcul des ratios finaux en pourcentage.
    dplyr::mutate(
      part_femmes_cadres = (femmes_cadres / femmes_actives_occupees_total) * 100,
      part_femmes_prof_inter = (femmes_prof_inter / femmes_actives_occupees_total) * 100,
      taux_femmes_parmi_cadres = (femmes_cadres / ensemble_cadres) * 100
    ) %>%
    dplyr::select(code_commune, part_femmes_cadres, part_femmes_prof_inter, taux_femmes_parmi_cadres)
  
  # 3. Fusion des indicateurs en une table
  communes_features <- taux_activite %>%
    dplyr::full_join(structure_emploi, by = "code_commune") %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA)))
  
  message("✅ Création des indicateurs socio-démographiques terminée.")
  return(communes_features)
}


#' @title Agrège les données socio-démographiques communales par Zone d'Emploi.
#' @description Prend les indicateurs communaux et calcule leur moyenne à l'échelle
#'   de la Zone d'Emploi, en se basant sur une table de correspondance.
#' @param communes_features_df Le dataframe des indicateurs par commune.
#' @return Un dataframe avec les indicateurs agrégés par Zone d'Emploi.
aggregate_socio_to_ze <- function(communes_features_df) {
  message("--- Agrégation des indicateurs socio-démographiques par Zone d'Emploi ---")
  
  path_ze_ref <- "data/raw/commune_ze_2020.xlsx"
  if (!file.exists(path_ze_ref)) {
    stop("Fichier de correspondance 'commune_ze_2020.xlsx' introuvable.")
  }
  ze_ref <- readxl::read_excel(path_ze_ref) %>%
    dplyr::select(code_commune = CODGEO, ze_code = ZE2020, ze_name = LIBZE2020)
  
  ze_features <- communes_features_df %>%
    # Jointure avec la table de correspondance pour obtenir le code ZE
    dplyr::inner_join(ze_ref, by = "code_commune") %>%
    # Grouper par Zone d'Emploi
    dplyr::group_by(ze_code, ze_name) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = where(is.numeric),
        .fns = ~mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))
  
  message("✅ Agrégation terminée : ", nrow(ze_features), " Zones d'Emploi traitées.")
  return(ze_features)
}

# ------------------------------------------------------------------------------
# SECTION 3 : FONCTIONS DE GESTION GÉOGRAPHIQUE
# ------------------------------------------------------------------------------



#' @title Charge et prépare le fond de carte communal de référence.
#' @description Cette fonction télécharge le fond de carte des communes d'Île-de-France
#'   et effectue plusieurs opérations de préparation :
#'   1. Harmonise les codes communes de Paris (arrondissements -> commune).
#'   2. Crée une colonne `interco_name` unifiée (EPT en petite couronne, EPCI en grande).
#'   3. Joint les données avec le référentiel des Zones d'Emploi.
#' @return Un objet `sf` prêt à l'emploi.
load_and_prepare_map <- function() {
  message("--- Chargement et préparation du fond de carte ---")
  
  # Données géographiques des communes d'Île-de-France.
  myURL <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/georef-france-commune-arrondissement-municipal-millesime/exports/geojson?lang=fr&refine=reg_name%3A%22%C3%8Ele-de-France%22&refine=year%3A%222020%22"
  
  map_idf <- sf::st_read(myURL, quiet = TRUE) %>%
    # On garde uniquement les départements de l'Île-de-France.
    dplyr::filter(dep_code %in% c("75", "92", "93", "94", "77", "78", "91", "95")) %>%
    dplyr::mutate(dplyr::across(
      c(com_arm_code, com_arm_name, ept_code, ept_name, epci_code, epci_name, dep_code, dep_name), 
      as.character
    )) %>%
    # On sélectionne et on renomme les colonnes pertinentes
    dplyr::select(
      com_code = com_arm_code, com_name = com_arm_name, 
      ept_code, ept_name, epci_code, epci_name, 
      dep_code, dep_name, geometry
    )
  
  # On charge le fichier Excel qui fait le lien entre une commune et sa zone d'emploi.
  ze_ref <- readxl::read_excel("data/raw/commune_ze_2020.xlsx") %>%
    dplyr::select(com_code = CODGEO, ze_code = ZE2020, ze_name = LIBZE2020) %>%
    dplyr::mutate(com_code = as.character(com_code))
  
  # --- Logique de préparation des données ---
  map_prepared <- map_idf %>%
    dplyr::mutate(
      # Pour la ville de Paris, on regroupe tous les arrondissements (75101, 75102...) sous un seul code (75056).
      com_code = ifelse(stringr::str_starts(com_code, "751"), "75056", com_code),
      
      # On crée la colonne "interco_name" qui contiendra le nom de l'intercommunalité.
      interco_name = dplyr::case_when(
        # Règle n°1 : Communes en Grande Couronne mais appartennant à un EPT. 
        com_code == "95018" ~ "Boucle Nord de Seine",
        com_code %in% c("91432", "91326", "91589", "91479", "91027", "91687") ~ "Grand-Orly Seine Bièvre",
        
        # Règle n°2 : Si la commune est en Petite Couronne (75, 92, 93, 94), on prend le nom de l'EPT.
        dep_code %in% c("75", "92", "93", "94") ~ ept_name,
        
        # Règle n°3 : Pour toutes les autres communes (la Grande Couronne), on prend le nom de l'EPCI.
        TRUE ~ epci_name
      ),
      
      interco_code = dplyr::case_when(
        com_code == "95018" ~ "200057990",
        com_code %in% c("91432", "91326", "91589", "91479", "91027", "91687") ~ "200058014",
        
        dep_code %in% c("75", "92", "93", "94") ~ ept_code,
        
        TRUE ~ epci_code
      ),
      
      interco_name = ifelse(dep_code == "75", "Ville de Paris", interco_name),
      interco_code = ifelse(dep_code == "75", "T1", interco_code)
    ) %>%
    
    # Fusion de la carte préparée avec les informations sur les zones d'emploi.
    dplyr::left_join(ze_ref, by = "com_code") %>%
    
    dplyr::select(
      com_code, com_name,
      epci_code = interco_code, 
      epci_name = interco_name,
      dep_code, dep_name,
      ze_code, ze_name,
      geometry
    )
  
  message("✅ Fond de carte communal OK : ", nrow(map_prepared), " communes/arrondissements chargés.")
  
  return(map_prepared)
}


#' @title Agrège des géométries à un niveau territorial supérieur.
#' @param map_com_sf L'objet `sf` des communes.
#' @param level Le niveau d'agrégation souhaité ("dep", "epci", ou "ze").
#' @return Un nouvel objet `sf` avec les géométries agrégées.
aggregate_map <- function(map_com_sf, level = "epci") {
  group_vars <- if (level == "epci") c("epci_code", "epci_name") else if (level == "dep") c("dep_code", "dep_name") else c("ze_code", "ze_name")
  
  map_com_sf %>%
    filter(!sf::st_is_empty(geometry) & !is.na(!!sym(group_vars[1]))) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(geometry = sf::st_union(geometry), .groups = "drop")
}

# ------------------------------------------------------------------------------
# SECTION 4 : FONCTIONS UTILITAIRES POUR L'APPLICATION SHINY
# ------------------------------------------------------------------------------

#' @title Génère le contenu HTML du popup d'une entreprise pour la carte.
#' @description Crée une chaîne de caractères HTML formatée pour être affichée
#'   dans un popup Leaflet. 
#' @param df_row Une ligne du dataframe `master_df_historique`.
#' @return Une chaîne de caractères HTML.
generate_company_popup <- function(df_row) {
  format_note <- function(note, max_points) {
    numeric_note <- as.numeric(note)
    if (is.na(numeric_note)) 'NC' else paste(note, "/", max_points)
  }
  
  # Logique métier pour afficher le bon dénominateur pour la note "Augmentations".
  # Le barème officiel est sur 35 pour les entreprises de 50-250 salariés
  tranche_eff <- df_row["tranche_effectifs"]
  max_points_aug <- if (!is.na(tranche_eff) && tranche_eff == "50 à 250") {
    35
  } else {
    20
  }
  
  popup_html <- paste0(
    "<h4>", df_row["raison_sociale"], "</h4><hr>",
    "<p><strong>Score Egapro (", df_row["annee"], ") : </strong>", 
    "<strong style='font-size: 1.2em; color: #08519c;'>", format_note(df_row["index"], 100), "</strong></p>",
    "<p><strong>Détail des indicateurs :</strong></p>",
    "<ul>",
    "<li>Rémunération : ", format_note(df_row["note_remuneration"], 40), "</li>",
    "<li>Augmentations : ", format_note(df_row["note_augmentation"], max_points_aug), "</li>",
    "<li>Promotions : ", format_note(df_row["note_promotion"], 15), "</li>",
    "<li>Congé maternité : ", format_note(df_row["note_conge_mat"], 15), "</li>",
    "<li>Hautes rémunérations : ", format_note(df_row["note_hautes_rem"], 10), "</li>",
    "</ul><hr>",
    "<p><strong>Taille : </strong>", df_row["tranche_effectifs"], "</p>",
    "<p><strong>Secteur : </strong>", df_row["secteur_activite"], "</p>",
    "<p><strong>SIREN : </strong>", df_row["siren"], "</p>"
  )

  return(htmltools::HTML(popup_html))
}



#' @title Crée un widget de sélection pour la palette de couleurs.
#' @param id L'ID du widget pour le namespace Shiny.
#' @return Un objet UI Shiny.
color_switch_ui <- function(id) {
  shinyWidgets::switchInput(
    inputId = id, 
    label = "Palette accessible", 
    onLabel = "Oui", 
    offLabel = "Non", 
    value = FALSE,
    size = "small", 
    inline = TRUE, 
    width = "auto"
  )
}


# ------------------------------------------------------------------------------
# SECTION 5 : FONCTIONS DE GESTION DE L'APPLICATION
# ------------------------------------------------------------------------------

#' Vérifie la fraîcheur d'un fichier de données.
#'
#' @param file_path Chemin vers le fichier à vérifier.
#' @param seuil Nombre de jours au-delà duquel les données sont considérées comme obsolètes.
#' @return Une liste contenant l'état des données (is_fresh), leur âge et un message.
check_data_freshness <- function(file_path = "data_shiny/master_df_historique.RDS", seuil = 30) {
  
  if (!file.exists(file_path)) {
    stop("Fichier de données principal introuvable à l'emplacement : ", file_path)
  }
  
  last_modified_time <- file.info(file_path)$mtime
  age_days <- as.numeric(difftime(Sys.time(), last_modified_time, units = "days"))
  
  is_fresh <- age_days <= seuil
  
  message_text <- if (is_fresh) {
    paste0("Les données sont à jour (dernière mise à jour il y a ", floor(age_days), " jours).")
  } else {
    paste0("Les données datent de plus de ", seuil, " jours (dernière mise à jour il y a ", floor(age_days), " jours). Il est recommandé de les rafraîchir.")
  }
  
  message("Vérification des données : ", message_text)
  
  return(
    list(
      is_fresh = is_fresh,
      last_modified = as.Date(last_modified_time),
      age_days = floor(age_days),
      message = message_text
    )
  )
}






