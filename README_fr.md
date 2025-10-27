<!-- Language Navigation -->
<div align="right">
  <a href="./README.md">English</a> | <b><a href="./README_fr.md">Français</a></b> | <a href="./README_es.md">Español</a>
</div>

# Baromètre de la Parité en Entreprise – Île-de-France

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
![Language](https://img.shields.io/badge/Langage-R-blue)
![Framework](https://img.shields.io/badge/Framework-Shiny-hotpink)
[![Status](https://img.shields.io/badge/Statut-Actif-success)](https://alespfer.shinyapps.io/barometre-parite-idf/)

Une application web interactive développée avec **R** et **Shiny** pour analyser et visualiser les données de l'égalité professionnelle femmes-hommes en **Île-de-France**. Ce baromètre exploite les données publiques de l'Index Egapro pour offrir des analyses territoriales et sectorielles fines, destinées aux décideurs publics, agences de développement économique et chercheurs.

**[➡️ Accéder à l'application en ligne](https://alespfer.shinyapps.io/barometre-parite-idf/)**

![Vue principale du tableau de bord](img/main-dashboard-view.png)

## Table des matières

- [À propos du projet](#à-propos-du-projet)
- [Fonctionnalités principales](#fonctionnalités-principales)
- [Pipeline de données et automatisation](#pipeline-de-données-et-automatisation)
- [Technologies utilisées](#technologies-utilisées)
- [Démarrage rapide](#démarrage-rapide)
  - [Prérequis](#prérequis)
  - [Installation](#installation)
- [Utilisation](#utilisation)
- [Licence](#licence)
- [Contact](#contact)

## À propos du projet

Depuis 2018, les entreprises françaises de plus de 50 salariés ont l'obligation de calculer et de publier leur **Index de l'égalité professionnelle (Egapro)**. Noté sur 100 points, cet index est un instrument clé pour mesurer et réduire les inégalités en entreprise. Il se compose de cinq indicateurs :
- Écart de rémunération (40 points)
- Écart de taux d'augmentations individuelles (20-35 points)
- Écart de taux de promotions (15 points)
- Augmentations au retour de congé maternité (15 points)
- Parité parmi les 10 plus hautes rémunérations (10 points)

Bien que ces données soient publiques, leur analyse se limite souvent à l'échelle nationale. Ce projet a été conçu pour offrir une **perspective territoriale** fine en Île-de-France, en permettant des analyses à l'échelle des départements, des intercommunalités (EPCI) et des zones d'emploi.

## Fonctionnalités principales

Le baromètre est organisé en plusieurs modules d'analyse :

*   🗺️ **Carte & Territoires :** Une carte interactive pour visualiser les scores Egapro moyens selon différentes mailles territoriales. L'utilisateur peut filtrer par année, taille d'entreprise, secteur d'activité, et rechercher une entreprise par son numéro SIREN.
*   📊 **Analyse Sectorielle :** Un graphique en "lollipops" pour identifier les secteurs d'activité les plus et les moins performants. Ce module est interactif : un clic sur un secteur filtre la carte principale pour une exploration approfondie.
*   🔍 **Analyse des Indicateurs :** Un module pour analyser en détail la performance sur chacun des cinq indicateurs qui composent le score Egapro global.
*   📈 **Analyse Socio-démographique :** Un outil exploratoire pour visualiser les corrélations potentielles entre la performance des entreprises et le contexte socio-économique de leur zone d'emploi (ex: taux d'activité des femmes, part de femmes cadres).
*   📉 **Historique & Évolutions :** Un module d'analyse temporelle pour suivre et comparer l'évolution des scores Egapro sur plusieurs années pour les territoires sélectionnés.

## Pipeline de données et automatisation

Pour garantir des données fiables et à jour, le projet intègre un pipeline de traitement entièrement automatisé grâce à **GitHub Actions**.



1.  **Extraction :** Un workflow planifié (`.github/workflows/data-pipeline.yml`) s'exécute chaque mois. Il collecte les dernières données depuis plusieurs API publiques :
    *   **Index Egapro :** `data.gouv.fr`
    *   **Base SIRENE (infos entreprises) :** `Opendatasoft`
    *   **Données du Recensement (socio-démographie) :** INSEE (fichiers locaux)
2.  **Transformation :** Le script `run_pipeline.R` nettoie, standardise, enrichit et fusionne ces jeux de données. Les étapes clés incluent la géolocalisation des sièges sociaux, la conversion des codes NAF en secteurs d'activité, et le calcul d'indicateurs socio-démographiques.
3.  **Chargement pour Shiny :** Les données traitées sont sauvegardées au format `.RDS` optimisé dans le dossier `data_shiny/`. L'application Shiny lit directement ces fichiers, garantissant des temps de chargement rapides et une grande réactivité.
4.  **Déploiement Continu :** Un second workflow (`.github/workflows/deploy-shinyapp.yml`) redéploie automatiquement l'application sur `shinyapps.io` à chaque modification poussée sur la branche `main`, y compris les mises à jour automatiques des données.

## Technologies utilisées

Ce projet s'appuie sur l'écosystème moderne de R pour l'analyse de données et la science des données spatiales :

*   **Coeur :** [R](https://www.r-project.org/), [Shiny](https://shiny.posit.co/)
*   **UI/UX :** [{bslib}](https://rstudio.github.io/bslib/) pour le thème Bootstrap 5, [{plotly}](https://plotly.com/r/) pour les graphiques interactifs
*   **Manipulation de données :** [{dplyr}](https://dplyr.tidyverse.org/), [{tidyr}](https://tidyr.tidyverse.org/)
*   **Analyse spatiale & Cartographie :** [{sf}](https://r-spatial.github.io/sf/), [{leaflet}](https://rstudio.github.io/leaflet/)
*   **Reproductibilité :** [{renv}](https://rstudio.github.io/renv/) pour la gestion des dépendances

## Démarrage rapide

Pour exécuter ce projet localement, suivez ces étapes.

### Prérequis

*   R (version 4.2 ou supérieure)
*   RStudio est recommandé pour une meilleure expérience.

### Installation

1.  Clonez le dépôt :
    ```bash
    git clone https://github.com/Alespfer/barometre-parite-idf.git
    ```
2.  Naviguez vers le dossier du projet :
    ```bash
    cd barometre-parite-idf
    ```
3.  Ouvrez le fichier `egapro.Rproj` dans RStudio.
4.  Le package `{renv}` restaurera automatiquement les dépendances du projet à partir du fichier `renv.lock`. Si une notification apparaît, tapez `renv::restore()` dans la console et confirmez. Cela installera tous les packages requis dans une bibliothèque isolée propre au projet.
5.  Si vous souhaitez exécuter le pipeline de données vous-même, vous devrez télécharger les fichiers du recensement de l'INSEE spécifiés dans `methodologie_preparation_donnees.Rmd` et les placer dans le dossier `data/raw/`. Sinon, les données pré-traitées sont déjà disponibles dans `data_shiny/`.

## Utilisation

Une fois les dépendances installées, vous pouvez lancer l'application en ouvrant le fichier `app.R` et en cliquant sur "Run App" dans RStudio, ou en exécutant la commande suivante dans la console R :

```R
shiny::runApp('app.R')
```

## Licence

Ce projet est distribué sous la Licence MIT. Voir le fichier `LICENSE` pour plus d'informations.

## Contact

Alberto Esperon - [LinkedIn](https://www.linkedin.com/in/alberto-espfer) - [Profil GitHub](https://github.com/Alespfer)

Lien du projet : [https://github.com/Alespfer/barometre-parite-idf](https://github.com/Alespfer/barometre-parite-idf)
