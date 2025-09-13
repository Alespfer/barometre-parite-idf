# ==============================================================================
# CARTE & TERRITOIRES
#
# Ce fichier définit l'interface (UI) et la logique (Server) de l'onglet
# principal de l'application, dédié à l'exploration cartographique.
# ==============================================================================

# ==============================================================================
# PARTIE UI
# ==============================================================================
carte_ui <- function(id, df) {
  # Création d'un "namespace" pour éviter les conflits d'ID avec les autres modules.
  ns <- shiny::NS(id)
  shiny::fluidRow(
    # --- Colonne de gauche : Panneau de contrôle des filtres ---
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     "Filtres de la carte",
                     bslib::tooltip(
                       shiny::actionButton(ns("show_download_modal"), label = NULL, icon = shiny::icon("download"), class = "btn-sm"),
                       "Exporter les données filtrées"
                     )
          )
        ),
        bslib::card_body(
          padding = "10px",
          bslib::accordion(
            open = c("Territoires", "Entreprises"),
            bslib::accordion_panel(
              title = "Filtres Territoriaux", icon = shiny::icon("location-dot"), value = "Territoires",
              shiny::selectInput(
                inputId = ns("filtre_annee_carte"),
                label = "Année de l'Index :",
                choices = sort(unique(df$annee), decreasing = TRUE),
                selected = max(df$annee),
                width = "100%"
              ),              
              shiny::selectInput(ns("niveau_geo_carte"), "Niveau d'analyse :", 
                                 choices = c("Départements", "Intercommunalités (EPCI)", "Zones d'emploi"),
                                 selected = "Départements"),              
              shiny::selectizeInput(ns("selection_territoires"), "Sélectionner des territoires :", choices = NULL, multiple = TRUE, options = list(placeholder = 'Tous les territoires par défaut'))
            ),
            bslib::accordion_panel(
              title = "Filtres sur les Entreprises", icon = shiny::icon("building"), value = "Entreprises",
              shiny::selectizeInput(ns("filtre_secteur_carte"), "Secteur d'activité :", choices = c("Tous les secteurs", sort(unique(na.omit(df$secteur_activite)))), multiple = TRUE, selected = "Tous les secteurs"),
              shiny::div(class = "d-flex align-items-center",
                         shiny::selectInput(ns("filtre_taille_carte"), "Taille d'entreprise :", choices = c("Toutes les tailles", unique(df$tranche_effectifs)), width = "90%"),
                         bslib::tooltip(shiny::icon("info-circle"), "L'Index est obligatoire pour les entreprises de 50 salariés et plus.", placement = "right")
              ),
              shiny::sliderInput(ns("filtre_score_carte"), "Filtrer par Score Egapro :", min = 0, max = 100, value = c(0, 100))
            ),
            bslib::accordion_panel(
              title = "Recherche par Entreprise", icon = shiny::icon("search"), value = "Recherche",
              shiny::textInput(ns("search_siren"), "Entrer un n° SIREN :"),
              shiny::actionButton(ns("search_button"), "Rechercher", icon = icon("arrow-right"), class = "btn-primary w-100")
            ),
            bslib::accordion_panel(
              title = "Options d'Affichage", icon = shiny::icon("eye"), value = "Affichage",
              shiny::radioButtons(ns("map_basemap"), "Choisir le fond de carte :", choices = list("Clair" = "CartoDB.Positron", "Détaillé (Satellite)" = "Esri.WorldImagery", "Plan de rues" = "OpenStreetMap.Mapnik"), selected = "CartoDB.Positron")
            )
          )
        )
      )
    ),
    # --- Colonne de droite : Visualisations (KPIs et Carte) ---
    shiny::column(
      width = 9,
      shiny::uiOutput(ns("kpi_carte_ui")),
      # La carte Leaflet a été intégrée dans une carte bslib pour une intégration visuelle propre.
      bslib::card(
        bslib::card_body(padding = 0, leaflet::leafletOutput(ns("map"), height = "calc(100vh - 250px)"))
      )
    )
  )
}


# ==============================================================================
# PARTIE SERVER 
# ==============================================================================
carte_server <- function(id, master_df_historique, map_epci, map_dep, map_ze, palette_accessible, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # --- Gestion de la communication entre modules (avec Analyse sectorielle) ---
    observeEvent(shared_state$selected_sector, {
      req(shared_state$selected_sector)
      updateSelectizeInput(session, "filtre_secteur_carte", selected = shared_state$selected_sector)
      shared_state$selected_sector <- NULL
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    

    # Observateur pour le re-zoomage de la carte.
    observeEvent(shared_state$trigger_zoom, {
      req(shared_state$trigger_zoom)
      
      leafletProxy("map") %>%
        setView(lng = 2.55, lat = 48.80, zoom = 9)
      
      # On réinitialise le déclencheur.
      shared_state$trigger_zoom <- NULL
      
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    
    # --- Logique réactive du module ---
    
    # Met à jour la liste des territoires sélectionnables lorsque l'utilisateur change le niveau d'analyse (Départements, EPCI, etc.).
    observeEvent(input$niveau_geo_carte, {
      col_name <- switch(input$niveau_geo_carte,
                         "Départements" = "dep_name",
                         "Intercommunalités (EPCI)" = "epci_name",
                         "Zones d'emploi" = "ze_name")
      req(col_name %in% names(master_df_historique))
      choix <- sort(unique(na.omit(master_df_historique[[col_name]])))
      updateSelectizeInput(session, "selection_territoires", choices = choix, selected = NULL)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    # Filtre de base sur les entreprises
    data_filtree_entreprise <- reactive({
      master_df_historique %>%
        filter(
          annee == input$filtre_annee_carte,
          if (input$filtre_taille_carte != "Toutes les tailles") tranche_effectifs == input$filtre_taille_carte else TRUE,
          if (!is.null(input$filtre_secteur_carte) && !("Tous les secteurs" %in% input$filtre_secteur_carte)) secteur_activite %in% input$filtre_secteur_carte else TRUE,
          index >= input$filtre_score_carte[1] & index <= input$filtre_score_carte[2]
        )
    })
    
    # Données des polygones (territoires) à afficher
    # Il agrège les données d'entreprises filtrées pour calculer un score moyen par territoire.
    
    map_data_filtrée <- reactive({
      req(input$niveau_geo_carte)
      switch(input$niveau_geo_carte,
             "Départements" = { group_var <- "dep_name"; map_shape <- map_dep },
             "Intercommunalités (EPCI)" = { group_var <- "epci_name"; map_shape <- map_epci },
             "Zones d'emploi" = { group_var <- "ze_name"; map_shape <- map_ze })
      
      # Calcule le score moyen pondéré par territoire.
      agg_data <- data_filtree_entreprise() %>%
        filter(!is.na(.data[[group_var]])) %>%
        group_by(name_col = .data[[group_var]]) %>%
        summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop")
      
      # Jointure entre les géométries et les scores calculés.
      df_map <- left_join(map_shape, agg_data, by = setNames("name_col", group_var))
      
      # Applique le filtre de sélection de territoires.
      if (length(input$selection_territoires) > 0) {
        df_map <- filter(df_map, .data[[group_var]] %in% input$selection_territoires)
      }
      return(df_map)
    })
    
    # Données des entreprises à afficher
    points_filtres_carte <- reactive({
      df <- data_filtree_entreprise()
      if (length(input$selection_territoires) > 0) {
        col_geo <- switch(input$niveau_geo_carte,
                          "Départements" = "dep_name",
                          "Intercommunalités (EPCI)" = "epci_name",
                          "Zones d'emploi" = "ze_name")
        if (col_geo %in% names(df)) {
          df <- filter(df, .data[[col_geo]] %in% input$selection_territoires)
        }
      }
      return(df)
    })
    
    

    # Affiche les indicateurs clés (KPIs) au-dessus de la carte.
    output$kpi_carte_ui <- renderUI({
      df_filtre <- points_filtres_carte()
      req(df_filtre)
      score_moyen_pondere <- if (nrow(df_filtre) > 0) weighted.mean(df_filtre$index, df_filtre$poids, na.rm = TRUE) else NA
      fluidRow(
        column(6, bslib::value_box(title = "Entreprises dans la sélection", value = scales::comma(nrow(df_filtre)), showcase = icon("building"), theme = "secondary")),
        column(6, bslib::value_box(title = "Score moyen pondéré", value = if (is.na(score_moyen_pondere)) "N/A" else round(score_moyen_pondere, 1), showcase = icon("balance-scale"), theme = "primary"))
      )
    })
    
    # Initialise la carte Leaflet une seule fois au démarrage de l'application.
    output$map <- renderLeaflet(leaflet() %>% setView(2.55, 48.80, 9) %>% addProviderTiles("CartoDB.Positron"))
    
    # Met à jour le fond de carte (tiles) lorsque l'utilisateur le change.
    observeEvent(input$map_basemap, {
      leafletProxy("map") %>% addProviderTiles(input$map_basemap)
    })
    
    # Observateur principal pour redessiner les couches de la carte avec les données filtrées
    observe({
      df_polygones <- map_data_filtrée()
      req(df_polygones, nrow(df_polygones) > 0)
      df_points <- points_filtres_carte()
      
      pal_points <- colorNumeric(palette = RColorBrewer::brewer.pal(9, "Blues"), domain = c(0, 100))
      pal_territoires <- colorNumeric(palette = RColorBrewer::brewer.pal(9, "YlGn"), domain = c(75, 100), na.color = "#F0F0F0")
      
      proxy <- leafletProxy("map", data = df_polygones) %>%
        clearShapes() %>% clearMarkers() %>% clearControls()
      

      group_var_name <- names(df_polygones)[grepl("_name", names(df_polygones))][1]
      popup_polygones <- ifelse(
        is.na(df_polygones$score_moyen),
        paste0("<strong>", df_polygones[[group_var_name]], "</strong><br>Aucune donnée disponible."),
        paste0("<strong>", df_polygones[[group_var_name]], "</strong><br>Score moyen : ", round(df_polygones$score_moyen, 1))
      )
      
      # Ajout des polygones et de leur légende.
      proxy %>%
        addPolygons(
          fillColor = ~pal_territoires(score_moyen), 
          fillOpacity = 0.7, weight = 1.5, color = "white", 
          group = "Vue Territoriale", 
          popup = lapply(popup_polygones, HTML)
        ) %>%
        addLegend(pal = pal_territoires, values = ~score_moyen, title = "Score Moyen<br>(Territoires)", position = "bottomright")
      
      # Ajout des points des entreprises.
      if (nrow(df_points) > 0) {
        popup_points <- apply(df_points, 1, generate_company_popup)
        proxy %>% addCircleMarkers(
          data = df_points, lng = ~longitude, lat = ~latitude,
          popup = lapply(popup_points, HTML), radius = 5, color = "white", weight = 1,
          fillColor = ~pal_points(index), fillOpacity = 0.8,
          group = "Vue Entreprises"
        )
      }
      
      if (nrow(df_points) > 0 || any(!is.na(df_points$index))) {
        proxy %>% addLegend(pal = pal_points, values = df_points$index, title = "Score Egapro<br>(Entreprises)", position = "bottomleft")
      }
      
      proxy %>% addLayersControl(
        baseGroups = c("Vue Territoriale"),
        overlayGroups = c("Vue Entreprises"), 
        options = layersControlOptions(collapsed = FALSE, position = "topright")
      )
    })
    
    # --- Téléchargement et recherche ---
    observeEvent(input$show_download_modal, {
      showModal(modalDialog(
        title = "Exporter les Données Filtrées",
        p("Choisissez le format pour télécharger la liste des entreprises actuellement visibles sur la carte."),
        footer = tagList(
          modalButton("Annuler"),
          downloadButton(session$ns("download_csv"), "Télécharger (CSV)", class = "btn-primary"),
          downloadButton(session$ns("download_excel"), "Télécharger (Excel)", class = "btn-success")
        )
      ))
    })
    
    shiny::observeEvent(input$search_button, {
      siren_a_chercher <- trimws(input$search_siren)
      shiny::req(siren_a_chercher)
      
      entreprise_trouvee <- master_df_historique %>%
        dplyr::filter(siren == siren_a_chercher) %>%
        dplyr::arrange(desc(annee)) %>%
        dplyr::slice(1)
      
      if (nrow(entreprise_trouvee) > 0) {
        popup_content <- generate_company_popup(entreprise_trouvee)
        leaflet::leafletProxy("map", session) %>%
          leaflet::setView(lng = entreprise_trouvee$longitude, lat = entreprise_trouvee$latitude, zoom = 15) %>%
          leaflet::clearPopups() %>%
          leaflet::addPopups(
            lng = entreprise_trouvee$longitude,
            lat = entreprise_trouvee$latitude,
            popup = popup_content
          )
      } else {
        shiny::showNotification(
          paste("Le SIREN", siren_a_chercher, "n'a pas été trouvé dans la base de données Egapro."),
          type = "warning",
          duration = 5
        )
      }
    })
    
    # Logique pour le téléchargement des données au format CSV.
    output$download_csv <- downloadHandler(
      filename = function() { paste0("donnees_filtrees_carte_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(points_filtres_carte(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    
    # Logique pour le téléchargement des données au format Excel.
    output$download_excel <- downloadHandler(
      filename = function() { paste0("donnees_filtrees_carte_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(points_filtres_carte(), file) }
    )
  })
}