# ==============================================================================
# ANALYSE DES INDICATEURS
#
# Ce fichier définit l'interface (UI) et la logique (Server) de l'onglet
# dédié à l'exploration de chaque indicateur composant l'Index Egapro.
# ==============================================================================


# ==============================================================================
# PARTIE UI
# ==============================================================================
indicateurs_ui <- function(id, df) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    # --- Colonne de gauche : Panneau de contrôle des filtres ---
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     "Filtres de l'analyse",
                     bslib::tooltip(
                       shiny::actionButton(ns("show_download_modal"), 
                                           label = NULL, 
                                           icon = shiny::icon("download"), 
                                           class = "btn-sm"),
                       "Exporter les données agrégées"
                     )
          )
        ),
        bslib::card_body(
          padding = "10px",
          bslib::accordion(
            open = TRUE,
            bslib::accordion_panel(
              title = "Options de Filtrage",
              icon = shiny::icon("filter"),
              shiny::selectInput(
                inputId = ns("filtre_annee_indicateur"),
                label = "Année :",
                choices = sort(unique(df$annee), decreasing = TRUE),
                selected = max(df$annee),
                width = "100%"
              ),
              shiny::selectInput(
                inputId = ns("select_indicateur"),
                label = "Choisir un indicateur à analyser :",
                choices = indicateur_labels,
                selected = "note_remuneration"
              ),
              shiny::uiOutput(ns("indicateur_description_ui")),
              shiny::selectInput(
                inputId = ns("niveau_geo_indicateur"),
                label = "Niveau d'analyse territorial :",
                choices = c("Départements", "Intercommunalités (EPCI)", "Zones d'emploi"),
                selected = "Départements"
              )
            )
          )
        )
      )
    ),
    
    shiny::column(
      # --- Colonne de droite : Visualisations (Carte et Graphique) ---
      width = 9,
      bslib::card(
        bslib::card_header("Performance Territoriale sur l'Indicateur Sélectionné"),
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map_indicateur"), height = "450px")
        )
      ),
      bslib::card(
        bslib::card_header("Performance Sectorielle sur l'Indicateur Sélectionné"),
        bslib::card_body(
          plotly::plotlyOutput(ns("plot_sectoriel_indicateur"), height = "450px")
        )
      )
    )
  )
}


# ==============================================================================
# PARTIE SERVER 
# ==============================================================================
indicateurs_server <- function(id, master_df_historique, map_epci, map_dep, map_ze) {
  shiny::moduleServer(id, function(input, output, session) {
    
    
    # --- Logique réactive du module ---
    
    # Filtre les données sur l'année et l'indicateur sélectionné.
    data_filtree <- shiny::reactive({
      shiny::req(input$select_indicateur)
      master_df_historique %>%
        dplyr::filter(annee == input$filtre_annee_indicateur, !is.na(.data[[input$select_indicateur]]))
    })
    
    
    # Affiche la description textuelle de l'indicateur choisi sous le menu déroulant.
    output$indicateur_description_ui <- shiny::renderUI({
      shiny::req(input$select_indicateur)
      description <- indicateur_descriptions[[input$select_indicateur]]
      shiny::div(
        style = "font-size: 0.85em; background-color: #f8f9fa; border-radius: 5px; padding: 10px; margin-top: -10px; margin-bottom: 15px;",
        shiny::HTML(description)
      )
    })
    
    
    # Agrège les données par territoire pour la carte.
    data_agg_territoriale <- shiny::reactive({
      group_var <- switch(input$niveau_geo_indicateur,
                          "Départements" = "dep_name",
                          "Intercommunalités (EPCI)" = "epci_name",
                          "Zones d'emploi" = "ze_name")
      
      shiny::req(group_var)
      
      data_filtree() %>%
        dplyr::filter(!is.na(.data[[group_var]])) %>%
        dplyr::group_by(Territoire = .data[[group_var]]) %>%
        dplyr::summarise(
          `Note Moyenne` = mean(.data[[input$select_indicateur]], na.rm = TRUE),
          `Nombre d'entreprises` = dplyr::n(),
          .groups = "drop"
        )
    })
    
    
    # Agrège les données par secteur pour le graphique.
    data_agg_sectorielle <- shiny::reactive({
      data_filtree() %>%
        dplyr::group_by(Secteur = secteur_activite) %>%
        dplyr::summarise(
          `Note Moyenne` = mean(.data[[input$select_indicateur]], na.rm = TRUE),
          `Nombre d'entreprises` = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::filter(`Nombre d'entreprises` >= 10)
    })
    
    
    # --- Visualisations ---
    
    # Rendu de la carte choroplèthe.
    output$map_indicateur <- leaflet::renderLeaflet({
      agg_data <- data_agg_territoriale()
      
      shiny::validate(shiny::need(nrow(agg_data) > 0, "Aucune donnée territoriale à afficher pour cette sélection."))
      
      if (input$niveau_geo_indicateur == "Départements") {
        map_shape <- map_dep
        group_var <- "dep_name"
      } else if (input$niveau_geo_indicateur == "Intercommunalités (EPCI)") {
        map_shape <- map_epci
        group_var <- "epci_name"
      } else {
        map_shape <- map_ze
        group_var <- "ze_name"
      }
      
      map_final <- map_shape %>%
        dplyr::left_join(agg_data, by = setNames("Territoire", group_var))
      
      
      # Extrait dynamiquement la note maximale de l'indicateur (par exemple : 40 pour la rémunération).
      indicateur_nom <- names(indicateur_labels)[indicateur_labels == input$select_indicateur]
      max_points <- as.numeric(stringr::str_extract(indicateur_nom, "\\d+"))
      
      pal <- leaflet::colorNumeric("YlGnBu", domain = c(0, max_points), na.color = "#E0E0E0")
      
      # Prépare le contenu des popups.
      popup_polygones <- ifelse(
        is.na(map_final$`Note Moyenne`),
        paste0("<strong>", map_final[[group_var]], "</strong><br>Aucune donnée disponible pour cette sélection."),
        paste0(
          "<strong>", map_final[[group_var]], "</strong><br>",
          "Note moyenne : ", round(map_final$`Note Moyenne`, 1), "/", max_points, "<br>",
          "Nombre d'entreprises : ", map_final$`Nombre d'entreprises`
        )
      )
      
      
      # Dessine la carte.
      leaflet::leaflet(data = map_final) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::fitBounds(lng1 = 2.0, lat1 = 48.6, lng2 = 2.7, lat2 = 49.1) %>%
        leaflet::addPolygons(
          fillColor = ~pal(`Note Moyenne`), 
          fillOpacity = 0.8, 
          weight = 1.5, 
          color = "white", 
          popup = lapply(popup_polygones, shiny::HTML)
        ) %>%
        leaflet::addLegend(pal = pal, values = ~`Note Moyenne`, title = "Note Moyenne", position = "bottomright")
    })
    

    # Rendu du graphique à barres sectoriel.
        output$plot_sectoriel_indicateur <- plotly::renderPlotly({
      
          summary_secteur <- data_agg_sectorielle() %>%
        dplyr::arrange(`Note Moyenne`) %>%
        dplyr::mutate(SecteurCourt = secteur_labels_courts[Secteur])
      
      shiny::validate(shiny::need(nrow(summary_secteur) > 0, 
                                  "Pas assez de données sectorielles à afficher (seuil de 10 entreprises min. par secteur)."))
      
      indicateur_nom <- names(indicateur_labels)[indicateur_labels == input$select_indicateur]
      max_points <- as.numeric(stringr::str_extract(indicateur_nom, "\\d+"))
      
      # Création du graphique ggplot
      g <- ggplot2::ggplot(
        summary_secteur,
        ggplot2::aes(
          x = `Note Moyenne`,
          y = reorder(SecteurCourt, `Note Moyenne`),
          # Liaison de la couleur de remplissage à la note pour créer le dégradé
          fill = `Note Moyenne`,
          # L'infobulle affiche le nom complet du secteur, alors que les labels affichent les noms abrégés. 
          text = sprintf("<b>Secteur :</b> %s<br><b>Note moyenne :</b> %.1f / %d",
                         Secteur, `Note Moyenne`, max_points)
        )
      ) +
        ggplot2::geom_col(width = 0.8) +
        
        # Palette de couleurs accessible.
        ggplot2::scale_fill_viridis_c(option = "cividis", direction = 1) +
        
        ggplot2::labs(x = paste("Note moyenne sur", max_points), y = NULL) +
        ggplot2::coord_cartesian(xlim = c(0, max_points)) +
        ggplot2::theme_minimal(base_family = "Inter") +
        ggplot2::theme(
          legend.position = "none", # On cache la légende de couleur
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_line(linetype = "dashed", color = "gray80"),
          # On s'assure que les labels de l'axe Y sont lisibles
          axis.text.y = ggplot2::element_text(face = "bold", size = 9)
        )
      
      # Conversion en graphique interactif Plotly.
      plotly::ggplotly(g, tooltip = "text") %>%
        plotly::config(displayModeBar = FALSE)
    })
    
        
    # Gère l'affichage de la fenêtre modale pour le téléchargement.
    shiny::observeEvent(input$show_download_modal, {
      shiny::showModal(shiny::modalDialog(
        title = "Exporter les Données Agrégées",
        shiny::p("Choisissez les données agrégées que vous souhaitez télécharger, en fonction des filtres actuels."),
        shiny::h4("Données Territoriales (Carte)"),
        shiny::div(
          shiny::downloadButton(session$ns("download_terr_csv"), "Télécharger (CSV)", class = "btn-primary btn-sm"),
          shiny::downloadButton(session$ns("download_terr_excel"), "Télécharger (Excel)", class = "btn-success btn-sm")
        ),
        shiny::hr(),
        shiny::h4("Données Sectorielles (Graphique)"),
        shiny::div(
          shiny::downloadButton(session$ns("download_sec_csv"), "Télécharger (CSV)", class = "btn-primary btn-sm"),
          shiny::downloadButton(session$ns("download_sec_excel"), "Télécharger (Excel)", class = "btn-success btn-sm")
        ),
        footer = shiny::modalButton("Annuler"),
        easyClose = TRUE
      ))
    })
    
    
    # Gère le téléchargement des données territoriales (CSV et Excel).
    output$download_terr_csv <- shiny::downloadHandler(
      filename = function() { paste0("agregation_territoriale_", input$select_indicateur, "_", Sys.Date(), ".csv") },
      content = function(file) { utils::write.csv(data_agg_territoriale(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    output$download_terr_excel <- shiny::downloadHandler(
      filename = function() { paste0("agregation_territoriale_", input$select_indicateur, "_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(data_agg_territoriale(), file) }
    )
    
    
    # Gère le téléchargement des données sectorielles (CSV et Excel).
    output$download_sec_csv <- shiny::downloadHandler(
      filename = function() { paste0("agregation_sectorielle_", input$select_indicateur, "_", Sys.Date(), ".csv") },
      content = function(file) { utils::write.csv(data_agg_sectorielle(), file, row.names = FALSE, fileEncoding = "UTF-8") }
    )
    output$download_sec_excel <- shiny::downloadHandler(
      filename = function() { paste0("agregation_sectorielle_", input$select_indicateur, "_", Sys.Date(), ".xlsx") },
      content = function(file) { writexl::write_xlsx(data_agg_sectorielle(), file) }
    )
    
  })
}