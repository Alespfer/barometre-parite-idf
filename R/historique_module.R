# ==============================================================================
# HISTORIQUE & ÉVOLUTIONS
#
# Ce module fournit une vue temporelle des scores Egapro, permettant
# de comparer l'évolution de plusieurs territoires sur plusieurs années.
# ==============================================================================


# ==============================================================================
# PARTIE UI 
# ==============================================================================
historique_ui <- function(id, df) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    # --- Colonne des filtres ---
    shiny::column(
      width = 3,
      bslib::card(
        bslib::card_header("Filtres de l'analyse"),
        bslib::card_body(
          padding = "10px",
          bslib::accordion(
            open = TRUE,
            bslib::accordion_panel(
              title = "Options de Filtrage",
              icon = shiny::icon("filter"),
              
              shiny::selectInput(ns("filtre_taille_historique"), "Taille d'entreprise :", 
                                 choices = c("Toutes les tailles", unique(df$tranche_effectifs))),
              
              shiny::selectInput(ns("niveau_geo_historique"), "Niveau d'analyse :", 
                                 choices = c("Départements", "Intercommunalités (EPCI)", "Zones d'emploi"),
                                 selected = "Départements"),
              
              shiny::uiOutput(ns("territory_selector_ui"))
            )
          )
        )
      )
    ),
    
    # --- Colonne des visualisations ---
    shiny::column(
      width = 9,
      shiny::uiOutput(ns("kpi_historique_ui")),
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     "Évolution Comparée des Scores Egapro",
                     color_switch_ui(ns("color_switch_historique"))
          )
        ),
        bslib::card_body(
          plotly::plotlyOutput(ns("plot_historique_interactif"), height = "450px")
        )
      ),
      bslib::card(
        bslib::card_header("Données Détaillées"),
        bslib::card_body(
          DT::dataTableOutput(ns("table_historique"))
        )
      )
    )
  )
}

# ==============================================================================
# PARTIE SERVER 
# ==============================================================================
historique_server <- function(id, master_df_historique, palette_accessible) {
  shiny::moduleServer(id, function(input, output, session) {
    
    
    # Ce bloc `renderUI` est ré-exécuté à chaque fois que `input$niveau_geo_historique` change.
    # Cela permet de gérer les filtres en cascade
    output$territory_selector_ui <- shiny::renderUI({
      ns <- session$ns
      shiny::req(input$niveau_geo_historique)
      
      # Sélectionne la bonne colonne du dataframe en fonction du niveau géographique choisi.
      col_name <- switch(input$niveau_geo_historique,
                         "Départements" = "dep_name",
                         "Intercommunalités (EPCI)" = "epci_name",
                         "Zones d'emploi" = "ze_name"
      )
      
      if (!col_name %in% names(master_df_historique)) return(NULL)
      
      # Extrait la liste des territoires uniques pour peupler les choix.
      choix <- sort(unique(na.omit(master_df_historique[[col_name]])))
      
      # Sélection par défaut
      selected_choix <- if (input$niveau_geo_historique == "Départements") {
        intersect(c("Paris", "Hauts-de-Seine"), choix)
      } else {
        head(choix, 2)
      }
      
      shiny::selectizeInput(ns("filtre_territoire_historique"), "Choisir un ou plusieurs territoire(s) :", 
                            choices = choix, 
                            selected = selected_choix,
                            multiple = TRUE)
    })
    
    # Calcule les scores moyens annuels pour les territoires sélectionnés.
    data_historique_agg <- reactive({
      req(input$filtre_territoire_historique, input$niveau_geo_historique)
      
      col_name <- switch(input$niveau_geo_historique,
                         "Départements" = "dep_name",
                         "Intercommunalités (EPCI)" = "epci_name",
                         "Zones d'emploi" = "ze_name"
      )
      
      df_base <- master_df_historique
      if (input$filtre_taille_historique != "Toutes les tailles") {
        df_base <- filter(df_base, tranche_effectifs == input$filtre_taille_historique)
      }
      
      df_base %>% 
        filter(.data[[col_name]] %in% input$filtre_territoire_historique) %>%
        group_by(annee, territoire = .data[[col_name]]) %>% 
        # Le calcul du score moyen est bien pondéré par la taille des entreprises.
        summarise(score_moyen = weighted.mean(index, poids, na.rm = TRUE), .groups = "drop") %>%
        arrange(territoire, annee)
    })
    
    # Calcule et affiche le KPI de progression moyenne entre la première et la dernière année.
    output$kpi_historique_ui <- renderUI({
      df <- data_historique_agg(); req(nrow(df) > 1)
      annee_debut <- min(df$annee); annee_fin <- max(df$annee)
      
      progression_df <- df %>% 
        group_by(territoire) %>% 
        summarise(
          score_start = score_moyen[annee == annee_debut], 
          score_end = score_moyen[annee == annee_fin], .groups = "drop"
        ) %>% 
        filter(!is.na(score_start) & !is.na(score_end) & length(score_start) > 0 & length(score_end) > 0)
      
      if (nrow(progression_df) == 0) return(bslib::value_box(title = "Progression Moyenne", value = "Données insuffisantes", theme = "secondary"))
      
      mean_progression <- mean(progression_df$score_end - progression_df$score_start, na.rm = TRUE)
      kpi_theme <- if (mean_progression > 0) "success" else if (mean_progression < 0) "danger" else "secondary"
      kpi_icon <- if (mean_progression > 0) icon("arrow-trend-up") else if (mean_progression < 0) icon("arrow-trend-down") else icon("minus")
      
      bslib::value_box(title = paste("Progression Moyenne (", annee_debut, "-", annee_fin, ")"), value = sprintf("%+.1f points", mean_progression), showcase = kpi_icon, theme = kpi_theme)
    })
    
    output$plot_historique_interactif <- plotly::renderPlotly({
      df_plot <- data_historique_agg()
      shiny::req(nrow(df_plot) > 0)
      
      # Logique pour basculer entre la palette par défaut et la palette accessible.
      palette_a_utiliser <- if (isTRUE(input$color_switch_historique)) {
        palette_accessible
      } else {
        rep(c("#7B61FF","#20C997", "#495057", "#FD7E14", "#FFC107"), length.out = length(input$filtre_territoire_historique))
      }
      
      # On initialise le graphique en étant explicite sur le package `plotly`.
      p <- plotly::plot_ly() %>%
        # On utilise plotly::layout pour éviter tout conflit.
        plotly::layout(
          xaxis = list(title = "Année"),
          yaxis = list(title = "Score Egapro moyen pondéré"),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
        ) %>%
        # C'EST LA CORRECTION PRINCIPALE : On utilise plotly::config pour être sûr
        # d'appeler la bonne fonction et ne pas transformer notre graphique en requête web.
        plotly::config(displayModeBar = FALSE)
      
      # La boucle `for` reste la même, mais on préfixe aussi `add_trace` par sécurité.
      for (i in seq_along(input$filtre_territoire_historique)) {
        territoire_actuel <- input$filtre_territoire_historique[i]
        couleur_actuelle <- palette_a_utiliser[i]
        
        data_pour_trace <- df_plot %>%
          filter(territoire == territoire_actuel) %>%
          mutate(tooltip_text = paste0("<b>Territoire:</b> ", territoire,
                                       "<br><b>Année:</b> ", annee,
                                       "<br><b>Score:</b> ", round(score_moyen, 2)))
        
        p <- p %>% plotly::add_trace(
          data = data_pour_trace,
          x = ~annee,
          y = ~score_moyen,
          name = territoire_actuel,
          type = 'scatter',
          mode = 'lines+markers',
          color = I(couleur_actuelle),
          text = ~tooltip_text,
          hoverinfo = 'text'
        )
      }
      
      # Retour gu graphique final.
      p
    })
    
    # Rendu du tableau de données.
    output$table_historique <- renderDataTable({
      df <- data_historique_agg(); if (nrow(df) == 0) return(datatable(data.frame(Message = "Aucune donnée disponible"), options = list(dom = 't')))
      # Utilisation de `pivot_wider` pour transformer les données du format "long" au format "large"
      df_table <- df %>% 
        select(Territoire = territoire, Année = annee, `Score Moyen` = score_moyen) %>% 
        pivot_wider(names_from = Année, values_from = `Score Moyen`)
      
      df_table %>%
        # Active les boutons d'export (Copier, CSV, Excel).
        datatable(rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Bfrtip', ordering = FALSE, buttons = list('copy', 'csv', 'excel'), language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/fr-FR.json')), caption = NULL) %>%
        formatRound(columns = which(sapply(df_table, is.numeric)), digits = 2)
    })
    
  })
}