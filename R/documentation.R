# ==============================================================================
# CONTENU DE L'ONGLET DOCUMENTATION (R/documentation.R)
#
# Ce script définit l'intégralité du contenu de l'onglet "Documentation"
# de l'application Shiny.
# ==============================================================================
documentation_content <- fluidPage(
  style = "padding: 20px; max-width: 900px; margin: auto;",
  
  h3(strong("Mission et périmètre du baromètre")),
  p("Le « Baromètre de la Parité en Entreprise » est un outil interactif d'aide à la décision conçu pour éclairer les politiques publiques en matière d'égalité professionnelle femmes-hommes. Son périmètre couvre l'ensemble de la ", strong("région Île-de-France.")),
  p("Il permet de visualiser, comparer et suivre les performances des entreprises en matière de parité, tout en les replaçant dans le contexte socio-économique et territorial francilien. Il s'adresse principalement aux :"),
  tags$ul(
    tags$li("Décideurs politiques et services des collectivités (Région, Départements, Intercommunalités)"),
    tags$li("Équipes de développement économique et agences d'attractivité"),
    tags$li("Chargés de mission Égalité, Diversité et RSE.")
  ),
  br(),
  
  h3(strong("Principes clés & méthodologie")),
  
  h4("1. Les sources de données"),
  p("La richesse de l'outil repose sur la fusion de trois sources de données publiques de référence :"),
  tags$ul(
    tags$li(
      strong("Index Egapro (Ministère du Travail) :"), 
      " Le cœur de l'analyse, via les données de l'", tags$a(href = "https://www.data.gouv.fr/fr/datasets/index-egalite-professionnelle-f-h-des-entreprises-de-50-salaries-ou-plus/", "Index de l'Égalité Professionnelle", target = "_blank"), ". Cet index (calculé sur 100 points) est obligatoire pour les entreprises de plus de 50 salariés et agrège cinq indicateurs détaillés :",
      tags$ul(
        style = "margin-top: 10px;",
        tags$li("Écarts de rémunération (noté sur 40 points)"),
        tags$li("Écarts dans les taux d'augmentations individuelles (sur 20 ou 35 points, en fonction de la taille de l'entreprise)"),
        tags$li("Écarts dans les taux de promotions (sur 15 points)"),
        tags$li("Pourcentage de salariées augmentées au retour de leur congé maternité (sur 15 points)"),
        tags$li("Nombre de femmes parmi les 10 plus hautes rémunérations (sur 10 points)")
      )
    ),
    tags$li(strong("Base SIRENE (INSEE) :"), " Utilisée pour localiser précisément les sièges sociaux des entreprises en Île-de-France et connaître leur secteur d'activité (code NAF)."),
    tags$li(strong("Recensement de la Population (INSEE) :"), " Les données détaillées sur la structure de la population et l'activité des résidents sont mobilisées pour construire des indicateurs socio-démographiques de la région.")
  ),
  
  h4("2. Les niveaux d'analyse territoriale"),
  p("Le baromètre propose trois niveaux d'analyse complémentaires :"),
  tags$ul(
    tags$li(strong("Départements :"), " Une vue macroscopique à l'échelle des 8 départements franciliens."),
    tags$li(strong("Intercommunalités (EPCI) :"), " Une analyse fine au niveau des Établissements Publics de Coopération Intercommunale (incluant les EPT de la Métropole du Grand Paris), niveau qui correspond à l'échelle de pilotage de nombreuses politiques économiques locales."),
    tags$li(strong("Zones d'Emploi (ZE) :"), " Une maille fonctionnelle définie par l'INSEE, qui regroupe des communes où la plupart des actifs résident et travaillent. Ce niveau s'avère particulièrement pertinent pour analyser les liens potentiels entre les performances des entreprises et les caractéristiques du marché du travail local.")
  ),
  
  h4("3. Choix Méthodologiques Structurants"),
  tags$ul(
    tags$li(strong("Pondération des scores :"), " Pour que les moyennes reflètent l'impact économique réel, le score de chaque entreprise est pondéré par sa taille (point-milieu des tranches d'effectifs). Pour la tranche '1000 et plus', un poids de 1500 est appliqué, correspondant au point-milieu de la première sous-catégorie disponible ('1000-1999')."),
    tags$li(
      strong("Règles de calcul différenciées selon la taille :"), " Le calcul de l'Index Egapro et le barème de ses indicateurs ne sont pas identiques pour toutes les entreprises : ",
      tags$ul(
        style = "margin-top: 10px;",
        tags$li(
          strong("Pour les entreprises de 50 à 250 salariés :"), " L'Index est calculé sur la base de 4 indicateurs, l'écarts de promotions n'étant pas applicable. De ce fait, l'indicateur des 'écarts de taux d'augmentations individuelles' reçoit un poids plus important, étant noté sur ", strong("35 points.")
        ),
        tags$li(
          strong("Pour les entreprises de plus de 250 salariés :"), " L'Index est calculé sur la base de 5 indicateurs. L'indicateur des 'écarts de promotions' est applicable (noté sur 15 points), et celui des 'écarts de taux d'augmentations' est donc noté sur ", strong("20 points.")
        )
      )
    ),
    br(),
    tags$li(
      strong("Harmonisation d'un indicateur :"), " En raison de cette différence de barème, comparer directement la note de l'indicateur 'augmentations' entre une PME et une grande entreprise n'aurait pas de sens. Pour permettre des analyses justes dans l'onglet ", tags$a(href="#", onclick="document.querySelector('a[data-value=\"Analyse des Indicateurs\"]').click(); return false;", strong("'Analyse des Indicateurs'")), ", la note de cet indicateur a été ", strong("systématiquement ramenée à une base commune de 20 points"), " pour toutes les entreprises."
    )
    ),
  br(),
  
  h3(strong("Guide d'utilisation des onglets")),
  
  p(strong(tags$a(href="#", onclick="document.querySelector('a[data-value=\"Carte & Territoires\"]').click(); return false;", "Carte & Territoires")), " : Explorez la géographie de la parité. Visualisez les scores moyens par département, EPCI ou zone d'emploi, et localisez les entreprises déclarantes."),
  
  p(strong(tags$a(href="#", onclick="document.querySelector('a[data-value=\"Analyse Sectorielle\"]').click(); return false;", "Analyse Sectorielle")), " : Identifiez les secteurs d'activité les plus et les moins performants. Cliquez sur un secteur pour l'analyser directement sur la carte."),
  
  p(strong(tags$a(href="#", onclick="document.querySelector('a[data-value=\"Analyse des Indicateurs\"]').click(); return false;", "Analyse des Indicateurs")), " : Plongez au cœur de l'Index. Cet onglet permet d'analyser les territoires et les secteurs sur chacun des 5 indicateurs composant le score global."),
  
  p(strong(tags$a(href="#", onclick="document.querySelector('a[data-value=\"Socio-démographique\"]').click(); return false;", "Socio-démographique")), " : Croisez les performances des entreprises avec le contexte social de leur Zone d'Emploi. Cet onglet explore les corrélations statistiques entre le score Egapro et des indicateurs locaux."),
  
  p(strong(tags$a(href="#", onclick="document.querySelector('a[data-value=\"Historique & Évolutions\"]').click(); return false;", "Historique & Évolutions")), " : Suivez les tendances pluriannuelles. Observez si les scores s'améliorent au fil du temps dans les territoires que vous sélectionnez."),
  br(),
  
  
  h3(strong("Points de vigilance")),
  tags$ul(
    tags$li(strong("Effet siège social :"), " L'analyse est basée sur la localisation du siège social (SIREN). La performance affichée est celle de l'entreprise dans son ensemble, et non celle de ses établissements locaux (SIRET). Cela explique la forte concentration de déclarations à Paris et dans les grands pôles tertiaires."),
    tags$li(strong("Corrélation n'est pas causalité :"), " L'onglet 'Socio-démographique' peut révéler des liens statistiques, mais ne permet pas d'établir des relations de cause à effet. ls constituent avant tout des pistes de réflexion, et non des preuves définitives."),
    tags$li(strong("Qualité des données :"), " L'outil reflète les données déclarées par les entreprises. Des biais peuvent exister (erreurs, non-déclarations), bien que la couverture des données s'améliore progressivement.")
  ),
  br(),
  hr(),
  
  h3(strong("Crédits")),
  p("Conception et développement : Alberto Esperon.", br(),
    "Projet mené dans le cadre du Master 2 PISE (Promotion 2024-2025).")
)
