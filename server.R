# Serveur
server <- function(input, output, session) {
  
  
  # OPTIONS télécharger les fichiers PDF
  # Créer le dossier www s'il n'existe pas
  if (!dir.exists("www")) {
    dir.create("www")
  }
  
  # Liste des fichiers et URLs
  files_urls <- list(
    "www/SIMR3_I_Introduction.pdf" = "https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/PDF-files/SIMR3_I_Introduction.pdf",
    "www/SIMR3_XI_Directives.pdf" = "https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/PDF-files/SIMR3_XI_Directives.pdf",
    "www/SIMR2.pdf" = "https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/PDF-files/SIMR2.pdf",
    "www/Situation_Urgences_Sanitaires_S50.pdf" = "https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/PDF-files/Situation_Urgences_Sanitaires_S50.pdf"
  )
  
  # Téléchargement des fichiers
  for (file in names(files_urls)) {
    url <- files_urls[[file]]
    download.file(url, destfile = path.expand(file), mode = "wb")
  }
  
  # Liste des districts par région
  districts_par_region <- list(
    "Grand Lomé" = c("Agoè-Nyivé", "Golfe"),
    "Maritime" = c("Avé", "Bas-Mono", "Lacs", "Vo", "Yoto", "Zio"),
    "Plateaux" = c("Agou", "Akébou", "Amou", "Anié", "Danyi", "Est-Mono", "Haho", "Kloto", "Kpélé", "Moyen-Mono", "Ogou", "Wawa"),
    "Centrale" = c("Blitta", "Mô", "Sotouboua", "Tchamba", "Tchaoudjo"),
    "Kara" = c("Assoli", "Bassar", "Binah", "Dankpen", "Doufelgou", "Kéran", "Kozah"),
    "Savanes" = c("Cinkassé", "Kpendjal", "Kpendjal-Ouest", "Oti", "Oti-Sud", "Tandjoaré", "Tône"),
    "Toutes" = c("Toutes", "Agoè-Nyivé", "Golfe", "Avé", "Bas-Mono", "Lacs", "Vo", "Yoto", "Zio", 
                 "Agou", "Akébou", "Amou", "Anié", "Danyi", "Est-Mono", "Haho", "Kloto",
                 "Kpélé", "Moyen-Mono", "Ogou", "Wawa", "Blitta", "Mô", "Sotouboua", "Tchamba", 
                 "Tchaoudjo", "Assoli", "Bassar", "Binah", "Dankpen", "Doufelgou", "Kéran", "Kozah",
                 "Cinkassé", "Kpendjal", "Kpendjal-Ouest", "Oti", "Oti-Sud", "Tandjoaré", "Tône")
  )
  
  # Observer pour la sélection de la région
  observe({
    # Vérifiez si la région a été sélectionnée
    req(input$region) # Si rien n'est sélectionné, attend la sélection
    
    region_choisie <- input$region  # Récupérer la région choisie
    
    # Met à jour la liste des districts en fonction de la région choisie
    updateSelectInput(session, "district", 
                      choices = c("Toutes", districts_par_region[[region_choisie]]), 
                      selected = "Toutes")
  })
  
  # Réactif pour stocker les sélections des boutons
  selected_buttons <- reactiveVal(character(0))
  
  # Fonction pour mettre à jour les boutons sélectionnés
  update_selected_buttons <- function(button_id) {
    selected_buttons(button_id)  # Mettre à jour avec le bouton sélectionné
  }
  
  # Gérer les boutons de sélection exclusive
  observeEvent(input$Synthese_btn, {
    update_selected_buttons("Synthese_btn")
    updateButtonStates(session, "Synthese_btn", c("cholera_btn", "dengue_btn", "rougeole_btn", "polio_btn"))
  })
  observeEvent(input$cholera_btn, {
    update_selected_buttons("cholera_btn")
    updateButtonStates(session, "cholera_btn", c("Synthese_btn", "dengue_btn", "rougeole_btn", "polio_btn"))
  })
  observeEvent(input$dengue_btn, {
    update_selected_buttons("dengue_btn")
    updateButtonStates(session, "dengue_btn", c("Synthese_btn", "cholera_btn", "rougeole_btn", "polio_btn"))
  })
  
  observeEvent(input$rougeole_btn, {
    update_selected_buttons("rougeole_btn")
    updateButtonStates(session, "rougeole_btn", c("Synthese_btn", "cholera_btn", "dengue_btn", "polio_btn"))
  })
  
  observeEvent(input$polio_btn, {
    update_selected_buttons("polio_btn")
    updateButtonStates(session, "polio_btn", c("Synthese_btn", "cholera_btn", "dengue_btn", "rougeole_btn"))
  })
  
  # Fonction pour mettre à jour les états des boutons
  updateButtonStates <- function(session, selected_id, other_ids) {
    # Mettre le bouton sélectionné en mode actif (style de classe)
    shinyjs::addClass(selector = paste0("#", selected_id), class = "active")
    
    # Réinitialiser tous les autres boutons (non sélectionnés)
    lapply(other_ids, function(id) {
      shinyjs::removeClass(selector = paste0("#", id), class = "active")
    })
  }
  
  # Générer dynamiquement les listes déroulantes sous chaque bouton sélectionné
  output$analysis_btn <- renderUI({
    selected <- selected_buttons()
    
    # Effacer le contenu si aucun bouton n'est sélectionné
    if (length(selected) == 0) {
      return(NULL)
    }
    
    # Créer les éléments en fonction des boutons sélectionnés
    choices_ui <- lapply(selected, function(id) {
      switch(
        id,
        "Synthese_btn" = div(class = "btn-group", role = "group",
                             style = "display: flex; width: 100%; justify-content: space-between;",
                             actionButton("option1", "Option 1", class = "btn btn-secondary",
                                          style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                             actionButton("option2", "Option 2", class = "btn btn-secondary",
                                          style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                             actionButton("option3", "Option 3", class = "btn btn-secondary",
                                          style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                             actionButton("option4", "Option 4", class = "btn btn-secondary",
                                          style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;")
        ),
        "cholera_btn" = div(class = "btn-group", role = "group",
                            style = "display: flex; width: 100%; justify-content: space-between;",
                            actionButton("apercu_cholera", "Aperçu", class = "btn btn-secondary",
                                         style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                            actionButton("Stat_cholera", "Statistiques", class = "btn btn-secondary",
                                         style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                            actionButton("Graph_cholera", "Graphiques", class = "btn btn-secondary",
                                         style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                            actionButton("gtfcc_weekly_reporting", "Tablau AFRO", class = "btn btn-secondary",
                                         style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;")
        ),
        "dengue_btn" = div(class = "btn-group", role = "group",
                           style = "display: flex; width: 100%; justify-content: space-between;",
                           actionButton("choixX", "Choix X", class = "btn btn-secondary",
                                        style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                           actionButton("choixY", "Choix Y", class = "btn btn-secondary",
                                        style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                           actionButton("choixZ", "Choix Z", class = "btn btn-secondary",
                                        style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                           actionButton("choixW", "Choix W", class = "btn btn-secondary",
                                        style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;")
        ),
        "rougeole_btn" = div(class = "btn-group", role = "group",
                             style = "display: flex; width: 100%; justify-content: space-between;",
                             actionButton("choix1", "Choix 1", class = "btn btn-secondary",
                                          style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                             actionButton("choix2", "Choix 2", class = "btn btn-secondary",
                                          style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                             actionButton("choix3", "Choix 3", class = "btn btn-secondary",
                                          style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                             actionButton("choix4", "Choix 4", class = "btn btn-secondary",
                                          style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;")
        ),
        "polio_btn" = div(class = "btn-group", role = "group",
                          style = "display: flex; width: 100%; justify-content: space-between;",
                          actionButton("apercu_polio", "Aperçu", class = "btn btn-secondary",
                                       style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                          actionButton("Stat_polio", "Statistiques", class = "btn btn-secondary",
                                       style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                          actionButton("Graph_polio", "Graphiques", class = "btn btn-secondary",
                                       style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;"),
                          actionButton("rrt_weekly_reporting", "Tablau RRT", class = "btn btn-secondary",
                                       style = "font-size: calc(1vw + 5px); width: 24%; background-color: lightblue;")
        ),
        NULL  # Retourne rien si l'ID n'est pas trouvé
      )
    })
    
    # Retourner les éléments générés dans une liste verticale
    do.call(tags$div, choices_ui)
  })
  
  
  
  # HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
  
  # Créer des états logiques pour les boutons
  button_state <- reactiveValues(
    active = "apercu_cholera" # Valeur par défaut
  )
  
  # Observateur pour Aperçu
  observeEvent(input$apercu_cholera, {
    if (input$apercu_cholera) {
      button_state$active <- "apercu_cholera"
    }
  })
  
  # Observateur pour Statistiques
  observeEvent(input$Stat_cholera, {
    if (input$Stat_cholera) {
      button_state$active <- "Stat_cholera"
    }
  })
  
  # Observateur pour Graphiques
  observeEvent(input$Graph_cholera, {
    if (input$Graph_cholera) {
      button_state$active <- "Graph_cholera"
    }
  })
  
  # Observateur pour Suppléments
  observeEvent(input$gtfcc_weekly_reporting, {
    if (input$gtfcc_weekly_reporting) {
      button_state$active <- "gtfcc_weekly_reporting"
    }
  })
  
  # Observateur pour Aperçu
  observeEvent(input$apercu_polio, {
    if (input$apercu_polio) {
      button_state$active <- "apercu_polio"
    }
  })
  
  # Observateur pour Statistiques
  observeEvent(input$Stat_polio, {
    if (input$Stat_polio) {
      button_state$active <- "Stat_polio"
    }
  })
  
  # Observateur pour Graphiques
  observeEvent(input$Graph_polio, {
    if (input$Graph_polio) {
      button_state$active <- "Graph_polio"
    }
  })
  
  # Observateur pour Suppléments
  observeEvent(input$rrt_weekly_reporting, {
    if (input$rrt_weekly_reporting) {
      button_state$active <- "rrt_weekly_reporting"
    }
  })
  
  # Mise à jour dynamique du contenu en fonction de l'état actif
  output$analysis_content <- renderUI({
    if (button_state$active == "apercu_cholera") {
      fluidPage(
        uiOutput("resume_jour"),
        fluidRow(
          column(12,
                 valueBoxOutput("day_cas"),
                 valueBoxOutput("day_death"),
                 valueBoxOutput("day_hcas"),
                 valueBoxOutput("day_comcas"),
                 valueBoxOutput("day_hfcas")
          )
        ),
        uiOutput("resume_periode"),
        fluidRow(
          column(12,
                 valueBoxOutput("cumul_cas"),
                 valueBoxOutput("cumul_death"),
                 valueBoxOutput("cumul_hcas"),
                 valueBoxOutput("cumul_comcas"),
                 valueBoxOutput("cumul_hfcas")
          )
        )
      )
    } else if (button_state$active == "Stat_cholera") {
      fluidPage(
        headerPanel(uiOutput("resume_statistique")),
        tags$h3("Situation cumulée des cas"),
        fluidRow(
          valueBoxOutput("cumul_cas1", width = 3),
          valueBoxOutput("cumul_positif", width = 3),
          valueBoxOutput("cumul_death1", width = 3),
          valueBoxOutput("cumul_deathrate", width = 3)
        ),
        tags$h3("Situation cumulée des cas en milieu hospitalier"),                 
        fluidRow( 
          valueBoxOutput("cumul_hfcas2", width = 3),
          valueBoxOutput("cumul_hfpositif", width = 3),
          valueBoxOutput("cumul_hfdeath", width = 3),
          valueBoxOutput("cumul_hfdeathrate", width = 3)   
        ),
        tags$h3("Situation cumulée des cas communautaires"),                 
        fluidRow( 
          valueBoxOutput("cumul_comcas2", width = 3),
          valueBoxOutput("cumul_compositif", width = 3),
          valueBoxOutput("cumul_comdeath", width = 3),
          valueBoxOutput("cumul_comdeathrate", width = 3)
        )
      )
    } else if (button_state$active == "Graph_cholera") {
      fluidPage(
        headerPanel(uiOutput("resume_graphique")),
        fluidPage(
          fluidRow(
            column(
              width = 12,
              downloadButton("Mean_evoluplotweek_download", "Télécharger le graphique")
            ),
            column(
              width=12,
              box(
                plotOutput("Mean_evoluplotweek"), 
                title="EpiCurve des cas de choléra",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                width=NULL)
            )
          ),
          
          fluidRow(
            column(
              width=6,
              box(
                plotOutput("evoluplotweek"), 
                title="Evolution hebdomadaire des cas de choléra",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                width=NULL)
            ),
            column(
              width=6,
              box(
                plotOutput("evoluplotjour"), 
                title="Evolution journalière des cas de choléra",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                width="100%")
            )
          ),
          fluidRow(
            column(
              width=6,
              box(plotOutput("pyamidplotcas"), 
                  title="Pyramide des âges des décès de choléra",
                  status = "warning", solidHeader = TRUE,
                  collapsible = TRUE,
                  width=NULL)
            ),
            column(
              width=6,
              box(div(style = "padding: 0; margin: 0;",
                      plotOutput("pyamidplotpercent")), 
                  title="Pyramide des âges des cas (%) de choléra",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  width=NULL) 
            )
          ),
          fluidRow(
            column(
              width=4,
              box(plotOutput("Sexeplot"), 
                  title="Repartition des cas selon le Genre",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  width=NULL) 
            ),
            column(
              width=4,
              box(plotOutput("districtplot"), 
                  title="Repartition des cas selon le District",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  width=NULL) 
            ),
            column(
              width=4,
              box(plotOutput("regionplot"), 
                  title="Repartition des cas selon la Région",
                  status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  width=NULL) 
            )
          ),
          fluidRow(
            column(
              width=12,
              box(
                plotOutput("professionPlot"), 
                title="Rapartition des cas de choléra selon la profession du malade",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                width=NULL)
            )
          ),
          fluidRow(
            column(
              width=12,
              box(
                plotOutput("professionPlot2"), 
                title="Rapartition des cas de choléra selon la profession du malade et le statut laboratoire",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                width=NULL)
            )
          ),
          fluidRow(
            column(
              width=12,
              box(
                div(
                  leafletOutput("mapCommune", width = "100%", height = "100%"),
                  style = "height: 72vh;" # Définit la hauteur en pourcentage de la hauteur de la fenêtre (viewport)
                ),
                title="Repartition des cas de choléra selon la profession du malade",
                status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                width="100%")
            )
          )
        ) 
      )
    } else if (button_state$active == "gtfcc_weekly_reporting") {
      # Code pour les suppléments
      fluidPage(
        tags$h3("Tableau des données suivant le format Rapport hebdo GTFCC de AFRO"),
        downloadButton("download_data", "Exporter le tableau en Excel (.xlsx)"),  # Bouton pour télécharger
        # Pour afficher le tableau
        # Zone pour afficher le tableau avec scroll horizontal et vertical
        div(style = "overflow-x: auto; overflow-y: auto; max-width: 100%; max-height: 510px;",
            gt_output("table_afro"))
      )
    }
    # ################################ POLIO ###########################################
    else if (button_state$active == "apercu_polio") {
      fluidPage(
        uiOutput("resume_jour_polio"),
        fluidRow(
          column(12,
                 valueBoxOutput("day_cas_pfa"),
                 valueBoxOutput("day_positif_polio"),
                 valueBoxOutput("day_NPENT_polio")
          )
        ),
        uiOutput("resume_week_polio"),
        fluidRow(
          column(12,
                 valueBoxOutput("lastweek_cas_pfa"),
                 valueBoxOutput("lastweek_positif_polio"),
                 valueBoxOutput("lastweek_NPENT_polio")
          )
        ),
        uiOutput("resume_periode_polio"),
        fluidRow(
          column(12,
                 valueBoxOutput("cumul_cas_pfa"),
                 valueBoxOutput("cumul_polio"),
                 valueBoxOutput("cumul_npent_polios")
          )
        )
      )
    }
  })
  
  
  
  
  
  
  # ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||--
  output$resume_periode <- renderUI({
    periode <- input$periode
    if (is.null(periode) || length(periode) != 2) {
      periode_txt <- "Période non définie"
    } else {
      periode_txt <- paste0("du ", periode[1], " au ", periode[2])
    }
    tags$h3(tags$b(paste("Situation cumulée des cas ", periode_txt)))
  })
  output$resume_periode_polio <- renderUI({
    periode <- input$periode
    if (is.null(periode) || length(periode) != 2) {
      periode_txt <- "Période non définie"
    } else {
      periode_txt <- paste0("du ", periode[1], " au ", periode[2])
    }
    tags$h3(tags$b(paste("Situation cumulée des cas de PFA ", periode_txt)))
  })
  
  output$resume_jour <- renderUI({
    periode <- input$periode
    if (is.null(periode) || length(periode) != 2) {
      periode_txt <- "Période non définie"
    } else {
      periode_txt <- periode[2]
    }
    tags$h3(tags$b(paste("Résumé journaliers (", periode_txt, ")")))
  })
  
  output$resume_jour_polio <- renderUI({
    periode <- input$periode
    if (is.null(periode) || length(periode) != 2) {
      periode_txt <- "Période non définie"
    } else {
      periode_txt <- periode[2]
    }
    tags$h3(tags$b(paste("Résumé journaliers surveillance PFA (", periode_txt, ")")))
  })
  
  output$resume_statistique <- renderUI({
    periode <- input$periode
    if (is.null(periode) || length(periode) != 2) {
      periode_txt <- "Période non définie"
    } else {
      periode_txt <- paste0("du ", periode[1], " au ", periode[2])
    }
    tags$h3(tags$b(paste("Résumé statistique sur la situation de cholera au Togo ", periode_txt)))
  })
  output$resume_statistique_polio <- renderUI({
    periode <- input$periode
    if (is.null(periode) || length(periode) != 2) {
      periode_txt <- "Période non définie"
    } else {
      periode_txt <- paste0("du ", periode[1], " au ", periode[2])
    }
    tags$h3(tags$b(paste("Résumé statistique sur la surveillance du PFA au Togo ", periode_txt)))
  })
  
  output$resume_graphique <- renderUI({
    periode <- input$periode
    if (is.null(periode) || length(periode) != 2) {
      periode_txt <- "Période non définie"
    } else {
      periode_txt <- paste0("du ", periode[1], " au ", periode[2])
    }
    tags$h3(tags$b(paste("Illustrations graphiques sur la situation de choléra au Togo", periode_txt)))
  })
  
  output$resume_graphique_polio <- renderUI({
    periode <- input$periode
    if (is.null(periode) || length(periode) != 2) {
      periode_txt <- "Période non définie"
    } else {
      periode_txt <- paste0("du ", periode[1], " au ", periode[2])
    }
    tags$h3(tags$b(paste("Illustrations graphiques sur la surveillance du PFA au Togo", periode_txt)))
  })
  
  # --------------------cas du jour-------------
  output$day_cas <- renderValueBox({
    req(input$periode, length(input$periode) == 2, input$periode[2])  # Vérifie que `input$periode` est défini et valide
    cholera <- Data_cholera
    # Vérifie que les colonnes nécessaires sont présentes
    required_columns <- c("Dateonset", "Prefecture", "Classification")
    if (!all(required_columns %in% colnames(cholera))) {
      return(valueBox(
        "Colonnes manquantes",
        "Nouveau cas",
        icon = icon("fas fa-exclamation-triangle"),
        color = "red",
        subtitle = "Colonnes introuvables"
      ))
    }
    
    # Filtrage par les critères sélectionnés
    cholera <- cholera %>%
      dplyr::filter(Dateonset == input$periode[2])
    
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    # Calculer le nombre de cas (0 si aucune ligne restante)
    total_cases <- nrow(cholera)
    
    # Générer le valueBox avec 0 si la base est vide
    valueBox(
      as.integer(total_cases),
      "Nouveau cas",
      icon = icon("fas fa-virus"),
      color = ifelse(total_cases > 0, "orange", "blue"),
      subtitle = ifelse(total_cases > 0, "Cas du jour", "Aucun cas pour ce jour")
    )
  })
  
  # 
  output$day_death <- renderValueBox({
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset == input$periode[2], Issue == "dcd")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Nouveau décès",
      icon = icon("cross"),
      color = "red",
      subtitle = "Décès du jour"
    )
  })
  
  output$day_hcas <- renderValueBox({
    cholera <- Data_cholera
    
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera %>%
                        dplyr::filter(Dateonset == input$periode[2], is.na(Issue)))),
      "Cas actifs",
      icon = icon("fas fa-heart-broken"),
      color = "yellow",
      subtitle = "Cas actifs"
    )
  })
  
  output$day_comcas <- renderValueBox({
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset == input$periode[2], Type_fs_comm == "Communautaire")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Cas communautaire",
      icon = icon("fas fa-users"),
      color = "green",
      subtitle = "Cas communautaire"
    )
  })
  
  output$day_hfcas <- renderValueBox({
    cholera <- Data_cholera
    
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera %>% dplyr::filter(Dateonset == input$periode[2], Type_fs_comm != "Communautaire"))),
      "Cas en formation sanitaire",
      icon = icon("hospital"),
      color = "blue",
      subtitle = "Cas en formation sanitaire"
    )
  })
  # __________________________________________
  
  
  # ________________________________________
  
  output$cumul_cas <- renderValueBox({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera %>% dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2]))),
      "Cas cumulés",
      icon = icon("fas fa-virus"),
      color = "orange",
      subtitle = "Cas cumulés"
    )
  })
  
  output$cumul_death <- renderValueBox({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera %>% dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Issue == "dcd"))),
      "Décès cumulés",
      icon = icon("cross"),
      color = "red",
      subtitle = "Total décès"
    )
  })
  output$cumul_deathrate <- renderValueBox({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }    
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      paste0(round(100 * nrow(cholera %>% dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Issue == "dcd")) / nrow(cholera %>%
                                                                                                                                                dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])), 1), " %"),
      "Létalité",
      icon = icon("fas fa-heart-broken"),
      color = "purple",
      subtitle = "Létalité en FS"
    )
  })
  
  output$cumul_hcas <- renderValueBox({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera %>% dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], is.na(Issue)))),
      "Cas actifs",
      icon = icon("fas fa-heart-broken"),
      color = "yellow",
      subtitle = "Cas actifs"
    )
  })
  output$cumul_comcas <- renderValueBox({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera %>% dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Type_fs_comm == "Communautaire"))),
      "Total Cas communautaire",
      icon = icon("fas fa-users"),
      color = "green",
      subtitle = "Cumul cas communautaire"
    )
  })
  
  output$cumul_hfcas <- renderValueBox({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera %>% dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Type_fs_comm != "Communautaire")
      )),
      "Total Cas en formation sanitaire",
      icon = icon("hospital"),
      color = "blue",
      subtitle = "Total Cas en FS"
    )
  })
  
  
  output$cumul_cas1 <- renderValueBox({
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Cas cumulés",
      icon = icon("fas fa-virus"),
      color = "blue",
      subtitle = "Total suspects"
    )
  })
  
  output$cumul_positif <- renderValueBox({
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Type == "Positif")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Cas confimés",
      icon = icon("bacterium"),
      color = "orange",
      subtitle = "Total confimés"
    )
  })
  
  output$cumul_death1 <- renderValueBox({
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Issue == "dcd")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Décès cumulés",
      icon = icon("cross"),
      color = "red",
      subtitle = "Total décès"
    )
  })
  
  output$cumul_hfdeathrate <- renderValueBox({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }   
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      paste0(round(100 * nrow(cholera %>%
                                dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Issue == "dcd", Type_fs_comm != "Communautaire")) / nrow(cholera %>%
                                                                                                                                                                       dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Type_fs_comm != "Communautaire")), 1), " %"),
      "Létalité",
      icon = icon("fas fa-heart-broken"),
      color = "purple",
      subtitle = "Létalité en Formation Sanitatire"
    )
  })
  output$cumul_hfcas2 <- renderValueBox({
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Type_fs_comm != "Communautaire")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Total Cas en formation sanitaire",
      icon = icon("hospital"),
      color = "blue",
      subtitle = "Cumul cas hospitalière"
    )
  })
  output$cumul_hfpositif <- renderValueBox({
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Type == "Positif",Type_fs_comm != "Communautaire")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Cas hospitaliers confimés",
      icon = icon("bacterium"),
      color = "orange",
      subtitle = "Cas hospitaliers confimés"
    )
  })
  output$cumul_hfdeath <- renderValueBox({
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Issue == "dcd",Type_fs_comm != "Communautaire")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Décès hospitaliers",
      icon = icon("cross"),
      color = "red",
      subtitle = "Décès hospitaliers"
    )
  })
  
  output$cumul_comcas2 <- renderValueBox({
    cholera <- Data_cholera
    
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Type_fs_comm == "Communautaire")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Cas cumulés",
      icon = icon("fas fa-users"),
      color = "blue",
      subtitle = "Cumaul Cas Communautaire"
    )
  })
  
  output$cumul_compositif <- renderValueBox({
    cholera <- Data_cholera
    
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Type == "Positif", Type_fs_comm == "Communautaire")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Cas confimés",
      icon = icon("bacterium"),
      color = "orange",
      subtitle = "Cas Communautaire confimés"
    )
  })
  
  output$cumul_comdeath <- renderValueBox({
    cholera <- Data_cholera
    
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Issue == "dcd", Type_fs_comm == "Communautaire")
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      as.integer(nrow(cholera)),
      "Décès cumulés",
      icon = icon("cross"),
      color = "red",
      subtitle = "Décès communautaire"
    )
  })
  
  output$cumul_comdeathrate <- renderValueBox({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }    
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    valueBox(
      paste0(round(100 * nrow(cholera %>%
                                dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2], Issue == "dcd", Type_fs_comm == "Communautaire")) / nrow(cholera %>%
                                                                                                                                                                       dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2],Type_fs_comm == "Communautaire")), 1), " %"),
      "Létalité",
      icon = icon("fas fa-heart-broken"),
      color = "purple",
      subtitle = "Létalité en communauté"
    )
  })
  # ====================================================================
  
  # ===================================================================
  
  #epi curve
  #  output$Mean_evoluplotweek <- renderPlot({
  plot_data <- reactive({
    cholera <- Data_cholera
    
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    cholera00 <-cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    #  je veux donnée les valeur de S1 2024 à S1 2025
    cholera <- cholera %>%
      # Créer une nouvelle semaine combinée en prenant en compte les chevauchements
      mutate(Année = ifelse(Année == 2024 & Week %in% unique(cholera$Week[cholera$Année == 2025]),
                            2025, 
                            Année))
    
    # Calcul des données avec létalité avec `reframe`
    data <- cholera %>%
      dplyr::group_by(Année, Week,Issue01) %>%
      dplyr::reframe(
        nb = dplyr::n()
      )
    
    # 1. Créer un vecteur contenant toutes les semaines entre la min et max de Week
    # 2024
    # Séquence pour les semaines de 2024
    if (!is.null(nrow(cholera$Week[cholera$Année == 2024]))){
      weeks_full01 <- seq(
        from = min(cholera$Week[cholera$Année == 2024], na.rm = TRUE),
        to = max(cholera$Week[cholera$Année == 2024], na.rm = TRUE),
        by = 1
      )
      # 2. Fusionner ce vecteur avec votre dataframe pour ajouter des semaines manquantes 2024
      data <- data %>%
        dplyr::full_join(data.frame(Week = weeks_full01), by = "Week") %>%
        # 3. Ajouter des valeurs 0 pour les semaines manquantes
        dplyr::mutate(
          nb = ifelse(is.na(nb), 0, nb),  # Remplacer les NA de 'nb' par 0
          Issue01=ifelse(is.na(Issue01), "vivant", Issue01),
          Année = ifelse(is.na(Année), 2024, Année)
        )
    }
    # Séquence pour les semaines de 2025
    if (!is.null(nrow(cholera$Week[cholera$Année == 2025]))){
      weeks_full02 <- as.numeric(seq(
        from = min(cholera$Week[cholera$Année == 2025], na.rm = TRUE),
        to = max(cholera$Week[cholera$Année == 2025], na.rm = TRUE),
        by = 1
      ))
      # 2. Fusionner ce vecteur avec votre dataframe pour ajouter des semaines manquantes 2025
      data <- data %>%
        dplyr::full_join(data.frame(Week = weeks_full02), by = "Week") %>%
        # 3. Ajouter des valeurs 0 pour les semaines manquantes
        dplyr::mutate(
          nb = ifelse(is.na(nb), 0, nb),  # Remplacer les NA de 'nb' par 0
          Issue01=ifelse(is.na(Issue01), "vivant", Issue01),
          Année = ifelse(is.na(Année), 2025, Année)
        )
    }
    
    # Calcul des données avec létalité avec `reframe`
    data01 <- cholera %>% 
      dplyr::filter(Issue01=="décès") %>%
      dplyr::group_by(Année,Week) %>%
      dplyr::reframe(deaths=dplyr::n()) 
    
    data02 <- cholera %>%
      dplyr::group_by(Année,Week) %>%
      dplyr::reframe(nbcas=dplyr::n()) 
    
    
    data03 <- dplyr::full_join(data01,data02)
    data03$Letality <-round(data03$deaths/data03$nbcas*100,1)
    data03 <- data03 %>% 
      dplyr::mutate(
        nbcas = ifelse(is.na(nbcas), 0, nbcas),
        Letality = ifelse(is.na(Letality), 0, Letality),
        deaths = ifelse(is.na(deaths), 0, deaths)
      ) %>% dplyr::arrange(Année,Week)  # Tri des données par la variable Week
    
    # 2. Fusionner ce vecteur avec votre dataframe pour ajouter des semaines manquantes de 2024
    if (!is.null(nrow(cholera$Week[cholera$Année == 2024]))){
      data03 <- data03 %>%
        dplyr::full_join(data.frame(Week = weeks_full01), by = "Week") %>%
        # 3. Ajouter des valeurs 0 pour les semaines manquantes
        dplyr::mutate(
          Letality = ifelse(is.na(Letality), 0, Letality),  # Remplacer les NA de 'Letality' par 0
          deaths = ifelse(is.na(deaths), 0, deaths),
          nbcas = ifelse(is.na(nbcas), 0, nbcas),
          Année = ifelse(is.na(Année), 2024, Année)
        )
    }
    # 2. Fusionner ce vecteur avec votre dataframe pour ajouter des semaines manquantes de 2025
    if (!is.null(nrow(cholera$Week[cholera$Année == 2025]))){
      data03 <- data03 %>%
        dplyr::full_join(data.frame(Week = weeks_full02), by = "Week") %>%
        # 3. Ajouter des valeurs 0 pour les semaines manquantes
        dplyr::mutate(
          Letality = ifelse(is.na(Letality), 0, Letality),  # Remplacer les NA de 'Letality' par 0
          deaths = ifelse(is.na(deaths), 0, deaths),
          nbcas = ifelse(is.na(nbcas), 0, nbcas),
          Année = ifelse(is.na(Année), 2025, Année)
        )
      
    }
    # Déterminer le maximum pour la mise à l'échelle et autres calculs
    max_nb <- max(data$nb, na.rm = TRUE)
    
    # Calcul d'une échelle proportionnelle pour la létalité
    max_cases <- max(data$nb, na.rm = TRUE)
    max_lethality <- max(data03$Letality, na.rm = TRUE)
    
    # Modifier les données pour afficher les semaines 2024 avant 2025
    data <- data %>%
      mutate(Week_Adjusted = ifelse(Année == 2024, Week - max(Week), Week))
    # Modifier les données pour afficher les semaines 2024 avant 2025
    data03 <- data03 %>%
      mutate(Week_Adjusted = ifelse(Année == 2024, Week - max(Week), Week))
    # Déterminer le maximum pour la mise à l'échelle et autres calculs
    max_nb <- max(data$nb, na.rm = TRUE)
    # Transition week for geom_vline
    transition_week_2024 <- min(data$Week_Adjusted[data$Année == 2024], na.rm = TRUE)
    transition_week_2025 <- min(data$Week_Adjusted[data$Année == 2025], na.rm = TRUE)
    
    # Créer le graphique
    ggplot2::ggplot(data, ggplot2::aes(x = Week_Adjusted)) +
      
      # Ligne de transition pour 2025
      ggplot2::geom_vline(
        xintercept = transition_week_2025,
        color = "red",
        linetype = "dashed",
        size = 0.9,
        alpha = 0.8
      )+
      annotate(
        "text",
        x = transition_week_2024+1,
        y = max_nb * 1.05,
        label = "2024",
        color = "blue",
        size = 5,
        hjust = 0
      ) +
      annotate(
        "text",
        x = transition_week_2025 + 1,
        y = max_nb * 1.05,
        label = "2025",
        color = "blue",
        size = 5,
        hjust = 0
      ) +
      # Barres des cas
      ggplot2::geom_bar(
        ggplot2::aes(y = nb, fill = factor(Issue01, levels = c("vivant", "décès"))),
        stat = "identity",
        color = "darkgrey",
        alpha = 0.6
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          y = nb,
          label = nb  # Valeur affichée au sommet de chaque barre
        ),
        vjust = 1.3,  # Décalage vertical
        hjust = -0.05,
        color = "black",
        size = 3
      ) +
      # Ligne de létalité
      ggplot2::geom_line(
        data = data03,
        ggplot2::aes(y = Letality * max_nb / max(data03$Letality, na.rm = TRUE), color = "Létalité"),
        size = 1
      ) +
      ggplot2::geom_point(
        data = data03,
        ggplot2::aes(y = Letality * max_nb / max(data03$Letality, na.rm = TRUE), color = "Létalité"),
        size = 2
      ) +
      ggplot2::geom_text(
        data = data03,
        ggplot2::aes(
          y = Letality * max_cases / max_lethality,,
          label = paste0(round(Letality, 0), "%")  # Afficher les valeurs de létalité avec 1 décimale
        ),
        vjust = -1.2,  # Positionnement au-dessus des points
        color = "darkred",
        size = 3
      )+
      # Afficher les vraies semaines sur l'axe des X
      ggplot2::scale_x_continuous(
        breaks = data$Week_Adjusted,
        labels = paste0("S", data$Week),
        name = "Semaine"
      ) +
      ggplot2::scale_y_continuous(
        name = "Nombre de cas (barres)",
        sec.axis = ggplot2::sec_axis(~ . * max(data03$Letality, na.rm = TRUE) / max_nb, name = "Létalité (%)")
      ) +
      ggplot2::scale_fill_manual(name = "Issue", values = c("vivant" = "#00A19C", "décès" = "darkred")) +
      ggplot2::scale_color_manual(name = "Courbes", values = c("Létalité" = "red")) +
      ggplot2::labs(x = "Semaine", y = "Nombre de cas") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 0.5,vjust = 0.5),
        legend.position = "top"
      )
  })
  
  output$Mean_evoluplotweek <- renderPlot({
    plot_data()
  })
  
  # # Télécharger le graphique
  # output$Mean_evoluplotweek_download <- downloadHandler(
  #   filename = function() {
  #     paste("EpiCurve_Cholera", Sys.Date(), ".png", sep = "")
  #   },
  #   content = function(file) {
  #     # Sauvegarder le graphique en image
  #     # Définir les dimensions du graphique en pixels
  #     png(file, width = 1400, height = 700) # Exemple : largeur 1200px, hauteur 800px
  #     print(plot_data()) # Génération du graphique
  #     dev.off() # Fermer le périphérique graphique
  #   }
  # )
  
  # output$Mean_evoluplotweek_download <- downloadHandler(
  #   filename = function() {
  #     paste0("EpiCurve_Cholera_", Sys.Date(), ".png")
  #   },
  #   content = function(file) {
  #     temp_file <- tempfile(fileext = ".png")  # Générer un fichier temporaire
  #     png(temp_file, width = 1400, height = 700, type = "cairo") 
  #     print(plot_data())  
  #     dev.off()
  #     file.copy(temp_file, file)  # Copier l'image générée vers l'utilisateur
  #   },
  #   contentType = "image/png"
  # )
  
  output$Mean_evoluplotweek_download <- downloadHandler(
    filename = function() {
      paste0("EpiCurve_Cholera_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Create a temporary file
      temp_file <- tempfile(fileext = ".png")
      
      # Ensure the temporary file is deleted after the function exits
      on.exit({
        if (file.exists(temp_file)) {
          unlink(temp_file)
        }
      })
      
      # Attempt to create the PNG file
      tryCatch({
        png(temp_file, width = 1400, height = 700, type = "cairo")
        print(plot_data())  # Generate the plot
        dev.off()
        
        # Copy the temporary file to the user's download location
        file.copy(temp_file, file)
      }, error = function(e) {
        # Log the error and provide a user-friendly message
        message("Problème dans l'enrégistrement du graphique : ", e$message)
        showNotification("Problème dans l'enrégistrement du graphique. Merci de reessayer", type = "error")
      })
    },
    contentType = "image/png"
  )
  
  # output$Mean_evoluplotweek_download <- downloadHandler(
  #   filename = function() {
  #     paste0("EpiCurve_Cholera_", Sys.Date(), ".png")
  #   },
  #   content = function(file) {
  #     ggsave(file, plot = plot_data(), width = 14, height = 7, units = "in", dpi = 300)
  #   },
  #   contentType = "image/png"
  # )
  
  
  
  output$Sexeplot <- renderPlot({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    ggplot2::ggplot(cholera %>% dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2]) %>%
                      dplyr::group_by(Sexe) %>%
                      dplyr::summarise(Nb_cas_r = sum(n)) %>%
                      dplyr::mutate(Part = round((Nb_cas_r / sum(Nb_cas_r)) * 100, 1)), ggplot2::aes(Sexe, Part)) +
      ggplot2::geom_col(fill = c("orange", "blue")) +
      ggplot2::theme_gray(base_size = 14) +
      ggplot2::labs(y = "Pourcentage", title = "Cas de choléra selon le genre") +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Part, "%"), y = Part + 3), position = ggplot2::position_dodge(0.9))
  })
  
  
  
  output$districtplot <- renderPlot({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    # Filtrer et résumer les données
    cholera_summary <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2]) %>%
      dplyr::group_by(Prefecture) %>%
      dplyr::summarise(Nb_cas_r = sum(n)) %>%
      dplyr::mutate(Part = round((Nb_cas_r / sum(Nb_cas_r)) * 100, 1))
    
    # Générer des couleurs aléatoires
    set.seed(123) # Pour la reproductibilité
    colors <- grDevices::rainbow(n = nrow(cholera_summary))
    
    ggplot2::ggplot(cholera_summary, ggplot2::aes(Prefecture, Part)) +
      ggplot2::geom_col(fill = colors) +
      ggplot2::theme_gray(base_size = 12) +
      ggplot2::labs(y = "Pourcentage", title = "Cas de choléra par district") +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Part, "%"), y = Part + 3), position = ggplot2::position_dodge(0.5))
  })
  
  output$regionplot <- renderPlot({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    ggplot2::ggplot(cholera %>% dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2]) %>%
                      dplyr::group_by(Région) %>%
                      dplyr::summarise(Nb_cas_r = sum(n)) %>%
                      dplyr::mutate(Part = round((Nb_cas_r / sum(Nb_cas_r)) * 100, 1)), ggplot2::aes(Région, Part)) +
      ggplot2::geom_col(ggplot2::aes(fill = Région)) +
      ggplot2::scale_fill_manual(values = sample(colors(), length(unique(cholera$Région)))) +
      ggplot2::theme_gray(base_size = 12) +
      ggplot2::labs(y = "Pourcentage", title = "Cas de choléra par Région") +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Part, "%"), y = Part + 3), position = ggplot2::position_dodge(0.9))
  })
  
  
  
  # pyramide pour les décès
  output$pyamidplotcas <- renderPlot({
    cholera <- Data_cholera
    
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    cholera <- cholera <- cholera %>%
      dplyr::filter(Issue == "dcd")
    
    datap2 <- as.data.frame(table(cholera$Tranche_age, cholera$Sexe))
    mid <- nrow(datap2)/2
    mid2<- nrow(datap2)/2+1
    ful <- nrow(datap2)
    cats2 <- datap2[1:mid, 1]
    Feminin2 <- datap2[1:mid, 3]
    Masculin2 <- datap2[mid2:ful, 3]
    datapy2 <- data.frame( Masculin2,Feminin2, cats2)
    
    datapy2$cats2 <- factor(
      datapy2$cats2,
      levels = c("[0-2[", "[2-5[", "[5-15[", "[15-45[", "[45-60[", "[60 +[")
    )
    totcas <- sum(datapy2$Feminin2) + sum(datapy2$Masculin2)
    
    datapy2 <- datapy2[order(datapy2$cats2), ]
    datapy2$Feminin2 <- (datapy2$Feminin2 /totcas) * 100
    datapy2$Masculin2 <- (datapy2$Masculin2 / totcas) * 100
    maxi <- max(max(datapy2$Feminin2), max(datapy2$Masculin2))    
    
    pyramid::pyramid(
      data = datapy2,
      main = "Pyramide des ages des décès de cholera en pourcentage (%) ",
      Laxis = seq(from = 0, to = maxi +  round(maxi/5), by = round(maxi/5)),
      labels = paste0(seq(0, maxi +  round(maxi/5), by = round(maxi/5)), "%"),
      Llab = "Masculin",
      Rlab = "Féminin",
      Clab = "Tranches d'âge",
      Csize = 1,
      Cstep = 1
    )
    
  })
  
  output$pyamidplotpercent <- renderPlot({
    cholera <- Data_cholera
    
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    
    datap2 <- as.data.frame(table(cholera$Tranche_age, cholera$Sexe))
    mid <- nrow(datap2)/2
    mid2<- nrow(datap2)/2+1
    ful <- nrow(datap2)
    cats2 <- datap2[1:mid, 1]
    Feminin2 <- datap2[1:mid, 3]
    Masculin2 <- datap2[mid2:ful, 3]
    datapy2 <- data.frame( Masculin2,Feminin2, cats2)
    
    datapy2$cats2 <- factor(
      datapy2$cats2,
      levels = c("[0-2[", "[2-5[", "[5-15[", "[15-45[", "[45-60[", "[60 +[")
    )
    totcas <- sum(datapy2$Feminin2) + sum(datapy2$Masculin2)
    
    datapy2 <- datapy2[order(datapy2$cats2), ]
    datapy2$Feminin2 <- (datapy2$Feminin2 /totcas) * 100
    datapy2$Masculin2 <- (datapy2$Masculin2 / totcas) * 100
    maxi <- max(max(datapy2$Feminin2), max(datapy2$Masculin2))    
    
    pyramid::pyramid(
      data = datapy2,
      main = "Pyramide des ages des cas de cholera en pourcentage (%) ",
      Laxis = seq(from = 0, to = maxi +  round(maxi/5), by = round(maxi/5)),
      labels = paste0(seq(0, maxi +  round(maxi/5), by = round(maxi/5)), "%"),
      Llab = "Masculin",
      Rlab = "Féminin",
      Clab = "Tranches d'âge",
      Csize = 1,
      Cstep = 1
    )
    
  })
  
  
  
  output$evoluplotjour <- renderPlot({
    cholera <- Data_cholera
    
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    
    cholera <- cholera %>%
      dplyr::group_by(Dateonset) %>%
      dplyr::summarise(nb = dplyr::n())
    
    data <- data.frame(
      day = as.Date(cholera$Dateonset),
      value = cholera$nb
    )
    
    ggplot2::ggplot(data, ggplot2::aes(x = day, y = value)) +
      ggplot2::geom_line(color = "blue", size = 0.8) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::xlab("Date") +
      ggplot2::ylab("Évolution journalière des cas") +
      ggplot2::scale_x_date(date_labels = "%d-%b", date_breaks = "1 week") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, size = 10, hjust = 1))
  })
  
  output$evoluplotweek <- renderPlot({
    cholera <- Data_cholera
    
    # Filtrage par période sélectionnée
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    
    # Gestion des années croisées entre 2024 et 2025
    cholera <- cholera %>%
      mutate(Année = ifelse(
        Année == 2024 & Week %in% unique(cholera$Week[cholera$Année == 2025]),
        2025,
        Année
      ))
    
    # Groupement des données pour le calcul des cas par classification
    data <- cholera %>%
      dplyr::group_by(Année, Week, Classification) %>%
      dplyr::summarise(nb = dplyr::n(), .groups = "drop")
    
    # Ajout des semaines manquantes pour chaque année
    for (year in unique(data$Année)) {
      weeks_full <- seq(
        min(cholera$Week[cholera$Année == year], na.rm = TRUE),
        max(cholera$Week[cholera$Année == year], na.rm = TRUE)
      )
      data <- data %>%
        dplyr::full_join(
          expand.grid(Week = weeks_full, Classification = c("suspect", "confirmé"), Année = year),
          by = c("Week", "Classification", "Année")
        ) %>%
        mutate(
          nb = ifelse(is.na(nb), 0, nb)
        )
    }
    
    # Modifier les semaines pour afficher 2024 avant 2025
    data <- data %>%
      mutate(Week_Adjusted = ifelse(Année == 2024, Week - max(Week), Week))
    
    # Déterminer le maximum pour la mise à l'échelle
    max_nb <- max(data$nb, na.rm = TRUE)
    # ordre
    # data <- data %>%
    #   mutate(Classification = factor(Classification, levels = c("confirmé","suspect")))
    # 
    # Créer le graphique
    ggplot2::ggplot(data, ggplot2::aes(x = Week_Adjusted, y = nb, fill = Classification)) +
      
      # Ligne de transition pour 2025
      ggplot2::geom_vline(
        xintercept = min(data$Week_Adjusted[data$Année == 2025], na.rm = TRUE),
        color = "red",
        linetype = "dashed",
        size = 0.8,
        alpha = 0.8
      ) +
      
      # Annotations des années
      annotate(
        "text",
        x = min(data$Week_Adjusted[data$Année == 2024], na.rm = TRUE) + 1,
        y = max_nb * 0.95,
        label = "2024",
        color = "blue",
        size = 5,
        hjust = 0
      ) +
      annotate(
        "text",
        x = min(data$Week_Adjusted[data$Année == 2025], na.rm = TRUE) + 1,
        y = max_nb * 0.95,
        label = "2025",
        color = "blue",
        size = 5,
        hjust = 0
      ) +
      
      # Barres des cas
      ggplot2::geom_bar(
        stat = "identity",
        position = ggplot2::position_stack(reverse = TRUE),
        color = "darkgrey",
        alpha = 0.8
      ) +
      
      # Valeurs sur les barres
      ggplot2::geom_text(
        ggplot2::aes(label = nb, fill = factor(Classification, levels = c("suspect","confirmé"))),
        position = ggplot2::position_stack(vjust = 0.5),
        color = "black",
        size = 3
      ) +
      
      # Mise en forme de l'axe X et des couleurs
      ggplot2::scale_x_continuous(
        breaks = data$Week_Adjusted,
        labels = paste0("S", data$Week),
        name = "Semaine"
      ) +
      ggplot2::scale_y_continuous(
        name = "Nombre de cas"
      ) +
      ggplot2::scale_fill_manual(
        name = "Classification Labo",
        values = c("suspect" = "#00A19C", "confirmé" = "#FFA0BA")
      ) +
      ggplot2::labs(x = "Semaine", y = "Nombre de cas") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 0.5, vjust = 0.5),
        legend.position = "top"
      )
  })
  
  
  output$professionPlot <- renderPlot({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    # Filtrer et résumer les données
    cholera_summary1 <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2]) %>%
      dplyr::group_by(Profession) %>%
      dplyr::summarise(Nb_cas_r = sum(n)) %>%
      dplyr::mutate(Part = round((Nb_cas_r / sum(Nb_cas_r)) * 100, 1))
    
    # Générer des couleurs
    set.seed(2345) # Pour la reproductibilité
    unique_professions <- cholera_summary1$Profession
    colors <- ifelse(unique_professions %in% c("autres", "N/A"), "grey", grDevices::rainbow(length(unique_professions)))
    
    # Tracer le graphique
    ggplot2::ggplot(cholera_summary1, ggplot2::aes(x = Profession, y = Part, fill = Profession)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::scale_fill_manual(values = setNames(colors, unique_professions)) +
      ggplot2::theme_gray(base_size = 12) +
      ggplot2::labs(y = "Pourcentage", title = "Cas de choléra selon la profession du malade") +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Part, "%"), y = Part + 3), position = ggplot2::position_dodge(0.5))
  })
  
  output$professionPlot2 <- renderPlot({
    cholera <- Data_cholera
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    # Filtrer et résumer les données
    cholera_summary2 <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2]) %>%
      dplyr::group_by(Profession, Classification) %>%
      dplyr::summarise(Nb_cas_r = sum(n), .groups = "drop") %>%
      dplyr::group_by(Profession) %>%
      dplyr::mutate(Part = round((Nb_cas_r / sum(Nb_cas_r)) * 100, 1))
    
    # Tracer le graphique avec des barres empilées
    ggplot2::ggplot(cholera_summary2, ggplot2::aes(x = Profession, y = Part, fill = Classification)) +
      ggplot2::geom_col(position = "stack", show.legend = TRUE) +
      ggplot2::scale_fill_manual(
        values = c("confirmé" = "orange", "suspect" = "lightblue")
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        y = "Pourcentage",
        x = "Profession",
        title = "Répartition des cas de choléra par profession et classification",
        fill = "Classification"
      ) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Part, "%")), 
                         position = ggplot2::position_stack(vjust = 0.5), size = 3)    
  })
  
  #### LES CARTES
  
  output$mapCommune <- renderLeaflet({
    #Importation du shapefile commune
    # URL brute du fichier ZIP
    url <- "https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/0caea3bc0ab697914d9db732e1539c68d1582d3e/data/Togo_Communes.zip"
    # Créer un fichier temporaire pour le ZIP
    temp_file <- tempfile(fileext = ".zip")
    # Télécharger le fichier ZIP
    download.file(url, temp_file, mode = "wb")
    # Créer un répertoire temporaire pour extraire les fichiers
    temp_dir <- tempdir()
    unzip(temp_file, exdir = temp_dir)
    # Trouver le fichier SHP
    fichier_Commune <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    # Charger le fichier SHP
    BDCommune <- sf::st_read(fichier_Commune)
    # Nettoyer les fichiers temporaires
    unlink(temp_file)
    unlink(temp_dir, recursive = TRUE)
    
    # Reprojection de la couche BDCommune
    BDCommune <- sf::st_transform(BDCommune, crs = 4326)
    cholera <- Data_cholera
    cholera <- cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    
    # Liste des modalités attendues dans Classification
    modalites_attendues <- c("confirmé", "suspect")
    
    # Traitement de la base cholera
    BD_Cholera <- cholera %>%
      dplyr::group_by(Région,Prefecture,Commune2, Classification) %>%  # Regrouper par Commune2 et Classification
      dplyr::summarise(nb_cas = dplyr::n(), .groups = "drop") %>%  # Compter les cas
      tidyr::complete(Classification = modalites_attendues, fill = list(nb_cas = 0)) %>%  # Compléter les modalités manquantes
      tidyr::pivot_wider(
        names_from = Classification,          # Colonnes basées sur Classification
        values_from = nb_cas,                 # Valeurs venant de nb_cas
        values_fill = list(nb_cas = 0)        # Remplir les NA par 0
      ) %>% 
      dplyr::select(Région, Prefecture, Commune2, confirmé, suspect) %>% 
      dplyr::group_by(Région, Prefecture, Commune2) %>% 
      dplyr::summarise(
        confirmé = sum(confirmé, na.rm = TRUE), 
        suspect = sum(suspect, na.rm = TRUE), 
        nb_cas = confirmé + suspect, 
        .groups = "drop"
      )
    # Faire la jointure avec BDCommune
    BD_Cholera01 <- BDCommune %>%
      dplyr::filter(region_id == "A") %>%  # Filtrer par region_id si nécessaire
      dplyr::left_join(BD_Cholera, by = c("commune_no" = "Commune2")) %>% 
      dplyr::mutate(
        nb_cas = ifelse(is.na(nb_cas), 0, nb_cas),
        confirmé = ifelse(is.na(confirmé), 0, confirmé),
        suspect = ifelse(is.na(suspect), 0, suspect)
      )
    
    # Afficher la carte       
    BD_Cholera_sf <- sf::st_as_sf(BD_Cholera01)
    # Déterminer les bornes en fonction des données filtrées
    max_nb_cas <- max(BD_Cholera_sf$nb_cas, na.rm = TRUE)
    max_nb_cas2 <- round(max_nb_cas/2,0)
    max_nb_cas3 <- round(max_nb_cas/3.3,0)
    max_nb_cas4 <- round(max_nb_cas/4,0)
    max_nb_cas5 <- round(max_nb_cas/5,0)
    bins <- unique(c(0, 1, max_nb_cas5, max_nb_cas4, max_nb_cas3, max_nb_cas2, max_nb_cas))
    # Créer une palette dynamique
    palette <- colorBin(
      palette = c("gray", "lightyellow", "yellow", "darkorange", "red", "darkred"), 
      domain = BD_Cholera_sf$nb_cas,          # Domain basé sur les données filtrées
      bins = bins,                           # Bornes dynamiques
      na.color = "transparent"               # Couleur pour NA
    )
    
    # Affichage avec leaflet
    leaflet::leaflet(BD_Cholera_sf) %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(
        fillColor = ~palette(nb_cas),         # Couleurs selon nb_cas
        color = "black",                      # Bordures des polygones
        weight = 1,                           # Épaisseur des bordures
        opacity = 1,                          # Opacité des bordures
        fillOpacity = 0.7,                    # Opacité du remplissage
        popup = ~paste(
          "<b>Commune:</b> ", commune_no, "<br>",
          "<b>Confirmés:</b> ", confirmé, "<br>",
          "<b>Suspects:</b> ", suspect, "<br>",
          "<b>Nombre de Cas:</b> ", nb_cas
        )                                     # Popup affichant des infos
      ) %>%
      leaflet::addLegend(
        pal = palette, 
        values = ~nb_cas, 
        title = "Nombre de Cas", 
        position = "bottomright"
      )
    
    
  })
  
  # Étape 1 : Identifier les valeurs distinctes pour les variables non numériques
  Tab_AFRO_03_react <- reactive({
    cholera <- Data_cholera
    cholera <-cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    Tab_AFRO_03<- cholera %>%
      select(Région, Prefecture) %>%  # Ajouter d'autres variables si nécessaire
      distinct() %>%  rename(
        admin_level_1 = Région,
        admin_level_2 = Prefecture)%>%
      arrange(admin_level_1, admin_level_2)
    return(Tab_AFRO_03)
  })
  # Créer un objet réactif pour stocker Tab_AFRO feuil02
  Tab_AFRO_02_react <- reactive({
    # ==========================================================================================================
    # ==========================================================================================================
    # TABLEAU gtfcc
    cholera <- Data_cholera
    cholera <-cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    cholera$country <- "TOGO"
    cholera <- cholera %>%
      mutate(age_afro = ifelse(age < 5,
                               "suspected_<5",
                               "suspected_≥5"),
             Année = ifelse(Week ==1 & Année ==2024,
                            2025,
                            Année))
    
    # Étape 1 : Calculer le min et max des semaines par année
    weeks_range <- cholera %>%
      group_by(Année) %>%
      summarise(
        min_week = min(Week, na.rm = TRUE),
        max_week = max(Week, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Étape 2 : Générer la grille étendue (expand_grid) par année et semaine
    combinaisons_possibles <- weeks_range %>%
      rowwise() %>%  # Appliquer une opération ligne par ligne
      mutate(Week = list(seq(min_week, max_week))) %>%  # Générer les semaines de chaque année
      select(Année, Week) %>%  # Conserver uniquement Année et la liste des semaines
      unnest(cols = c(Week))  # Déplier la liste en lignes distinctes
    
    # Transformation et agrégation
    TABLE01 <- cholera %>%
      # Classification des résultats culture
      mutate(
        culture_category = case_when(
          grepl("ositif", Résultat.culture) ~ "culture_Pos",
          Résultat.culture == "égatif" ~ "culture_Neg",
          TRUE ~ "culture_NA"
        )
      ) %>%
      group_by(Région, Week,Année) %>%
      summarise(
        nb_case = n(),  # Total des lignes par groupe
        confirmed_culture_pcr = sum(culture_category == "culture_Pos", na.rm = TRUE),
        culture_pcr_neg = sum(culture_category == "culture_Neg", na.rm = TRUE),
        culture_pcr_NA = sum(culture_category == "culture_NA", na.rm = TRUE),
        tested_pos_rdt = sum(Résultat.TDR == "positif", na.rm = TRUE),
        tested_neg_rdt = sum(Résultat.TDR == "négatif", na.rm = TRUE),
        tested_culture_pcr=sum(culture_category != "culture_NA", na.rm = TRUE),
        tested_rdt=sum(Résultat.TDR != "négatif" & Résultat.TDR != "positif"),
        suspect_deaths = sum(Issue == "dcd", na.rm = TRUE),
        Suspect_vivant = sum(Issue != "dcd", na.rm = TRUE),
        `suspected_<5` = sum(age_afro == "suspected_<5", na.rm = TRUE),
        `suspected_≥5` = sum(age_afro == "suspected_≥5", na.rm = TRUE),
        facility_case = sum(Type_fs_comm == "Formation sanitaire", na.rm = TRUE),
        community_case = sum(Type_fs_comm == "Communautaire", na.rm = TRUE),
        facility_deaths = sum(Issue == "dcd" & Type_fs_comm == "Formation sanitaire", na.rm = TRUE),
        community_deaths = sum(Issue == "dcd" & Type_fs_comm == "Communautaire", na.rm = TRUE),
        country="TOGO",
        remarks=""
      ) %>%
      ungroup()
    
    # Complétion des lignes manquantes
    # Étape 1 : Identifier les valeurs distinctes pour les variables non numériques
    valeurs_distinctes <- cholera %>%
      select(country, Région) %>%  # Ajouter d'autres variables si nécessaire
      distinct()
    
    # Fusionner les semaines avec les valeurs distinctes de Région
    combinaisons_complètes <- expand_grid(
      combinaisons_possibles,  # Contient Année et Week
      valeurs_distinctes)      # Contient Région
    
    
    # Étape 3 : Compléter les lignes manquantes dans TABLE01
    TABLE01_complété <- combinaisons_complètes %>%
      full_join(TABLE01, by = c("country", "Région", "Année", "Week")) %>%
      mutate(
        # Remplacer les valeurs manquantes
        across(where(is.numeric), ~ replace_na(., 0)),  # Remplace NA numériques par 0
        Week_day01= (as.Date(paste0(Année, "-01-01")) + (Week - 1) * 7) - as.POSIXlt(as.Date(paste0(Année, "-01-01")) + (Week - 1) * 7)$wday + 1
        
      )
    # Fin
    Tab_AFRO_02 <- TABLE01_complété %>%
      select(
        country, Région, Année, Week,Week_day01, nb_case,
        `suspected_<5`, `suspected_≥5`, tested_culture_pcr, confirmed_culture_pcr,
        tested_rdt, tested_pos_rdt, facility_deaths, community_deaths,remarks
      ) %>%
      arrange(country, Région, Année, Week) %>%  # Tri par colonnes spécifiées
      rename(
        admin_level_1 = Région,
        year = Année
      )
    return(Tab_AFRO_02)
  })
  # Créer un objet réactif pour stocker Tab_AFRO feuil01
  Tab_AFRO_react <- reactive({
    # ==========================================================================================================
    # ==========================================================================================================
    # TABLEAU gtfcc
    cholera <- Data_cholera
    cholera <-cholera %>%
      dplyr::filter(Dateonset >= input$periode[1], Dateonset <= input$periode[2])
    
    if (input$region != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Région == input$region)
    }
    if (input$district != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Prefecture == input$district)
    }
    
    if (input$type != "Toutes") {
      cholera <- cholera %>%
        dplyr::filter(Classification == input$type)
    }
    cholera$country <- "TOGO"
    cholera <- cholera %>% 
      mutate(age_afro = ifelse(age < 5,
                               "suspected_<5", 
                               "suspected_≥5"),
             Année = ifelse(Week ==1 & Année ==2024,
                            2025, 
                            Année))
    
    # Étape 1 : Calculer le min et max des semaines par année
    weeks_range <- cholera %>%
      group_by(Année) %>%
      summarise(
        min_week = min(Week, na.rm = TRUE),
        max_week = max(Week, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Étape 2 : Générer la grille étendue (expand_grid) par année et semaine
    combinaisons_possibles <- weeks_range %>%
      rowwise() %>%  # Appliquer une opération ligne par ligne
      mutate(Week = list(seq(min_week, max_week))) %>%  # Générer les semaines de chaque année
      select(Année, Week) %>%  # Conserver uniquement Année et la liste des semaines
      unnest(cols = c(Week))  # Déplier la liste en lignes distinctes
    unique(cholera$Résultat.culture)
    # Transformation et agrégation
    TABLE01 <- cholera %>%
      # Classification des résultats culture
      mutate(
        culture_category = case_when(
          grepl("ositif", Résultat.culture) ~ "culture_Pos",
          grepl("égatif", Résultat.culture) ~ "culture_Neg",
          TRUE ~ "culture_NA"
        )
      ) %>%
      group_by(Région, Prefecture, Week,Année) %>%
      summarise(
        nb_case = n(),  # Total des lignes par groupe
        confirmed_culture_pcr = sum(culture_category == "culture_Pos", na.rm = TRUE),
        culture_pcr_neg = sum(culture_category == "culture_Neg", na.rm = TRUE),
        culture_pcr_NA = sum(culture_category == "culture_NA", na.rm = TRUE),
        tested_pos_rdt = sum(Résultat.TDR == "positif"|Résultat.TDR == "POSITIF"|Résultat.TDR == "Positif", na.rm = TRUE),
        tested_neg_rdt = sum(Résultat.TDR == "négatif"|Résultat.TDR == "Négatif"|Résultat.TDR == "NEGATIF", na.rm = TRUE),
        tested_culture_pcr=sum(culture_category == "culture_Pos"|culture_category == "culture_Neg", na.rm = TRUE),
        tested_rdt=sum(Résultat.TDR != "NON fait" | is.na(Résultat.TDR)),
        suspect_deaths = sum(Issue == "dcd", na.rm = TRUE),
        Suspect_vivant = sum(Issue != "dcd", na.rm = TRUE),
        `suspected_<5` = sum(age_afro == "suspected_<5", na.rm = TRUE),
        `suspected_≥5` = sum(age_afro == "suspected_≥5", na.rm = TRUE),
        facility_case = sum(Type_fs_comm == "Formation sanitaire", na.rm = TRUE),
        community_case = sum(Type_fs_comm == "Communautaire", na.rm = TRUE),
        facility_deaths = sum(Issue == "dcd" & Type_fs_comm == "Formation sanitaire", na.rm = TRUE),
        community_deaths = sum(Issue == "dcd" & Type_fs_comm == "Communautaire", na.rm = TRUE),
        country="TOGO",
        remarks=""
      ) %>%
      ungroup()
    
    # Complétion des lignes manquantes
    # Étape 1 : Identifier les valeurs distinctes pour les variables non numériques
    valeurs_distinctes <- cholera %>%
      select(country, Région, Prefecture) %>%  # Ajouter d'autres variables si nécessaire
      distinct()
    
    # Fusionner les semaines avec les valeurs distinctes de Région et Prefecture
    combinaisons_complètes <- expand_grid(
      combinaisons_possibles,  # Contient Année et Week
      valeurs_distinctes)      # Contient Région et Prefecture
    
    
    # Étape 3 : Compléter les lignes manquantes dans TABLE01
    TABLE01_complété <- combinaisons_complètes %>%
      full_join(TABLE01, by = c("country", "Région", "Prefecture", "Année", "Week")) %>%
      mutate(
        # Remplacer les valeurs manquantes
        across(where(is.numeric), ~ replace_na(., 0)),  # Remplace NA numériques par 0
        Week_day01= (as.Date(paste0(Année, "-01-01")) + (Week - 1) * 7) - as.POSIXlt(as.Date(paste0(Année, "-01-01")) + (Week - 1) * 7)$wday + 1
      )
    # Fin
    Tab_AFRO <- TABLE01_complété %>%
      select(
        country, Région, Prefecture, Année, Week, Week_day01, nb_case, 
        `suspected_<5`, `suspected_≥5`, tested_culture_pcr, confirmed_culture_pcr, 
        tested_rdt, tested_pos_rdt, facility_deaths, community_deaths,remarks
      ) %>%
      arrange(country, Région, Prefecture, Année, Week) %>%  # Tri par colonnes spécifiées
      rename(
        admin_level_1 = Région,
        admin_level_2 = Prefecture,
        year = Année
      )                
    return(Tab_AFRO)
  })
  output$table_afro <- render_gt({
    #write.xlsx(Tab_AFRO, "Tab_AFRO.xlsx",sheetName="Togo_data", asTable = TRUE)
    Tab_AFRO_react() %>%
      gt() %>%
      tab_header(
        title = md("**<span style='color:black; font-weight:bold;'>Tableau des Cas de choléra : TOGO_GTFCC_Template</span>**")
      ) %>%
      tab_options(
        table.width = pct(75),  # Ajuster la largeur de la table à 100%
        table.align = "center",  # Centrer la table
        table.font.size = px(12),
        # Mettre les lignes horizontales en noir
        table_body.hlines.color = "black",
        table_body.border.bottom.color = "black",
        table_body.border.top.color = "black")%>%
      # Mettre en gras les entêtes des colonnes
      tab_style(
        style = cell_text(weight = "bold"), # Appliquer du texte en gras
        locations = cells_column_labels(
          columns = everything()
        )
      ) %>% 
      # Aligner la première colonne à gauche
      tab_style(
        style = cell_text(align = "left"),
        locations = cells_body(
          columns = 1
        )
      )%>% 
      # Aligner la première colonne à gauche
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(
          columns = 4:last_col()
        )
      )
    
    
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("TOGO_gtfcc_weekly_reporting_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Charger un fichier Excel existant à partir du répertoire (remplacer 'chemin_du_fichier.xlsx' par le chemin de votre fichier existant)
      # URL brute du fichier sur GitHub (utiliser le lien "raw")
      url <- "https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/data/TOGO_gtfcc_weekly_reporting.xlsx"
      # Télécharger temporairement le fichier
      temp_file <- tempfile(fileext = ".xlsx")
      download.file(url, temp_file, mode = "wb")
      # Charger le fichier Excel
      wb <- openxlsx::loadWorkbook(temp_file)
      
      # Supprimer le fichier temporaire après usage (optionnel)
      unlink(temp_file)
      
      # Créer une liste de données avec les noms des feuilles
      sheets <- list(
        "Preferred_Weekly_Dataset " = Tab_AFRO_react(),
        "Minimum_Weekly_Dataset" = Tab_AFRO_02_react(),
        "List of Units" = Tab_AFRO_03_react()
      )
      
      # Ajouter les nouvelles feuilles au classeur existant# Ajouter ou supprimer les feuilles
      for (sheet_name in names(sheets)) {
        # Vérifier si la feuille existe déjà, et la supprimer si nécessaire
        if(sheet_name %in% names(wb)) {
          removeWorksheet(wb, sheet_name)
        }
        # Ajouter la nouvelle feuille sans modifier les styles existants
        addWorksheet(wb, sheetName = sheet_name, gridLines = TRUE)  # L'ajout de gridLines est optionnel mais souvent utile
        writeData(wb, sheet = sheet_name, x = sheets[[sheet_name]], startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
      }
      
      # Sauvegarder le classeur modifié
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
}
