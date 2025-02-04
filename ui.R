# Interface utilisateur (UI)
ui <- dashboardPage(
  dashboardHeader(
    title = "Togo Health Data",
    dropdownMenu(
      type = "messages",
      badgeStatus = "primary",
      messageItem(from = "Support", message = "Mise à jour de la version")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de Bord", tabName = "dashboard", icon = icon("dashboard")),
      
      menuItem("Analyse de données", icon = icon("line-chart"),
               tags$head(tags$style(HTML("
                    .shiny-input-container label {
                      font-size:12px;
                      font-weight: bold;
                    }
                    .shiny-input-container .form-control {
                      font-size: 10px; 
                      font-family: 'Arial', sans-serif;
                    }
                  "))
               ),
               dateRangeInput("periode", label = "Période d'analyse",
                              start = Sys.Date() - 350,
                              min = "2024-01-01",
                              max = Sys.Date(),
                              end = Sys.Date() - 1,
                              separator = "<=>", format = "yyyy-mm-dd",
                              startview = 'year', language = 'fr'),
               menuSubItem("Données maladies", tabName = "Data_analysis", icon = icon("bacterium")),
               menuSubItem("Données Climat", tabName = "Climate_analysis", icon = icon("cloud-rain")),
               selectInput("region", label = "Sélectionnez la région",
                           choices = c("Toutes", "Grand Lomé", "Maritime", "Plateaux", 
                                       "Centrale", "Kara", "Savanes"), selected = "Toutes"),
               selectInput("district", label = "Sélectionnez le district",
                           choices = c("Toutes", "Agoè-Nyivé", "Golfe", "Avé", 
                                       "Bas-Mono", "Lacs", "Vo", "Yoto", "Zio"), selected = "Toutes"),
               selectInput("type", label = "Type Lab",
                           choices = c("Toutes", "confirmé", "suspect"), selected = "Toutes")
      ),
      
      menuItem("Documents utiles", icon = icon("book"),
               menuSubItem("Guide SIMR3 Section I", tabName = "guide_simr3_1", icon = icon("file-pdf")),
               menuSubItem("Guide SIMR3 Section XI", tabName = "guide_simr3_11", icon = icon("file-pdf")),
               menuSubItem("Guide SIMR2", tabName = "guide_simr2", icon = icon("file-pdf")),
               menuSubItem("Bulletins", tabName = "Bulletins", icon = icon("file-pdf"))
      ),
      menuItem("Liens utils", icon = icon("book"),
               menuSubItem("Formulaire absence RH", tabName = "forms_RH", icon = icon("user")),
               menuSubItem("LEAVES DASHBOARD", tabName = "db_RH", icon = icon("user"))
               
      ),
      
      menuItem("À propos", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
          .btn-lg { 
            width: 100% !important; 
            font-size: 18px !important; 
          }
        "))
    ),
    tabItems(
      # Onglet - Tableau de bord
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Tableau de Bord", width = 12, solidHeader = TRUE,
                    p("Bienvenue sur le tableau de bord. Sélectionnez une option dans le menu latéral."))
              )
      ),
      
      # Onglet - Analyse de données
      tabItem(tabName = "Data_analysis",
              fluidRow(
                box(width = 12, solidHeader = TRUE,
                    tags$div(class = "btn-group", role = "group",
                             style = "display: flex; width: 100%; justify-content: space-between;",
                             actionButton("Synthese_btn", "Synthèse", class = "btn btn-primary", 
                                          style = "font-size: calc(1vw + 5px); width: 19%;"),
                             actionButton("cholera_btn", "Choléra", class = "btn btn-primary", 
                                          style = "font-size: calc(1vw + 5px); width: 19%;"),
                             actionButton("dengue_btn", "Dengue", class = "btn btn-primary", 
                                          style = "font-size: calc(1vw + 5px); width: 19%;"),
                             actionButton("rougeole_btn", "Rougeole", class = "btn btn-primary", 
                                          style = "font-size: calc(1vw + 5px); width: 19%;"),
                             actionButton("polio_btn", "Polio", class = "btn btn-primary", 
                                          style = "font-size: calc(1vw + 5px); width: 19%;")
                    )
                )),
              uiOutput("analysis_btn"),
              uiOutput("analysis_content")
      ),
      
      # Onglets pour les documents PDF
      tabItem(tabName = "guide_simr3_1",
              fluidRow(
                box(title = "Guide Technique pour la Surveillance Intégrée de la Maladie et la Riposte dans la Région Africaine édition 3 (SIMR3) : Section I - Introduction", width = 12, solidHeader = TRUE,
                    tags$iframe(style = "width:100%; height:700px;",
                                src = "SIMR3_I_Introduction.pdf")
                )
              )
      ),
      tabItem(tabName = "guide_simr3_11",
              fluidRow(
                box(title = "Guide SIMR3 Section XI : Résumé des directives relatives à des maladies et affections prioritaires spécifiques", width = 12, solidHeader = TRUE,
                    tags$iframe(style = "width:100%; height:700px;",
                                src = "SIMR3_XI_Directives.pdf")
                )
              )
      ),
      tabItem(tabName = "guide_simr2",
              fluidRow(
                box(title = "Guide Technique pour la Surveillance Intégrée de la Maladie et la Riposte dans la Région Africaine édition 2", width = 12, solidHeader = TRUE,
                    tags$iframe(style = "width:100%; height:700px;",
                                src = "SIMR2.pdf")
                )
              )
      ),
      tabItem(tabName = "Bulletins",
              fluidRow(
                box(title = "Bulletins", width = 12, solidHeader = TRUE,
                    tags$iframe(style = "width:100%; height:700px;",
                                src = "Situation_Urgences_Sanitaires_S50.pdf")
                )
              )
      ),
      tabItem(tabName = "forms_RH",
              fluidPage(
                h3("Formulaire de planification des congés, absences et missions hors lieu de déploiement"),
                tags$iframe(src = "https://forms.office.com/e/Tm0ejuur8y", 
                            width = "100%", 
                            height = "700px", 
                            frameborder = 0)
              )),
      tabItem(tabName = "db_RH",
              fluidPage(
                h3("Tableau de bord de planification des congés, absences et missions hors lieu de déploiement"),
                tags$iframe(src = "https://app.powerbi.com/view?r=eyJrIjoiNzk5ZTg0ZmItOTA3My00MTdhLWE1MDItZjRiZDM3NDg5NjM1IiwidCI6ImY2MTBjMGI3LWJkMjQtNGIzOS04MTBiLTNkYzI4MGFmYjU5MCIsImMiOjh9", 
                            width = "100%", 
                            height = "700px", 
                            frameborder = 0)
              )),
      
      # Onglet - À propos
      tabItem(tabName = "about",
              fluidRow(
                box(title = "À propos de cette application", width = 12, solidHeader = TRUE,
                    p("Ce tableau de bord est construit avec Shiny et shinydashboard pour analyser les données de santé du Togo."))
              )
      )
    )
  )
)
