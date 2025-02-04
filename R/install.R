# Configuration du miroir CRAN par défaut
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Liste des packages à installer
packages <- c(
  # Packages Shiny et interface utilisateur
  "shiny", "shinydashboard", "DT", "highcharter", "leaflet", "plotly", "gt", "Cairo",
  # Manipulation de données
  "dplyr", "tidyr", "reshape2", "readxl", "openxlsx", "rio", "zoo", "lubridate", "stringi", "stringr",
  # Visualisation de données
  "ggplot2", "tmap", "pyramid", "forcats", "colorspace", "raster", "terra", "sf", "lwgeom",
  # Analyse de données et statistiques
  "forecast", "questionr", "Hmisc", "climate", "clifro",
  # Connexion à des bases de données
  "DBI", "odbc", "RODBC",
  # Déploiement et développement
  "rsconnect", "devtools", "remotes", "lintr", "curl", "magrittr"
)

# Installation des packages manquants
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Appliquer la fonction à tous les packages
invisible(sapply(packages, install_if_missing))

# Installation de packages spécifiques depuis GitHub
if (!requireNamespace("climate", quietly = TRUE)) {
  remotes::install_github("bczernecki/climate", quiet = TRUE, dependencies = TRUE, upgrade = "never")
}

# Chargement des packages
invisible(lapply(packages, library, character.only = TRUE))
