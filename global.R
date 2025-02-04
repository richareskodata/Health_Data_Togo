tempdir <- tempdir()  # Vérifie l'emplacement actuel du dossier temporaire
unlink(tempdir, recursive = TRUE)  # Supprime tout le contenu
dir.create(tempdir, showWarnings = FALSE)  # Crée un nouveau dossier propre
source("R/install.R")
source("R/importation_data.R")
source("R/traitements.R")
source("R/fonctions_utiles.R")
