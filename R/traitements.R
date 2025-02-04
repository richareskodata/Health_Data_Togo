# Traitement Données liste linéaire choléra ============
Data_cholera$Datenotif <- as.Date(Data_cholera$`Date de consultation`)
Data_cholera$Dateonset <- as.Date(Data_cholera$`Date de début des signes`)
Data_cholera$Année <- year(Data_cholera$Dateonset)
Data_cholera$Dateout <- as.Date(Data_cholera$`Date de Sortie`)
Data_cholera$age <- as.integer(Data_cholera$`Age (année)`)
Data_cholera$Week <- isoweek(Data_cholera$`Date de début des signes`)
Data_cholera$Issue <- Data_cholera$`Mode de sortie (Guéri/Référé/dcd)`
Data_cholera$Classification <- Data_cholera$`Classification finale (Suspect/Probable/Confirmé)`

Data_cholera$n <- 1
## Recodage de Data_cholera$`Tranche d'age`
Data_cholera$`Tranche d'age` <- Data_cholera$`Tranche d'age` %>%
  fct_recode(
    "[0-2[" = "[0-2]",
    "[15-45[" = "[15-44]",
    "[2-5[" = "[2-4]",
    "[45-60[" = "[45-59]",
    "[5-15[" = "[5-14]",
    "[60 +[" = "[60 et plus]"
  )

## Réordonnancement de Data_cholera$`Tranche d'age` en Data_cholera$Tranche_age
Data_cholera$Tranche_age <- Data_cholera$`Tranche d'age` %>%
  fct_relevel(
    "[0-2[", "[2-5[", "[5-15[", "[15-45[", "[45-60[", "[60 +["
  )

Data_cholera$Tranche_age <- Data_cholera$`Tranche d'age`

Data_cholera$Région <- Data_cholera$Région %>% 
  fct_recode("Maritime"="MARITIME")
## Recodage de Data_cholera$`Principale source d’eau de boisson` en Data_cholera$`Principale source d’eau de boisson_rec`
Data_cholera$source_eau <- Data_cholera$`Principale source d’eau de boisson` %>%
  fct_recode(
    "Puits" = "eau de puits",
    "Puits" = "Eau de puits",
    "Rivière" = "Eau de rivière",
    "Robinet" = "eau de robinet",
    "Robinet" = "Eau de robinet, Eau de puits",
    "Robinet" = "eau de robonet",
    "Bouteille" = "Eau en bouteille",
    "Sachet" = "Eau en sachet",
    "Robinet" = "eu de robinet",
    "Robinet" = "Tde",
    "Forage" = "Forage/Puits",
    NULL = "NION",
    NULL = "Non",
    NULL = "NON",
    NULL = "Oui",
    NULL = "OUI",
    "Puits" = "puits",
    "Puits" = "puits peu profod; eau en bouteille",
    "Forage" = "Tde/Forage",
    "Robinet" = "TdE+Eau de fleuve"
  )
# Vérifier et recoder en ignorant les modalités inexistantes


Data_cholera$Profession <- Data_cholera$Profession %>%
  fct_recode(
    "Artisanat" = "APPRENTI ELECTRICIEN", 
    "Artisanat" = "Mécanicien",
    "Artisanat" = "Electricien",
    "Artisanat" = "ELECTRO TECHNICIEN",
    "autres" = "APPRENTI HERBORISTE", 
    "Chauffeur" = "Zémidjan/Chauffeur",
    "autres" = "Boulangère",
    "Artisanat" = "Staffeur",
    "Boucher" = "CHARCUTIER",
    "Chauffeur" = "Chauffeur",
    "Chauffeur"="Apprenti Chauffeur",
    "Boucher" = "Boucher à Zongo",
    "Chauffeur" = "Condusteur Tricycle", 
    "Artisanat" = "DESSINATEUR BATIMENT", 
    "Enseignant(e)" = "ENSEIGNANT",
    "Enseignant(e)" ="Enseignante",
    "Enseignant(e)" = "Enseignant au CSI de Zongo",
    "Enseignant(e)" = "EnseignantE à l'Ecole privée Islamique Albarka de Zongo",
    "Enseignant(e)" = "Enseignant",
    "autres" = "MASSEUSE", 
    "Artisanat" = "MECANICIEN AUTO", 
    "Employé" = "DECLARANT EN DOUANE", 
    "Employé" = "JOURNALISTE", 
    "Artisanat" = "SOUDEUR", 
    "Chauffeur" = "TAXI MOTO", 
    "autres" = "Cuisinier", 
    "Artisanat" = "Soudeur", 
    "autres" = "Humanitaire",
    "Employé" = "Technicienne de surface", 
    "Employé" = "Informaticien", 
    "Employé" = "Docker à l'ancien port de pêche", 
    "chômeur" = "SANS PROFESSION",
    "chômeur" = "Sans emploi",
    "autres" = "OUVRIER",
    "autres" = "Coursier",
    "autres" = "MARCHAND",
    "Artisanat" = "APPRENTI MACON", 
    "Artisanat" = "MACON", 
    "Artisanat" = "Maçon", 
    "Chauffeur" = "CHAUFFEUR", 
    "Commerçant(e)" = "revendeur", 
    "Commerçant(e)" = "Revendeur", 
    "Commerçant(e)" = "REVENDEUR", 
    "Commerçant(e)" = "Revendeuse",
    "Commerçant(e)" = "REVENDEUSE",
    "Commerçant(e)" = "Revendeur au GM",
    "Commerçant(e)" = "Revendeur de vètements",
    "Employé" = "Agent de sécurité",
    "Employé" = "Agent commercial",
    "Artisanat" = "COIFFEUR",
    "Artisanat" = "COIFFEUSE",
    "Artisanat" = "Coiffure",
    "Artisanat" = "Apprenti couturière",
    "Artisanat" = "Coiffeuse/ménagère", 
    "Artisanat" = "Tresse", 
    "Commerçant(e)" = "Commerçant",
    "Commerçant(e)" = "Commercante", 
    "Artisanat" = "COUTURIERE", 
    "Artisanat" = "Couturière", 
    "Artisanat" = "Couture",
    "Cultivateur" = "CULTIVATEUR", 
    "Cultivateur" = "Cultivatrice", 
    "Pêcheur" = "PECHEUR", 
    "Pêcheur" = "pêcheur", 
    "Pêcheur" = "PECHEUSE", 
    "Elève" = "ECOLIER", 
    "Elève" = "ECOLIERE",
    "Elève" = "Elève (Ecole Islamique)",
    "Elève" = "Eleve", 
    "Elève" = "ELEVE",
    "Elève" = "Elève à l'Ecole Albarka",
    "Elève" = "Elève à l'Ecole Al Houda et vie avec les enfants de l'école Albarka",
    "Enfant" = "Nouveau Né",
    "Enfant" = "NA (Enfant)", 
    "Etudiant" = "ETUDIANT", 
    "Etudiant" = "Etudiante", 
    "Militaire" = "Militaire retraité", 
    "Retraité" = "Retraitée", 
    "Retraité" = "RETRAITE", 
    "Menagère" = "MENAGERE", 
    "Menagère" = "Domestique", 
    "Menagère" = "Ménagère", 
    "Artisanat" = "MENUISIER", 
    "Artisanat" = "MENUISIER ALU", 
    "Artisanat" = "Menuisier Alu", 
    "N/A" = "NA"
  ) %>%
  fct_na_value_to_level("N/A")
# unique(Data_cholera$Profession)

Data_cholera$Issue01 <- Data_cholera$Issue %>%
  fct_recode(
    "vivant" = "Guéri",
    "décès" = "dcd"
  ) %>% fct_na_value_to_level("vivant")

Data_cholera <- Data_cholera[ , !(names(Data_cholera) %in%  "Nom et Prénoms")]
Data_cholera <- Data_cholera[ , !(names(Data_cholera) %in%  "N° de Téléphone")]


#write.table(Data_cholera, "C:/Users/dzidzinyok/OneDrive - World Health Organization/RICHARD DZIDZINYO/R/DRAFT SHINY/Liste_lineaire", fileEncoding = "UTF-8")


# Traitement Données cas par cas polio / PFA =====
Poliocases <- Polio_all %>% subset(!grepl("C1|C2|C3|C4|C5|C6|C7|C8|C9", EpidNumber))
Poliocases$WeekOnset <- isoweek(as.Date(Poliocases$DateOfOnset))
Poliocases$AnnéeOfOnset <- year(Poliocases$DateOfOnset)
Poliocases$AnnéeNotif <- year(Poliocases$DateNotified)
Poliocases <- Poliocases %>%
  mutate_if(is.character, as.factor)

# Liste des ID et des Districts associés
district_patterns <- data.frame(
  Pattern = c("-MAR-AGE-", "-RGL-AGE-", "-RGL-GOL-", "-MAR-GOL-", "TOG-LCO-", "TOG-LCO-DN5",
              "-MAR-AVE-", "-MAR-BMO-", "-MAR-LAC-", "-MAR-VO-", "-MAR-VO -", "-MAR-YOT-", "-MAR-ZIO-",
              "-PLA-AGO-", "-PLA-AKE-", "-PLA-AMO-", "-PLA-ANI-", "-PLA-DAN-", "-PLA-EST-", "-PLA-HAH-",
              "-PLA-KLO-", "-PLA-KPE-", "-PLA-KLE-", "-PLA-MOY-", "-PLA-OGO-", "-PLA-WAW-", "-CEN-BLI-",
              "-CEN-MO-", "-CEN-MÔ -", "-CEN-MO -", "-CEN-SOT-", "-CEN-TCB-", "-CEN-TCH-", "-KAR-ASS-",
              "-KAR-BAS-", "-KAR-BIN-", "-KAR-DAK-", "-KAR-DOU-", "-KAR-KER-", "-KAR-KOZ-", "-SAV-CIN-",
              "-SAV-KPE-", "-SAV-KPO-", "-SAV-OTI-", "-SAV-OTS-", "-SAV-TAN-", "-SAV-TON-"),
  District = c("Agoè-Nyivé", "Agoè-Nyivé", "Golfe", "Golfe", "Golfe", "Agoè-Nyivé", "Avé", "Bas-Mono", "Lacs", "Vo", "Vo",
               "Yoto", "Zio", "Agou", "Akébou", "Amou", "Anié", "Danyi", "Est-Mono", "Haho", "Kloto",
               "Kpélé", "Kpélé", "Moyen-Mono", "Ogou", "Wawa", "Blitta", "Mô", "Mô", "Mô", "Sotouboua",
               "Tchamba", "Tchaoudjo", "Assoli", "Bassar", "Binah", "Dankpen", "Doufelgou", "Kéran", "Kozah",
               "Cinkassé", "Kpendjal", "Kpendjal-Ouest", "Oti", "Oti-Sud", "Tandjoaré", "Tône")
)

# Appliquer la logique pour assigner les districts
Poliocases$District_resid <- NA  # Initialiser avec NA par défaut

for (i in seq_len(nrow(district_patterns))) {
  Poliocases$District_resid[grepl(district_patterns$Pattern[i], Poliocases$EpidNumber)] <- district_patterns$District[i]
}
# Liste des motifs et des Régions associés
region_patterns <- data.frame(
  Pattern = c("-MAR-AGE-", "-RGL-AGE-", "-RGL-GOL-", "-MAR-GOL-", "TOG-LCO-", "TOG-LCO-DN5",
              "-MAR-AVE-", "-MAR-BMO-", "-MAR-LAC-", "-MAR-VO-", "-MAR-VO -", "-MAR-YOT-", "-MAR-ZIO-",
              "-PLA-AGO-", "-PLA-AKE-", "-PLA-AMO-", "-PLA-ANI-", "-PLA-DAN-", "-PLA-EST-", "-PLA-HAH-",
              "-PLA-KLO-", "-PLA-KPE-", "-PLA-KLE-", "-PLA-MOY-", "-PLA-OGO-", "-PLA-WAW-", "-CEN-BLI-",
              "-CEN-MO-", "-CEN-MÔ -", "-CEN-MO -", "-CEN-SOT-", "-CEN-TCB-", "-CEN-TCH-", "-KAR-ASS-",
              "-KAR-BAS-", "-KAR-BIN-", "-KAR-DAK-", "-KAR-DOU-", "-KAR-KER-", "-KAR-KOZ-", "-SAV-CIN-",
              "-SAV-KPE-", "-SAV-KPO-", "-SAV-OTI-", "-SAV-OTS-", "-SAV-TAN-", "-SAV-TON-"),
  region = c("Grand Lomé", "Grand Lomé", "Grand Lomé", "Grand Lomé", "Grand Lomé", "Grand Lomé", "Maritime", "Maritime", "Maritime", "Maritime", "Maritime",
             "Maritime", "Maritime", "Plateaux", "Plateaux", "Plateaux", "Plateaux", "Plateaux", "Plateaux", "Plateaux", "Plateaux",
             "Plateaux", "Plateaux", "Plateaux", "Plateaux", "Plateaux", "Centrale", "Centrale", "Centrale", "Centrale", "Centrale",
             "Centrale", "Centrale", "Kara", "Kara", "Kara", "Kara", "Kara", "Kara", "Kara",
             "Savanes", "Savanes", "Savanes", "Savanes", "Savanes", "Savanes", "Savanes")
)
# Appliquer la logique pour assigner les région
Poliocases$Région <- NA  # Initialiser avec NA par défaut

for (i in seq_len(nrow(region_patterns))) {
  Poliocases$Région[grepl(region_patterns$Pattern[i], Poliocases$EpidNumber)] <- region_patterns$region[i]
}
# Création de variable utiles surveillance PFA
Poliocases <- Poliocases %>%
  mutate(ResultatFinal_Polio = ifelse(FinalClassification == "7", 
                                      "Confirmé_Polio", 
                                      "Suspect_PFA"))
Poliocases <- Poliocases %>%
  mutate(ADECATION = ifelse(
    (as.numeric(Date2ndStool - Date1stStool) >= 1 & 
       as.numeric(Date2ndStool - DateOfOnset) <= 14 & 
       StoolCondition == "1"),
    "Adéquat", 
    "Inadéquat"
  ))
Poliocases <- Poliocases %>%
  mutate(PromptAcheminEchant_3jrsNatLab = ifelse(
    (StoolCondition == "1" & 
       as.numeric(DateSpecRecbyNatLab - Date2ndStool) <= 3 & 
       as.numeric(DateSpecRecbyNatLab - Date2ndStool) >= 0),
    "Prompt",
    "Non prompt"
  ))
Poliocases <- Poliocases %>%
  mutate(PromptActivitTerrain11jrs = ifelse(
    as.numeric(Date2ndStool - Date1stStool) <= 1 & 
      as.numeric(Date2ndStool - DateOfOnset) <= 11,
    "Prompt",
    "Non prompt"
  ))
Poliocases <- Poliocases %>%
  mutate(DelaiResultFinalLabo = as.numeric(DateFinalCellcultureResults - DateOfOnset))

Poliocases <- Poliocases %>%
  mutate(DetectionPFASysteme = ifelse
         (as.numeric(DateFinalCellcultureResults - DateOfOnset) <= 35,
           "Prompt",
           "Non prompt"
         ))
Poliocases <- Poliocases %>%
  mutate(DetectionPolioSysteme = ifelse
         (as.numeric(DateFinalCellcultureResults - DateOfOnset) <= 35 & FinalClassification == "7",
           "Prompt",
           "Non prompt"
         ))
Poliocases <- Poliocases %>%
  mutate(Notification7jrs = ifelse
         (as.numeric(DateNotified - DateOfOnset) <= 7,
           "Prompt",
           "Non prompt"
         ))
Poliocases <- Poliocases %>%
  mutate(Investigation48h_Notif = ifelse
         (as.numeric(DateCaseinvestigated - DateNotified) <= 2,
           "Prompt",
           "Non prompt"
         ))
# Calculer le nombre de districts distincts
DistrictNotifs <- Poliocases %>% 
  summarise(DistrictCount = n_distinct(District_resid)) %>% 
  pull(DistrictCount) %>% as.numeric()

Poliocases <- Poliocases %>%
  mutate_if(is.character, as.factor)
rm(Polio_all,district_patterns,region_patterns)

# Traitement Données cas par cas Rougeole ==========
Rougeole_cases <- Rougeole_all
Rougeole_cases$Typecas <- ifelse(Rougeole_cases$MeaslesIgm == 1, "confirmé", "suspect")
Rougeole_cases <- Rougeole_cases %>%
  mutate_if(is.character, as.factor)
Rougeole_cases$AnnéeOfonset <- year(Rougeole_cases$DateOfonset)
Rougeole_cases$AnnéeNotif <- year(Rougeole_cases$DateHealthfacilitynotified)
Rougeole_cases$WeekOnset <- isoweek(as.Date(Rougeole_cases$DateOfonset))
Rougeole_cases$WeekOnset <- as.factor(Rougeole_cases$WeekOnset)
Rougeole_cases$WeekNotif <- isoweek(as.Date(Rougeole_cases$DateHealthfacilitynotified))
Rougeole_cases$WeekNotift <- as.factor(Rougeole_cases$DateHealthfacilitynotified)
# Création d'une nouvelle colonne pour le statut vaccinal
Rougeole_cases$Statut_Vaccinal <- ifelse(
  Rougeole_cases$NumberOfVaccinedoses == 0, 
  "Non vaccinée",
  ifelse(
    Rougeole_cases$NumberOfVaccinedoses > 7, 
    "Inconnu", "Vaccinée"))

Rougeole_cases$Statut_Vaccinal01 <- ifelse(
  Rougeole_cases$NumberOfVaccinedoses == 0, 
  "Non vaccinée", ifelse(Rougeole_cases$NumberOfVaccinedoses > 7, 
                         "Inconnu",ifelse(Rougeole_cases$NumberOfVaccinedoses == 1, 
                                          "Vaccinée RR1", "Vaccinée RR2")))
# Création de la colonne Type_cas en fonction des conditions
Rougeole_cases$Type_cas <- ifelse(
  Rougeole_cases$MeaslesIgm == "1", "Confirmé IgM+",
  ifelse( Rougeole_cases$MeaslesIgm == "2", "Suspect",
          ifelse( Rougeole_cases$MeaslesIgm == "3", "Indéterminé",
                  ifelse( Rougeole_cases$MeaslesIgm == "4", "Non fait",
                          ifelse(Rougeole_cases$MeaslesIgm == "5", "En attente Labo",
                                 "Indéterminé") ))))

# Liste des motifs et des districts associés
district_patterns <- data.frame(
  Pattern = c("-MAR-GOL-", "RGL-GOL", "-LCO-", "-LCO-DN5", "-MAR-AGE-", "TOG-AGE-AGE-", "-RGL-AGE-", 
              "-MAR-AVE-", "-MAR-BMO-", "-MAR-LAC-", "-PLA-LAC-", "-MAR-VO-", "-MAR- VO-", "-MAR-VO -", "-MAR-V0-",
              "-MAR-YOT-", "-MAR-ZIO-", "-MAR-ZIO -", "-PLA-AGO-", "-PLA-AKE-", "-PLA-AMO-", "-PLA-ANI-", "-PLA-DAN-",
              "-PLA-EST-", "-PLA-HAH-", "-MAR-HAH-", "-PLA-KLO-","-MAR-KLO-", "-PLA-KPE-", "-PLA-KLE-", "-PLA-MOY-", "-CEN-MOY-",
              "-PLA-OGO-", "-PLA-WAW-", "-CEN-BLI-", "-CEN-MO-", "-CEN-MÔ -", "-CEN-MO -", "-CEN-SOT-", "-CEN-TCB-", 
              "-CEN-TCH-", "-KAR-ASS-", "-KAR-BAS-", "KAR-BAS-", "-KAR-BIN-", "-KAR-DAK-", "-KAR-DOU-", "-KAR-KER-",
              "-KAR-KOZ-", "-SAV-CIN-", "-SAV-KPE-", "-SAV-KPO-", "-SAV-OTI-", "-SAV-OTS-","-SAV-OTS ", "-SAV-TAN-", "-SAV-TON-"),
  District = c("Golfe", "Golfe", "Golfe", "Agoè-Nyivé", "Agoè-Nyivé", "Agoè-Nyivé", "Agoè-Nyivé", 
               "Avé", "Bas-Mono", "Lacs", "Lacs", "Vo", "Vo", "Vo", "Vo",
               "Yoto", "Zio", "Zio", "Agou", "Akébou", "Amou", "Anié", "Danyi",
               "Est-Mono", "Haho", "Haho", "Kloto","Kloto", "Kpélé", "Kpélé", "Moyen-Mono", "Moyen-Mono",
               "Ogou", "Wawa", "Blitta", "Mô", "Mô", "Mô", "Sotouboua", "Tchamba",
               "Tchaoudjo", "Assoli", "Bassar", "Bassar", "Binah", "Dankpen", "Doufelgou", "Kéran",
               "Kozah", "Cinkassé", "Kpendjal", "Kpendjal-Ouest", "Oti", "Oti-Sud","Oti-Sud", "Tandjoaré", "Tône")
)

# Ajout de la colonne District associée
Rougeole_cases$District_resid <- NA  # Initialiser avec NA

# Boucle pour assigner les districts basés sur les motifs trouvés
for (i in seq_len(nrow(district_patterns))) {
  Rougeole_cases$District_resid[grepl(district_patterns$Pattern[i], Rougeole_cases$IDNumber)] <- district_patterns$District[i]
}
# Liste des motifs et des région associés
region_patterns <- data.frame(
  Pattern = c("-MAR-GOL-", "RGL-GOL", "-LCO-", "-LCO-DN5", "-MAR-AGE-", "TOG-AGE-AGE-", "-RGL-AGE-", 
              "-MAR-AVE-", "-MAR-BMO-", "-MAR-LAC-", "-PLA-LAC-", "-MAR-VO-", "-MAR- VO-", "-MAR-VO -", "-MAR-V0-",
              "-MAR-YOT-", "-MAR-ZIO-", "-MAR-ZIO -", "-PLA-AGO-", "-PLA-AKE-", "-PLA-AMO-", "-PLA-ANI-", "-PLA-DAN-",
              "-PLA-EST-", "-PLA-HAH-", "-MAR-HAH-", "-PLA-KLO-","-MAR-KLO-", "-PLA-KPE-", "-PLA-KLE-", "-PLA-MOY-", "-CEN-MOY-",
              "-PLA-OGO-", "-PLA-WAW-", "-CEN-BLI-", "-CEN-MO-", "-CEN-MÔ -", "-CEN-MO -", "-CEN-SOT-", "-CEN-TCB-", 
              "-CEN-TCH-", "-KAR-ASS-", "-KAR-BAS-", "KAR-BAS-", "-KAR-BIN-", "-KAR-DAK-", "-KAR-DOU-", "-KAR-KER-",
              "-KAR-KOZ-", "-SAV-CIN-", "-SAV-KPE-", "-SAV-KPO-", "-SAV-OTI-", "-SAV-OTS-","-SAV-OTS ", "-SAV-TAN-", "-SAV-TON-"),
  region = c("Grand Lomé", "Grand Lomé", "Grand Lomé", "Grand Lomé", "Grand Lomé", "Grand Lomé", "Grand Lomé", 
             "Maritime", "Maritime", "Maritime", "Maritime", "Maritime", "Maritime", "Maritime", "Maritime",
             "Maritime", "Maritime", "Maritime", "Plateaux", "Plateaux", "Plateaux", "Plateaux", "Plateaux",
             "Plateaux", "Plateaux", "Plateaux", "Plateaux","Plateaux", "Plateaux", "Plateaux", "Plateaux", "Plateaux",
             "Plateaux", "Plateaux", "Centrale", "Centrale", "Centrale", "Centrale", "Centrale", "Centrale",
             "Centrale", "Kara", "Kara", "Kara", "Kara", "Kara", "Kara", "Kara",
             "Kara", "Savanes", "Savanes", "Savanes", "Savanes", "Savanes","Savanes", "Savanes", "Savanes")
)

# Ajout de la colonne District associée
Rougeole_cases$Région <- NA  # Initialiser avec NA

# Boucle pour assigner les districts basés sur les ID trouvés
for (i in seq_len(nrow(region_patterns))) {
  Rougeole_cases$Région[grepl(region_patterns$Pattern[i], Rougeole_cases$IDNumber)] <- region_patterns$region[i]
}
# Recoder la variable Age
Rougeole_cases$Age_Group <- ifelse(as.numeric(Rougeole_cases$Age) < 5, "Moins de 5 ans", "5 ans et plus")
# Création d'une nouvelle colonne pour Décès
Rougeole_cases$Issue <- ifelse(
  Rougeole_cases$Outcome == 2, 
  "décès","vivant")
rm(Rougeole_all,district_patterns,region_patterns)
# Traitement Données cas par cas Fièvre Jaune ==========
FievreJaune_cases <- FievreJaune_all
FievreJaune_cases$Typecas <- ifelse(FievreJaune_cases$YellowFeverIgM == 1, "confirmé", "suspect")
FievreJaune_cases <- FievreJaune_cases %>%
  mutate_if(is.character, as.factor)
FievreJaune_cases$AnnéeOfonset <- year(FievreJaune_cases$DateOfOnset)
FievreJaune_cases$AnnéeNotif <- year(FievreJaune_cases$DateHeathFacilityNotified)
FievreJaune_cases$WeekOnset <- isoweek(as.Date(FievreJaune_cases$DateOfOnset))
FievreJaune_cases$WeekOnset <- as.factor(FievreJaune_cases$WeekOnset)
FievreJaune_cases$WeekNotif <- isoweek(as.Date(FievreJaune_cases$DateHeathFacilityNotified))
FievreJaune_cases$WeekNotift <- as.factor(FievreJaune_cases$DateHeathFacilityNotified)
# Création d'une nouvelle colonne pour le statut vaccinal
FievreJaune_cases$Statut_Vaccinal <- ifelse(
  FievreJaune_cases$NumberOfVaccinedoses == 0, 
  "Non vaccinée",
  ifelse(
    FievreJaune_cases$NumberOfVaccinedoses > 7, 
    "Inconnu", "Vaccinée"))

# Création de la colonne Type_cas en fonction des conditions
FievreJaune_cases$Type_caslab <- ifelse(
  FievreJaune_cases$YellowFeverIgM == "1", "Confirmé IgM+",
  ifelse( FievreJaune_cases$YellowFeverIgM == "2", "Suspect",
          ifelse( FievreJaune_cases$YellowFeverIgM == "3", "En attente Labo",
                  ifelse( FievreJaune_cases$YellowFeverIgM == "4", "Indéterminé",
                          "Indéterminé") )))

# Liste des motifs et des districts associés
district_patterns <- data.frame(
  Pattern = c("-MAR-GOL-", "RGL-GOL","-RGL-GLO-2", "-LCO-", "-LCO-DN5", "-MAR-AGE-","-GOL-AGE-", "TOG-AGE-AGE-", "-RGL-AGE-","-RGL-AGO-", 
              "-MAR-AVE-", "-MAR-BMO-", "-MAR-LAC-","-MAR_LAC-", "-PLA-LAC-", "-MAR-VO-", "-MAR- VO-", "-MAR-VO -", "-MAR-V0-",
              "-MAR-YOT-", "-MAR-ZIO-","-MAR-RIO-", "-MAR-ZIO -", "-PLA-AGO-","-PlA-CIN-24", "-PLA-AKE-", "-PLA-AMO-", "-PLA-ANI-", "-PLA-DAN-",
              "-PLA-EST-","-MAR-EST-", "-PLA-HAH-", "-MAR-HAH-", "-PLA-KLO-","-MAR-KLO-", "-PLA-KPE-","-MAR-KLE-", "-PLA-KLE-", "-PLA-MOY-", "-CEN-MOY-",
              "-PLA-OGO-","-PLA-OGO -", "-PLA-WAW-", "-CEN-BLI-","-SAV-BLI-", "-KAR-BLI-", "-CEN-MO-", "-CEN-MÔ -","-CEN- MO-", "-CEN-MO -", "-CEN-SOT-","-CEN- MO-18-211", "-CEN-TCB-", 
              "-CEN-TCH-", "-KAR-ASS-", "-KAR-BAS-", "KAR-BAS-", "-KAR-BIN-","-KAR-CEN-23-200", "-KAR-DAK-","-KAR-DAN-", "-KAR-DOU-", "-KAR-KER-",
              "-KAR-KOZ-", "-SAV-CIN-", "-SAV-KPE-", "-SAV-KPO-", "-SAV-OTI-", "-SAV-OTS-","-SAV-OTS ", "-SAV-TAN-", "-SAV-TON-"),
  District = c("Golfe", "Golfe","Golfe", "Golfe", "Agoè-Nyivé", "Agoè-Nyivé", "Agoè-Nyivé", "Agoè-Nyivé","Agoè-Nyivé", "Agoè-Nyivé", 
               "Avé", "Bas-Mono", "Lacs","Lacs", "Lacs", "Vo", "Vo", "Vo", "Vo",
               "Yoto", "Zio","Zio", "Zio", "Agou","Agou", "Akébou", "Amou", "Anié", "Danyi",
               "Est-Mono","Est-Mono", "Haho", "Haho", "Kloto","Kloto", "Kpélé","Kpélé", "Kpélé", "Moyen-Mono", "Moyen-Mono",
               "Ogou","Ogou", "Wawa", "Blitta","Blitta","Blitta", "Mô", "Mô","Mô", "Mô", "Sotouboua","Sotouboua", "Tchamba",
               "Tchaoudjo", "Assoli", "Bassar", "Bassar", "Binah","Binah", "Dankpen","Dankpen", "Doufelgou", "Kéran",
               "Kozah", "Cinkassé", "Kpendjal", "Kpendjal-Ouest", "Oti", "Oti-Sud","Oti-Sud", "Tandjoaré", "Tône")
)

# Ajout de la colonne District associée
FievreJaune_cases$District_resid <- NA  # Initialiser avec NA

# Boucle pour assigner les districts basés sur les motifs trouvés
for (i in seq_len(nrow(district_patterns))) {
  FievreJaune_cases$District_resid[grepl(district_patterns$Pattern[i], FievreJaune_cases$IdNumber)] <- district_patterns$District[i]
}
# Liste des motifs et des région associés
region_patterns <- data.frame(
  Pattern = c("-MAR-GOL-", "RGL-GOL","-RGL-GLO-2", "-LCO-", "-LCO-DN5", "-MAR-AGE-","-GOL-AGE-", "TOG-AGE-AGE-", "-RGL-AGE-","-RGL-AGO-", 
              "-MAR-AVE-", "-MAR-BMO-", "-MAR-LAC-","-MAR_LAC-", "-PLA-LAC-", "-MAR-VO-", "-MAR- VO-", "-MAR-VO -", "-MAR-V0-",
              "-MAR-YOT-", "-MAR-ZIO-","-MAR-RIO-", "-MAR-ZIO -", "-PLA-AGO-","-PlA-CIN-24", "-PLA-AKE-", "-PLA-AMO-", "-PLA-ANI-", "-PLA-DAN-",
              "-PLA-EST-","-MAR-EST-", "-PLA-HAH-", "-MAR-HAH-", "-PLA-KLO-","-MAR-KLO-", "-PLA-KPE-","-MAR-KLE-", "-PLA-KLE-", "-PLA-MOY-", "-CEN-MOY-",
              "-PLA-OGO-","-PLA-OGO -", "-PLA-WAW-", "-CEN-BLI-","-SAV-BLI-", "-KAR-BLI-", "-CEN-MO-", "-CEN-MÔ -","-CEN- MO-", "-CEN-MO -", "-CEN-SOT-","-CEN- MO-18-211", "-CEN-TCB-", 
              "-CEN-TCH-", "-KAR-ASS-", "-KAR-BAS-", "KAR-BAS-", "-KAR-BIN-","-KAR-CEN-23-200", "-KAR-DAK-","-KAR-DAN-", "-KAR-DOU-", "-KAR-KER-",
              "-KAR-KOZ-", "-SAV-CIN-", "-SAV-KPE-", "-SAV-KPO-", "-SAV-OTI-", "-SAV-OTS-","-SAV-OTS ", "-SAV-TAN-", "-SAV-TON-"),
  region = c("Grand Lomé", "Grand Lomé","Grand Lomé", "Grand Lomé", "Grand Lomé","Grand Lomé", "Grand Lomé","Grand Lomé", "Grand Lomé", "Grand Lomé", 
             "Maritime", "Maritime", "Maritime","Maritime", "Maritime", "Maritime", "Maritime", "Maritime", "Maritime",
             "Maritime","Maritime", "Maritime", "Maritime", "Plateaux","Plateaux", "Plateaux", "Plateaux", "Plateaux", "Plateaux",
             "Plateaux","Plateaux", "Plateaux", "Plateaux", "Plateaux","Plateaux", "Plateaux","Plateaux", "Plateaux", "Plateaux", "Plateaux",
             "Plateaux","Plateaux", "Plateaux", "Centrale", "Centrale", "Centrale","Centrale","Centrale", "Centrale", "Centrale","Centrale", "Centrale", "Centrale",
             "Centrale", "Kara", "Kara", "Kara", "Kara", "Kara","Kara", "Kara", "Kara","Kara",
             "Kara", "Savanes", "Savanes", "Savanes", "Savanes", "Savanes","Savanes", "Savanes", "Savanes")
)

# Ajout de la colonne District associée
FievreJaune_cases$Région <- NA  # Initialiser avec NA

# Boucle pour assigner les districts basés sur les motifs trouvés
for (i in seq_len(nrow(region_patterns))) {
  FievreJaune_cases$Région[grepl(region_patterns$Pattern[i], FievreJaune_cases$IdNumber)] <- region_patterns$region[i]
}

# Recoder la variable Age
FievreJaune_cases$Age_Group <- ifelse(as.numeric(FievreJaune_cases$Age) < 5, "Moins de 5 ans",
                                      ifelse(as.numeric(FievreJaune_cases$Age) < 10,"5-10 ans", "11 ans et plus"))
# Création d'une nouvelle colonne pour Décès
FievreJaune_cases$Issue <- ifelse(
  FievreJaune_cases$Outcome == 2, 
  "décès","vivant")
rm(FievreJaune_all,district_patterns,region_patterns)
# Traitement Données de notification MPE =======
Data_mpe <- Data_mpe %>%
  mutate(Data = case_when(
    str_detect(Data, "Cholera|cholera") ~ "Choléra",
    str_detect(Data, "Paralysie flasque aiguë") ~ "Paralysie flasque aiguë (PFA)",
    str_detect(Data, "Meningite|meningite") ~ "Méningite",
    str_detect(Data, "Fièvre jaune") ~ "Fièvre jaune",
    str_detect(Data, "rage") ~ "Rage",
    str_detect(Data, "Charbon humain") ~ "Charbon humain",
    str_detect(Data, "Syndrome de fièv") ~ "Syndrome de fièvre virale hémorragique",
    str_detect(Data, "syndrome grippal") ~ "Syndrome grippal",
    str_detect(Data, "rougeole") ~ "Rougeole",
    str_detect(Data, "COVID|Covid-19") ~ "Covid-19",
    str_detect(Data, "iarrhee rouge|diarrhee rouge") ~ "Diarrhée rouge",
    str_detect(Data, "deces de pneumonie enfant de moins de 5 ans|Pneumonie enfant de moins de 5 ans|pneumonie enfant de moins de 5 ans|Pneumonie enfant|pneumonie enfant") ~ "Pneumonie < 5 ans",
    str_detect(Data, "I.R.A.S.") ~ "Infection Respiratoire Aigüe",
    str_detect(Data, "tanos n") ~ "Tétanos néonatal",
    TRUE ~ Data  # Conserver les autres valeurs inchangées
  ))
#extraire les semaine complet par année
Semaine_all <- Data_mpe %>%
  distinct(Région,District, Semaine, year, Date)
#creer les variable suspect, confirmé et décès
Data_mpe_pivot <- Data_mpe %>%
  pivot_wider(
    names_from = Type,
    values_from = Value, 
    values_fill = list(Value = 0)  # Remplacer les valeurs manquantes par 0
  )
Data_mpe_pivot <- Data_mpe_pivot %>% rename(PC=`NA`)
Data_mpe <- Data_mpe_pivot %>% 
  filter(!(Data %in% c("Promptitude", "Complétude"))) %>% dplyr::select(-PC,-Numerator,-Denominator)

Data_Indicateur <- Data_mpe_pivot %>%
  filter(Data %in% c("Promptitude", "Complétude")) %>% 
  dplyr::select(-Numerator, -Denominator, -suspects, -confirmés, -décès)

# Fusionner avec les données d'origine et remplacer les valeurs manquantes par 0
Data_mpe <- merge(Semaine_all, Data_mpe, all.x = TRUE)
Data_mpe <- Data_mpe %>% rename(Maladie=Data) # renommer la colonne Data en Maladie
Data_mpe <- Data_mpe %>% filter(!is.na(Maladie)) # garder seulement la base des maladies sans ind prompt et compl
Data_mpe[is.na(Data_mpe)] <- 0 # Donner 0 à tous les valeurs vide
rm(Data_mpe_pivot,Semaine_all) #supprimer les variables dont j'ai pas besoin
gc()#liberer de la mémoire