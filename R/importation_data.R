# Importation Données liste linéaire choléra ============
path_cholera <- path.expand("data/Cholera_Togo.xlsx")
url      <- paste0("https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/data/Choléra_Togo.xlsx")
download.file(url, destfile = path_cholera, mode = "wb")
Data_cholera <- read_excel(path_cholera, sheet = "Liste Linéaire_Togo",
                           col_types = c("numeric","text","numeric","text","numeric","text","text",
                                         "text","text","numeric", "numeric","text","text","text","text",
                                         "text","date", "text","date","text","text","text","text","text",
                                         "text","text","text","text","text","text","text","text","text",
                                         "text","text","date","text","text","text","text","text","text"))

# Importation Données cas par cas polio  ============
destfile <- path.expand("data/ATOG.mdb")
url      <- paste0("https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/data/ATOG.mdb")
download.file(url, destfile = destfile, mode = "wb")
Path_Polio <- odbcConnectAccess2007(destfile)
tables_info <- sqlTables(Path_Polio)
valid_tables <- subset(tables_info, !grepl("^MSys", TABLE_NAME))$TABLE_NAME[1]  # Exclut les tables système
Polio_all <- RODBC::sqlFetch(Path_Polio,valid_tables)
# Importation Données cas par cas Rougeole=========
destfile <- path.expand("data/MTOG.mdb")
url      <- paste0("https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/data/MTOG.mdb")
download.file(url, destfile = destfile, mode = "wb")
Path_Rougeole <- odbcConnectAccess2007(destfile)
tables_info <- sqlTables(Path_Rougeole)
valid_tables <- subset(tables_info, !grepl("^MSys", TABLE_NAME))$TABLE_NAME[1]  # Exclut les tables système
Rougeole_all <- RODBC::sqlFetch(Path_Rougeole,valid_tables)
# Importation Données cas par cas Fièvre jaune =====
destfile <- path.expand("data/YTOG.mdb")
url      <- paste0("https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/data/YTOG.mdb")
download.file(url, destfile = destfile, mode = "wb")
Path_FievreJaune <- odbcConnectAccess2007(destfile)
tables_info <- sqlTables(Path_FievreJaune)
valid_tables <- subset(tables_info, !grepl("^MSys", TABLE_NAME))$TABLE_NAME[1]  # Exclut les tables système
FievreJaune_all <- RODBC::sqlFetch(Path_FievreJaune,valid_tables)
# Importation Données de notification MPE ============
path_mpe <- path.expand("data/BASE_DATA.xlsx")
url      <- paste0("https://raw.githubusercontent.com/richareskodata/Togo-Heath-Data-Dashbord/master/data/BASE_DATA.xlsx")
download.file(url, destfile = path_mpe, mode = "wb")
Data_mpe <- read_excel(path_mpe, sheet = "BMPE",
                       col_types = c("text","text","text","text","numeric","numeric","numeric","numeric","numeric","date"))
#supprimer les variable temporaires====
rm(tables_info,destfile,path_cholera,Path_Polio, path_mpe, Path_FievreJaune,Path_Rougeole, url, valid_tables)
