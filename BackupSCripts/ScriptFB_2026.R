#FBolduc, hiver 20256 - enjeux base de donnees SOMEC


# WhereAreThePackages <- "C:/Users/BolducFr/Documents/packages" 
# .libPaths(WhereAreThePackages)#packages installés dans ma library perso

#libraries
library(RODBC)

getwd()

    #1. import des donnees existantes qui sont dans ACCESS

    #Tables a extraire:
    tablesMdb <- c("missions","observations","transects", 'Code espèces')
    #nouveaux noms des tables dans R (au besoin):
    table_noms <- c("missions","observations","transects", 'species')

    #ou sont les donnees:
     serveur <- 'U:'#oiseaux marins
     dossierDonnees <- 'SOMEC/BaseDeDonnees'
    mdb <- 'SOMEC_20251106.accdb'
    donnees <- paste(serveur,dossierDonnees,mdb,sep='/')


    con <- odbcConnectAccess2007(donnees)
    for(i in 1:length(tablesMdb)){
      name <- paste(table_noms[[i]])
      assign(name, sqlFetch(con,tablesMdb[i]))
    }
    close(con)

     dossier <- paste(serveur,dossierDonnees,'GestionDeDonnees',sep='/')
     save.image(file=paste(dossier,'ImportSOMEC202511.RData', sep="/"))

#env R a jour déja sauvées dans un RData:
load(paste(dossier,"ImportSOMEC202511.RData",sep="/"))


  #1. missions a deux observateurs au Quebec?
  #si on ouvre la table missions, on voit que la plupart sont des missions sur avion, donc toujours 2 obs;
  #reste deux sur bateau, TEL190906 ET TEL170529

t <- observations[observations$mission==c('TEL190906'),]#pas trop d'indice ici...
t <- observations[observations$mission==c('TEL170529'),]#pas de commentaire

#rien dans les commentaires qui documente un changement d'observateur
t$nchar <- nchar(t$commentaire)
t2 <- t[!is.na(nchar(t$commentaire)),]

###en vérifiant dans les donnees de base, l'observateur GC dans ces deux cas sont des erreurs d'entree de donnees, 
#seulement quelques lignes, probablement du au menu deroulant de PCMapper. 
#corrigé a la main dans la table de donnees Access - enlever le deuxieme observateur.
#ajout CD = Christine Drouin dans Access



####oiseaux en vol et non snapshot
str(observations)
table(observations$snapshot)
t <- observations[which((observations$snapshot=='Non')| (observations$snapshot=='Non') | (observations$snapshot=='NON')),]
#t <- t[!is.na(t$mission),]
table(observations$activite)
t <- t[which(t$activite=='Vol'| t$activite=='VOL'),]
# table(t$tra_rad)
# t <- t[which(t$tra_rad!= 'Exterieur'),]
table(t$tra_par)
t <- t[which(t$tra_par!= 'Exterieur'),]
# table(t$dis_rad)
#t <- t[which(t$dis_rad!= 'T'),]
table(t$dis_par)
#t <- t[t$dis_par!= 'T',]#aucun

#check quelles missions:
table(t$mission)
# COR160601 GOR140228 HUD081026 HUD091025 HUD101107 HUD111102 HUD121106 HUD140601 HUD141029 HUD151019 MLB090423 TEL100603 TEL110817 TEL120802 TEL120905 
# 1        10         5         9         9         4        22         2         1         3         2        15         2       246        14 
# TEL120919 TEL130804 TEL150802 TEL150904 THA140807 
# 213       490         1         2         1 
#check les plus grosses pour voir si c'est des erreurs d'entree ou de compréhension:
t2 <- observations[observations$mission%in%c('TEL130804'),]#LDC
t2 <- observations[observations$mission%in%c('TEL120919'),]#LDC
t2 <- observations[observations$mission%in%c('TEL120802'),]#LDC

##90% DES cas dans trois missions par la meme personne (LDC). pas de patron évident dans les données, 
#je concluerais que l'observateur a simplement noté des oiseaux en vol en tout temps, donc autant en dehors des snapshots.



####VALIDATION DES CODES PRESENTS DANS LES VARIABLES DES TABLES:
#mission: 
#str(missions)
#lapply(missions, unique)
missions$nom_plateforme[missions$nom_plateforme == "HUDSON"] <- "Hudson"
#table(missions$nom_plateforme)

#table(missions$Saisie)
missions$Saisie[missions$Saisie == "Ordi"] <- "ordi"

#transects
str(transects)
lapply(transects, unique)

#table(transects$site)
transects$site[transects$site == "Debut"] <- "Début"
transects$site[transects$site == "description initiale"] <- "Description initiale"
transects$site[transects$site == "fin"] <- "Fin"
transects$site[transects$site == "mioyen"] <- "Mitoyen"
transects$site[transects$site == "mitoyen"] <- "Mitoyen"

#table(transects$act_plateforme)
transects$act_plateforme[transects$act_plateforme == "EnDéplaceme"] <- "En Déplacement"
transects$act_plateforme[transects$act_plateforme == "EnDéplacement"] <- "En Déplacement"

#table(transects$visibilite)
transects$visibilite <- gsub("KM", "", transects$visibilite)
transects$visibilite <- gsub("km", "", transects$visibilite)
transects$visibilite <- gsub("M", "", transects$visibilite)
transects$visibilite <- gsub("m", "", transects$visibilite)

to_remove <- c("ill", "ILL", "illiitée",'ILLIITÉE','illitée','Horizon','H')
pattern <- paste(to_remove, collapse = "|")
transects$visibilite <- gsub(pattern, "20", transects$visibilite)#code par defaut dans le protocole
transects$visibilite <- trimws(transects$visibilite)  # clean leftover spaces

#assume tout ce qui est plus grand que 99 est en metres, donc change en km
vals <- c('100', '10000', '150', '200', '250', '300', '400', '500', '5000', '600', '700', '800')
vals <- unique(vals)
idx <- transects$visibilite %in% vals
transects$visibilite[idx] <- as.character(as.numeric(transects$visibilite[idx]) / 1000)
transects$visibilite <- gsub("25", "20", transects$visibilite)


#table(transects$cote_obs)
transects$cote_obs[transects$cote_obs == "Gauche"] <- "Babord"
transects$cote_obs[transects$cote_obs == "Droit"] <- "Tribord"

#table(transects$int_exterieur)
transects$int_exterieur[transects$int_exterieur == "Exterieur"] <- "Extérieur"
transects$int_exterieur[transects$int_exterieur == "Interieur"] <- "Intérieur"


##table observations
lapply(observations, unique)

table(observations$code_espece)
observations$code_espece <- toupper(observations$code_espece)
#compare observed species to mdb species list:
observations$code_espece[!(observations$code_espece %in% species$CodeFR)] 
#check commentaires:
observations[observations$code_espece=='LASA',c('code_espece','commentaire')]
#add new species to species code table:

#ajout des nouveaux codes a la table Code_espece
df <- rbind(
df<- data.frame(CodeFR='PATC', NomFR="Paruline a tête cendrée",  CodeAN='MAWA', NomAN="Magnolia Warbler", CodeLAT='SEMA', NomLAT='Setophaga magnolia',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='SAAB', NomFR="Sarcelle à ailes bleues",  CodeAN='BWTE', NomAN="Blue-winged Teal", CodeLAT='SPDI', NomLAT='Spatula discors',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='ROPO', NomFR="Roselin pourpré",  CodeAN='PUFI', NomAN="Purple Finch", CodeLAT='HAPU', NomLAT='Haemorhous purpureus',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='ROCR', NomFR="Roitelet à couronne rubis",  CodeAN='RCKI', NomAN="Ruby‑crowned Kinglet", CodeLAT='COCA', NomLAT='Corthylio calendula',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='PAPB', NomFR="Paruline à poitrine baie",  CodeAN='BBWA', NomAN="Bay‑breasted Warbler", CodeLAT='SECA', NomLAT='Setophaga magnolia',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='PAOB', NomFR="Paruline obscure",  CodeAN='TEWA', NomAN="Tennessee Warbler", CodeLAT='LEPE', NomLAT='Leiothlypis peregrina',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='PANB', NomFR="Paruline noir et blanc",  CodeAN='BAWW', NomAN="Black‑and‑white Warbler", CodeLAT='MNVA', NomLAT='Mniotilta varia',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='PAJA', NomFR="Paruline jaune",  CodeAN='YEWA', NomAN="Yellow Warbler", CodeLAT='SEPE', NomLAT='Setophaga petechia',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='PAGN', NomFR="Paruline à george noire",  CodeAN='BTNW', NomAN="Black‑throated Green Warbler", CodeLAT='SEVI', NomLAT='Setophaga virens',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='PACR', NomFR="Paruline à couronne rousse",  CodeAN='PAWA', NomAN="Palm Warbler", CodeLAT='SEPA', NomLAT='Setophaga palmarum',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='MOPH', NomFR="Moucherolle phébi",  CodeAN='EAPH', NomAN="Eastern Phoebe", CodeLAT='SAPH', NomLAT='Sayornis phoebe',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='MEAM', NomFR="Merle d'Amérique",  CodeAN='AMRO', NomAN="American Robin", CodeLAT='TUMI', NomLAT='Turdus migratorius',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='HIAH', NomFR="Hirondelle à ailes hérissées",  CodeAN='NRWS', NomAN="Northern Rough‑winged Swallow", CodeLAT='STSE', NomLAT='Stelgidopteryx serripennis',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='GRSO', NomFR="Grive solitaire",  CodeAN='HETH', NomAN="Hermit Thrush", CodeLAT='CAGU', NomLAT='Catharus guttatus',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='GRDO', NomFR="Grive à dos olive",  CodeAN='SWTH', NomAN="Swainson’s Thrush", CodeLAT='CAUS', NomLAT='Catharus ustulatus',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='GRCH', NomFR="Grand chevalier",  CodeAN='GRYE', NomAN="Greater Yellowlegs", CodeLAT='TRME', NomLAT='Tringa melanoleuca',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='GBER', NomFR="Gros-bec errant",  CodeAN='EVGR', NomAN="Evening Grosbeak", CodeLAT='COVE', NomLAT='Coccothraustes vespertinus',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='ENBP', NomFR="Engoulevent bois‑pourri",  CodeAN='WHIP', NomAN="Eastern Whip‑poor‑will", CodeLAT='ANVO', NomLAT='Antrostomus vociferus',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='CYSI', NomFR="Cygne siffleur",  CodeAN='TUSW', NomAN="Tundra Swan", CodeLAT='CYCO', NomLAT='Cygnus columbianus columbianus',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='BRLI', NomFR="Bruant de Lincoln",  CodeAN='LISP', NomAN="Lincoln’s Sparrow", CodeLAT='MELI', NomLAT='Melospiza lincolnii',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='GRBR', NomFR="Grinpereau brun",  CodeAN='BRCR', NomAN="Brown Creeper", CodeLAT='CEAM', NomLAT='Certhia americana',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='VITB', NomFR="Viréo à tête bleue",  CodeAN='BHVI', NomAN="Blue‑headed Vireo", CodeLAT='VISO', NomLAT='Vireo solitarius',  ECSAS_ESP_ID=''),
df<- data.frame(CodeFR='BRSP', NomFR="Bruant sp.",  CodeAN='SPSP', NomAN="Sparrow sp.", CodeLAT='EMSP', NomLAT='Emberizid sp.',  ECSAS_ESP_ID='')
)

df$ECSAS_ESP_ID<- as.integer(df$ECSAS_ESP_ID)
names(df)[names(df)=='CodeLAT']<-'CodeLat'
names(species)[names(species)=='ECSAS ESP ID']<-'ECSAS_ESP_ID'
species <- rbind(species,df)

#erreurs de code dans table observations
observations$code_espece[observations$code_espece == "MOTR1"] <- "MOTR"
observations$code_espece[observations$code_espece == "FOEM"] <- "FAEM"
observations$code_espece[observations$code_espece == "PLON"] <- "PLSP"
observations$code_espece[observations$code_espece == "REIN"] <- "RIEN"
observations$code_espece[observations$code_espece == "PITB"] <- "PYTB"
observations$code_espece[observations$code_espece == "PHOQ"] <- "PQSP"
observations$code_espece[observations$code_espece == "DAPH"] <- "DASP"

table(observations$activite)
observations$activite[observations$activite == "EAU"] <- "Eau"
observations$activite[observations$activite == "Sur l'eau"] <- "Eau"
observations$activite[observations$activite == "VOL"] <- "Vol"

table(observations$dis_par)
observations$dis_par[observations$dis_par == "c"] <- "C"
observations$dis_par[observations$dis_par == "d"] <- "D"
df<- observations[which(observations$dis_par=='Z' & observations$code_espece!='RIEN'),]
unique(df$mission)
df<- observations[observations$mission=='TEL170803',]
df<-df[order(df$date, df$heure), ]#semble etre des erreurs de saisie, n=337 sur 2 missions, meme observatrice (MP).
#code de distance changé pour 3 (in transect, no distance) pour au moins que ca compte dans les transects
observations$dis_par[which(observations$dis_par=='Z' & observations$code_espece!='RIEN')]<-'3'


table(observations$dis_rad)
observations$dis_rad[observations$dis_rad == "+ de 300 m"] <- "E"
observations$dis_rad[observations$dis_rad == "A1"] <- "A"
observations$dis_rad[observations$dis_rad == "A2"] <- "A"
observations$dis_rad[observations$dis_rad == "c"] <- "C"

df <- observations[observations$dis_rad%in% c('F','K','L','M','N','O'),]
df <- observations[observations$dis_rad%in% c('F'),]
unique(df$mission)

#mix up between ship codes and plane codes?
observations <- merge(observations,missions[, c("mission", "type_plateforme")],by = "mission",all.x = TRUE)
df <- observations[observations$type_plateforme=='Avion',]#dans certaines missions, code de distance entré dans dis_rad par erreur
table(df$dis_par)
table(df$dis_rad)
test<- observations[(observations$type_plateforme=='Avion') & (!is.na(observations$dis_par)) & (!is.na(observations$dis_rad)),]#eereur de codage?
observations$dis_par[(observations$type_plateforme=='Avion') & (!is.na(observations$dis_par)) & (!is.na(observations$dis_rad))]<-''

test<- observations[(observations$type_plateforme=='Avion') & (is.na(observations$dis_par)) & (!is.na(observations$dis_rad)),]
#transfert des donnees dans rad a par:
observations$dis_par <- ifelse(
  observations$type_plateforme == "Avion" &
    is.na(observations$dis_par) &
    !is.na(observations$dis_rad),
  observations$dis_rad,
  observations$dis_par
)

df <- observations[observations$type_plateforme=='Bateau',]
table(df$dis_par)#ok
table(df$dis_rad)
df2<-df[which(df$dis_rad=='moins de 300m'),]
df3<- observations[which(df$mission=='AMU250628'),]
