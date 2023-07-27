########################################################################
####-------------------------- CAH avec R --------------------------####
########################################################################



##----------------------- Import des données--------------------------##


# Tableau csv
data <- read.csv2(file = "data/DEV_AFRICA_2018/afrika_don.csv")

# ESRI shapefile (données géographiques)
library(sf)
basemap <- st_read("data/GADM_AFRICA_2020/afrika_map.shp",  quiet = TRUE)




##------------- Exploration des corrélations entre les variables -------------##

# Nuages de point entre toutes les variables quantitatives du tableau
pairs(data[,c(4:20)])

# Coefficients de corrélation entre toutes les variables quantitatives du tableau
cor(data[,c(4:20)], use = "na.or.complete")

# Nuage de point
plot(data[,"AGEMED"], data[,"DJEUNE"])
plot(data[,"AGEMED"], data[,"DVIEUX"])
plot(data[,"DJEUNE"], data[,"DVIEUX"])
plot(data[,"EMPAGR"], data[,"EMPSER"])
plot(data[,"ESPVIE"], data[,"MORINF"])
plot(data[,"PIB"], data[,"IDH"])
plot(data[,"IDH"], data[,"AGEMED"])
plot(data[,"IDH"], data[,"CO2HAB"])
plot(data[,"IDH"], data[,"DVIEUX"])
plot(data[,"IDH"], data[,"DJEUNE"])
plot(data[,"IDH"], data[,"INTERN"])

# Coefficient de corrélation
cor.test(data[,"AGEMED"], data[,"DJEUNE"])



##------------------------- Préparation du tableau ---------------------------##

# 1. Ajout de noms de ligne 
row.names(data) <- data$iso3

# 2. Sélection des colonnes
data <- data[ , c("POP", "PIB", "ADOFEC", "EMPAGR", "INTERN", 
                  "ESPVIE", "TXMIGR", "DVIEUX", "TUBERC", "URBANI")]



##--------------------------- Matrice de distance ----------------------------##

# Calcul de la matrice de distances (euclidienne)
md <- dist(data, method = "euclidean")



##------------------------- Regroupement héirarchique ------------------------##

# Aggrégation des individus par la méthode de Ward appliquée au carré des distances
hc <- hclust(md, method = "ward.D2")

# Affichage du dendogramme
plot(hc, main = "Distance euclidienne + Méthode de Ward2")



##----------------------- Représentation de l'inertie ------------------------##

# Récupération et tri croissant de l'inertie de chaque partition
inertie <- sort(hc$height, decreasing = TRUE)

# Représentation graphique de l'inertie en fonction du nombre de classe
plot(inertie[1:15], type = "s", xlab = "Nombre de classes", ylab = "Inertie")




##--------------------------- Découpage de l'arbre ---------------------------##

# Découpage de l'arbre en 3 classes - Résultat assigné dans une nouvelle colonne
data$Classe <- cutree(hc, k = 3)



##-------------------------- Cartographie de la CAH --------------------------##

# Jointure Fond de carte - tableau
map_data <-merge(basemap, data, by.x="iso3", by.y=0, all.x=TRUE)

# Carte thématique
library(mapsf)
mf_map(x = map_data, 
       var = "Classe", 
       type = "typo",
       pal = c(  "yellow3", "red3", "blue4"),
       leg_title = "Classes")

mf_layout(
  title = "CAH (distance euclidienne + méthode Ward.2)",
  credits = "Sources: Human Development Report 2020 & GADM 2023"
)

