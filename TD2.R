# TD R - Analyse de données Mario Kart
# Objectif : Manipulation de dataframes, statistiques et corrélations

# Exercice 1 - Importer des données

# 1. Afficher le répertoire courant
getwd()

# 2. Changer le répertoire vers le dossier contenant les datasets
# Note : Pensez à adapter le chemin selon votre ordinateur
setwd(dir = "C:/Nouveau dossier/S2/R/TD2/dataset")

# 3. Importer les datasets
# Attention : Chaque fichier a ses propres séparateurs (sep) et décimales (dec)
bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires        = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders      = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers      = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")

# 4. Afficher les dimensions (Lignes x Colonnes)
dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)


# Exercice 2 - Statistique (Corrélation & Graphiques)

# 1. Résumé statistique des données
summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)

# 2. Nuage de points : Weight vs Acceleration (Drivers)
plot(x = drivers$Weight, 
     y = drivers$Acceleration, 
     main = "Drivers : Weight / Acceleration",
     xlab = "Poids", ylab = "Accélération")
# COMMENTAIRE : Corrélation négative visible (quand le poids augmente, l'accel diminue).
# Moins de points que de drivers car certains ont les mêmes stats (points superposés).

# 3. Coefficient de corrélation
cor(x = drivers$Weight, y = drivers$Acceleration)

# 4. Calcul manuel de la corrélation (Formule : Cov(X,Y) / (sd(X)*sd(Y)))
covXY = cov(x = drivers$Weight, y = drivers$Acceleration)
sX = sd(drivers$Weight)
sY = sd(drivers$Acceleration)
print(covXY / (sX * sY))

# 5. Coefficient de détermination (R²)
coefCorr = cor(x = drivers$Weight, y = drivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter) # Indique le % de variance expliquée

# 6. Matrice des corrélations (Variables quantitatives uniquement)
# On exclut la 1ère colonne (Nom du driver)
matriceCor = cor(drivers[ , -1])
matriceCor = round(matriceCor, 2)
View(matriceCor)

# 7. Installation et chargement de corrplot
# install.packages("corrplot") # À ne faire qu'une fois
library(corrplot)

# 8. Corrélogramme simple
corrplot(matriceCor, method = "circle")

# 9. Corrélogrammes pour les autres datasets
# Tires
matriceTires = round(cor(tires[ , -1]), 1)
corrplot(matriceTires, method = "color", type = "upper", addCoef.col = "black", diag = FALSE)

# Bodies
matriceBodies = round(cor(bodies_karts[ , -1]), 1)
corrplot(matriceBodies, method = "color", type = "upper", addCoef.col = "black", diag = FALSE)

# Gliders (Gestion de l'erreur d'écart-type nul)
# NOTE : La colonne "Ground.Handling" est constante (sd=0), on doit l'exclure pour cor()
# On repère l'index ou on exclut par nom
gliders_quanti = gliders[, -1] 
# Si erreur, c'est que Ground.Handling (colonne 5 par ex) est constante :
# gliders_quanti = gliders_quanti[, sapply(gliders_quanti, sd) > 0]
matriceGliders = round(cor(gliders_quanti), 1)
corrplot(matriceGliders, method = "color", type = "upper", addCoef.col = "black", diag = FALSE)


#Exercice 3 - Manipulation de data frame (Selection & Tri)

# 1. Sélection simple
resultat = drivers[ , c("Driver" , "Weight")]

# 2. 10 premières lignes
resultat = drivers[ 1:10 , c("Driver" , "Acceleration")]

# 3. Exclusion de colonnes par index
resultat = drivers[ , -c(5,7,9)]

# 4. Exclusion par nom (Astuce : impossible avec '-', on utilise l'index ou subset)
resultat = drivers[ , -c(2,3)] # Ici Weight et Acceleration sont col 2 et 3

# 5. Changement d'ordre des colonnes
resultat = drivers[ , c("Driver", "Acceleration", "Weight")]
# NOTE : R respecte l'ordre du vecteur c() fourni.

# 6 & 7. Ordre spécifique des lignes
resultat = drivers[ c(32, 3, 12) , ]

# 8. Tri par poids croissant (order)
rang = order(drivers$Weight)
resultat = drivers[ rang , c("Driver", "Weight") ]

# 9. Tri par accélération décroissante
rang = order(drivers$Acceleration, decreasing = TRUE)
resultat = drivers[ rang , c("Driver", "Acceleration") ]

# 10. Tri multicritères (Accel décroissante, puis Weight croissant)
# Rappel : decreasing peut prendre un vecteur c(TRUE, FALSE)
rang = order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE, FALSE))
resultat = drivers[ rang , c("Driver", "Acceleration", "Weight") ]


# Exercice 4 - GOAT

# 1. Top Driver (Meilleure Accélération)
topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver", "Acceleration"))

# 2. Top Glider, Tires, Body
topGlider = subset(gliders, Acceleration == max(Acceleration), select = c("Glider", "Acceleration"))
topTires  = subset(tires, Acceleration == max(Acceleration), select = c("Tire", "Acceleration"))
topBody   = subset(bodies_karts, Acceleration == max(Acceleration), select = c("Body", "Acceleration"))

# Affichage des résultats
print(topDriver)
print(topGlider)
print(topTires)
print(topBody)