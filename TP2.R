# Objectifs : Importer, Sélection et filtre sur et Trier un dataframe

# Exercice 1 - Importer les données

# 1. Importation du jeu de données
# Note : Adaptez le chemin du fichier si nécessaire
df = read.csv("C:/Users/nirma/Downloads/fao.csv", sep = ";", dec = ",", header = TRUE)

# Vérification du type des colonnes
str(df)

# 2. Nombre de pays présents
nb_pays = nrow(df)
print(paste("Nombre de pays :", nb_pays))

# 3. Résumé statistique (permet de repérer les valeurs manquantes NA)
summary(df)


# Exercice 2 - Statistiques descriptives

# 1. Disponibilité alimentaire moyenne mondiale (Kcal/pers/jour)
mean(df$Dispo_alim, na.rm = TRUE)

# 2. Nombre total d'habitants dans le monde (en milliers selon le dataset)
sum(df$Population, na.rm = TRUE)

# 3. Écart-type des exportations et importations de viande
sd(df$Export_viande, na.rm = TRUE)
sd(df$Import_viande, na.rm = TRUE)

# 4. Médiane de la production de viande
median(df$Prod_viande, na.rm = TRUE)

# 5. Quartiles de la disponibilité alimentaire
quantile(df$Dispo_alim, na.rm = TRUE)
quantile(df$Dispo_alim, c(0, 0.25,0.5,0.75,1), na.rm = TRUE)
quantile(df$Dispo_alim, seq(from=0, to=1, by=0.25 ))

# 6. Centiles du volume d'importation de viande
quantile(df$Import_viande, seq(from=0, to=1, by=0.01))
quantile(df$Import_viande, seq(0, 1, 0.01), na.rm = TRUE)

# Exercice 3 - Tris et filtres

# 1. Les 5 pays les moins peuplés
rang_pauvre = order(df$Population)
res_moins_peuples = head(df[rang_pauvre, ], n = 5)
View(res_moins_peuples)

# 2. Les 5 pays les plus peuplés
rang_riche = order(df$Population, decreasing = TRUE)
res_plus_peuples = head(df[rang_riche, ], n = 5)

# 3. Les 5 plus gros producteurs de viande
rang_prod = order(df$Prod_viande, decreasing = TRUE)
res_gros_prod = head(df[rang_prod, ], n = 5)

# 4. Les 5 plus gros importateurs de viande
rang_imp = order(df$Import_viande, decreasing = TRUE)
res_gros_imp = head(df[rang_imp, ], n = 5)

# 5. Pays avec Dispo_alim >= 2300 kcal
res_faim_ok = subset(df, Dispo_alim >= 2300)
nrow(res_faim_ok) # Nombre de pays concernés

# 6. Pays avec Dispo > 3500 kcal ET Import >= 1000 tonnes
res_critere_strict = subset(df, Dispo_alim > 3500 & Import_viande >= 1000)

# 7. Extraction spécifique : France et Belgique
res_fr_bel = subset(df, Nom %in% c("France", "Belgique"))


# Exercice 4 - Modifier le dataframe

# 1. Ajout de la part des exportations / production
df$Part_export = df$Export_viande / df$Prod_viande

# 2. Ajout de la disponibilité alimentaire totale du pays
df$Dispo_alim_pays = df$Dispo_alim * df$Population

# 3. Exportation du dataframe modifié
write.table(x = df, file = "ExportTp2.csv")

# 4. Somme de la disponibilité alimentaire mondiale
dispo_alim_mondiale = sum(df$Dispo_alim_pays, na.rm = TRUE)

# 5. Nombre d'adultes que l'on pourrait nourrir (base 2300 kcal/jour)
nb_adultes_nourris = dispo_alim_mondiale / 2300
print(nb_adultes_nourris)

# Exercice 5 - Corrélation

# 1. Nuage de points : Production vs Exportation
plot(x = df$Prod_viande, 
     y = df$Export_viande, 
     main = "Relation Production / Exportation")

# 2. Coefficient de corrélation
cor(x = df$Prod_viande, y = df$Export_viande)
cor(df$Prod_viande, df$Export_viande, use = "complete.obs")

# 3. Matrice de corrélation (on exclut la colonne 1 qui est le Nom)
# use = "pairwise.complete.obs" permet de gérer les NA par paire de colonnes
matriceCor = cor(df[, -1], use = "pairwise.complete.obs")
matriceCor = round(matriceCor, 2)
View(matriceCor)

# 4. Installation de corrplot (si nécessaire) et chargement
library(corrplot)

# 5. Corrélogramme
corrplot(matriceCor, 
         method = "circle")