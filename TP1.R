# TP : Manipulation de Dataframes en R (Iris, Manga et Anime)

# EXERCICE 1 - Utilisation d'un dataframe existant (Iris)

# 1. Afficher le jeu de données iris puis afficher la classe de l'objet iris.
iris
class(iris)

# 2. Afficher le jeu de données iris dans une vue avec la fonction View()
View(iris)

# 3. Afficher le nombre de lignes avec la fonction nrow()
nrow(iris)

# 4. Afficher le nombre de colonnes avec la fonction ncol()
ncol(iris)

# 5. Afficher le nom des colonnes avec la fonction colnames()
colnames(iris)

# 6. Afficher un résumé du dataframe avec la fonction summary()
summary(iris)

# 7. Afficher uniquement les colonnes Sepal.Length et Species
iris[ , c("Sepal.Length", "Species")]

# 8. Afficher uniquement la ligne 100, 103 et 105
iris[c(100, 103, 105), ]

# 9. Afficher uniquement les lignes 50 à 100
iris[50:100, ]

# 10. Calculer la moyenne de la variable Sepal.Length
mean(iris$Sepal.Length)

# 11. Calculer la médiane de la variable Sepal.Width
median(iris$Sepal.Width)

# 12. Calculer l'écart-type de la variable Petal.Length
sd(iris$Petal.Length)

# 13. Calculer les déciles de la variable Petal.Width
quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by = 0.1))


# EXERCICE 2 - Import/Export et Statistiques (Manga & Anime)

# 1. Importer les jeux de données et vérifier la classe
dfManga = read.csv("C:/Users/nirma/Downloads/manga.csv", header = TRUE, sep = ",", dec = ".")
dfAnime = read.csv("C:/Users/nirma/Downloads/anime.csv", header = TRUE, sep = ",", dec = ".")

class(dfManga)
class(dfAnime)

# 2. Afficher les jeux de données dans des vues
View(dfManga)
View(dfAnime)

# 3. Afficher le nombre de lignes et colonnes avec la fonction dim()
dim(dfManga)
dim(dfAnime)

# 4. Calculer la moyenne du Score. Quelle est la plus élevée ?
mean(dfManga$Score, na.rm = TRUE)
mean(dfAnime$Score, na.rm = TRUE)

# 5. Calculer le nombre total de votes. Où y a-t-il le plus de votes ?
sum(dfManga$Vote, na.rm = TRUE)
sum(dfAnime$Vote, na.rm = TRUE)

# 6. Calculer l'écart-type (homogénéité) du Score
sd(dfManga$Score, na.rm = TRUE)
sd(dfAnime$Score, na.rm = TRUE)

# 7. Calculer les déciles du Score. Quel df a le décile 1 le plus petit ?
quantile(dfManga$Score, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
quantile(dfAnime$Score, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)


# EXERCICE 3 - Filtrage avec subset()

# --- FILTRE MANGAS ---

# 1. Manga avec note > 9/10
manga_sup9 = subset(dfManga, Score > 9)
nrow(manga_sup9)

# 2. Manga avec 200 000 votes ou plus
manga_vote200k = subset(dfManga, Vote >= 200000)
nrow(manga_vote200k)

# 3. Manga avec > 200 000 votes ET note > 8/10
manga_top = subset(dfManga, Vote > 200000 & Score > 8)
nrow(manga_top)

# 4. Manga avec note entre 7/10 et 8/10
manga_7_8 = subset(dfManga, Score >= 7 & Score <= 8)
nrow(manga_7_8)


# --- FILTRE ANIMES ---

# 1. Effectifs et modalités de la variable Rating
effectifRating = table(dfAnime$Rating)
print(effectifRating)
length(effectifRating) # Nombre de modalités
prop.table(effectifRating) * 100 # Pourcentage

# 2. Anime Rating : R - 17+ (violence & profanity)
anime_R17 = subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
nrow(anime_R17)

# 3. Anime R17 et note > 8/10
anime_R17_top = subset(dfAnime, Rating == "R - 17+ (violence & profanity)" & Score > 8)
nrow(anime_R17_top)

# 4. Anime qui ne sont PAS R17
anime_not_R17 = subset(dfAnime, Rating != "R - 17+ (violence & profanity)")
nrow(anime_not_R17)

# 5. Anime Rating : PG - Children et G - All Ages
anime_kids = subset(dfAnime, Rating %in% c("PG - Children", "G - All Ages"))
nrow(anime_kids)

# 6. Anime qui ne sont PAS PG - Children ou G - All Ages
anime_not_kids = subset(dfAnime, !Rating %in% c("PG - Children", "G - All Ages"))
nrow(anime_not_kids)
