
#1 Afficher le jeu de données iris puis afficher la classe de l'objet iris.
iris
class(iris)

#2 Afficher le jeu de données iris dans une vue avec la fonction View()
View(iris)

#3 Afficher le nombre de lignes avec la fonction nrow()
nrow(iris)

#4 Afficher le nombre de colonne avec la fonction ncol()
ncol(iris)

#5 Afficher le nom des colonnes avec la fonction colnames()
colnames(iris)

#6 Afficher un résumé du dataframe avec la fonction summary()
summary(iris)

#7 Afficher uniquement les colonnes Sepal.Length et Species
iris[ , c("Sepal.Length","Species")]

#8 Afficher uniquement la ligne 100,103 et 105
iris[c(100, 103, 105),]

#9 Afficher uniquement les lignes 50 à 100
iris[c(50:100),]

#10 Calculer la moyenne de la variable Sepal.Length
mean(iris$Sepal.Length)

#11 Calculer la médiane de la variable Sepal.Width
mean(iris$Sepal.Width)

#12 Calculer l'écart-type de la variable Petal.Length
sd(iris$Petal.Length)

#13 Calculer les déciles de la variable Petal.Width
quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by =0.1))

#1 Importer le jeu de données manga.csv dans un objet appelé dfManga et le jeu de données anime.csv dans un objet appelé dfAnime
dfManga = read.csv("C:/Users/nirma/Downloads/manga.csv", header = TRUE, sep = ",", dec = ".")

# Puis afficher la classe des deux objets pour vérifier que la classe est data.frame.
class(dfManga)

dfAnime <- read.csv("C:/Users/nirma/Downloads/anime.csv", header = TRUE, sep = ",", dec = ".")
class(dfAnime)

#2 Afficher les jeux de données dans des vues pour les visualiser
View(dfManga)
View(dfAnime)

#3 Afficher le nombre de lignes et colonnes avec la fonction dim()
dim(dfAnime)
dim(dfManga)

#4 Calculer la moyenne de la variable Score pour les deux dataframe. Quelle est la moyenne la plus élevée ?
mean(dfAnime$Score)
mean(dfManga$Score)

#5 Calculer le nombre total de votes de la variable Vote pour les deux dataframe. Ou y a t-il le plus de votes ?
sum(dfAnime$Vote)
sum(dfManga$Vote)

#6 Calculer l'écart-type des notes de la variable Score pour les deux dataframe. Ou est l'échantillon le plus homogènes au niveau des Score ?
sd(dfAnime$Score)
sd(dfManga$Score)

#7 Calculer les déciles des notes de la variable Score pour les deux dataframe. Quelle dataframe a le décile 1 le plus petit ?
quantile(dfAnime$Score, probs = seq(from = 0.1, to = 0.9, by =0.1))
quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by =0.1))

#1 Combien de Manga ont une note strictement supérieure à 9/10 ?
note_sup9 = subset(dfManga, Score > 9)
nrow(note_sup9)

#2 Combien de Manga ont 200000 votes ou plus ?
vote_sup200M = subset(dfManga, Vote >199999)
nrow(vote_sup200M)

#3 Combien de Manga ont strictement plus de 200000 votes et plus de 8/10 ?
condi = subset(dfManga,Vote>200000 & Score>8)
nrow(condi)

#4 Combien de Manga ont une note comprise entre 7/10 et 8/10 ?
note7a8 = subset(dfManga, Score>=7 & Score<=8)
nrow(note7a8)

#filtre sur les Animes

#1 Calculer les effectifs de la variable Rating(). Combien de modalité de la variable existe-t-il ? Calculer ensuite les effectifs en pourcentage.
effectifRating <- table(dfAnime$Rating)
print(effectifRating)
length(effectifRating)
prop.table(effectifRating)

#2 Combien d'Anime sont concernés par le Rating : R - 17+ (violence & profanity) ?
R17 = subset(dfAnime, Rating=="R - 17+ (violence & profanity)")
nrow(R17)

#3 Combien d'Anime sont concernés par le Rating : R - 17+ (violence & profanity) et ont une note supérieur à 8/10 ?
R3 = subset(dfAnime, Rating=="R - 17+ (violence & profanity)" & Score> 8)
nrow(R3)

#4 Combien d'Anime ne correspondent pas au Rating : R - 17+ (violence & profanity) ?
R4 = subset(dfAnime, Rating!="R - 17+ (violence & profanity)" )
nrow(R4)

#5 Combien d'Anime correspondent au Rating : PG - Children et G - All Ages ?
R5 = subset(dfAnime, Rating %in% c("PG - Children","G - All Ages"))
nrow(R5)

#6 Combien d'Anime ne correspondent pas au Rating : PG - Children et G - All Ages ?
R6 = subset(dfAnime, !Rating %in% c("PG - Children","G - All Ages"))
nrow(R6)

#7 Combien d'Anime ont une note supérieure à 9/10 ou ont plus de 400000 votes ?
R7 = subset(dfAnime, Score >=9 | Vote > 400000)
nrow(R7)

# Les fonctions rbind() et write.table()

#1 Modifier les deux dataframe en ne conservant que les variables :
dfAnime = dfAnime[ , c("Title","Score","Vote","Ranked")]
View(dfAnime)
dfManga = dfManga[ , c("Title","Score","Vote","Ranked")]
View(dfManga)

#2 Pour chaque dataframe créer une colonne Type avec pour valeur Anime ou Manga selon l'objet.
dfAnime$Type <- "Anime"
dfManga$Type <- "Manga"

#3 Compiler les deux dataframe avec la fonction rbind() dans un objet appelé dfConcat. Vérifier le résultat avec dans une vue.
dfConcat = rbind(dfAnime,dfManga)
View(dfConcat)

#4 Exporter le dataframe dans un fichier csv nommée ExportTp1.csv avec la fonction write.table.
write.table(x = dfConcat, file = "C:/Users/nirma/Downloads/ExportTp1.csv",
            sep = ";",row.names = FALSE)



