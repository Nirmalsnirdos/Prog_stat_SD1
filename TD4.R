# TD4

# Exercice 1 - Importer les données

# 1. Importer le jeu de données velov.csv à l’aide de la fonction read.csv().
df = read.csv(file = "C:/Users/nirma/Downloads/velov.csv", 
              header = TRUE, 
              sep = ";", 
              dec = "," )

# 2. Effectuer un résumé des données. 
# On constate que des variables sont mal typées comme status et CodePostal.
summary(df)
View(df)
class(df$status)
class(df$CodePostal)

# 3. Passer ces deux variables en type factor.
df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)

# 4. On souhaite vérifier s'il y a des bornes indisponibles sur les stations. 
# Il suffit de vérifier si le nombre de vélos et places disponibles est égal à la capacity de la station. 
# Créer une colonne nommée bornes avec la valeur OK ou KO s'il y a un ou plusieurs bornes indisponibles sur une station. 
# Combien y a-t-il de stations avec au moins une bornes HS ?
df$bornes = ifelse(df$capacity == (df$bikes + df$stands), "OK" , "KO")
View(df)

# Exercice 2 - L'histogramme

# La fonction hist()

#1. Construire un histogramme de la distribution des capacity à l'aide de la fonction hist(). 
# N'oublier pas de mettre un titre.
hist(x = df$capacity, main = "Histogramme des capacité des stations", 
     xlab = "Capacity",
     col="green",
     breaks = 20,
     probability = FALSE,
     ylim = c(0,200),
     xlim = c(0,130))
      
hist(x = df$capacity, 
     main = "Histogramme des capacité des stations")

# 2. Construire le même graphique mais avec 6 classes.
hist(x = df$capacity, 
     main = "Histogramme des capacité des stations",
     breaks = 6 )

# 3. Construire le même graphique mais en rouge.
hist(x = df$capacity, 
     main = "Histogramme des capacité des stations",
     breaks = 6,
     col = "red")

# 4. Renommer l'axe des abscisses par Capacity.
hist(x = df$capacity, 
     main = "Histogramme des capacité des stations",
     breaks = 6,
     col = "red",
     xlab = "Capacity")

# La fonction abline()

# 5. Sur le dernier graphique, ajouter une ligne horizontale bleue qui à pour ordonné la valeur 100, à l'aide de la fonction abline(). 
# La ligne s'ajoute au graphique en cours de lecture dans la fenêtre graphique. 
# Vous pouvez personnaliser le trait de la ligne avec l'argument lty.
abline(h = 100,
       col = "blue",
       lty = 4)

# Les fonctions hist(), lines() et density()

# 6. Construire le même graphique mais avec la densité plutôt que les effectifs. 
# Supprimer l'argument break pour rétablir les classes par défaut.
hist(x = df$capacity, 
     main = "Histogramme des capacité des stations",
     breaks = 6,
     col = "red",
     xlab = "Capacity",
     probability = TRUE)

# 7. Ajouter la courbe densité de cette distribution à l'aide des fonctions lines() et density(). 
# On peut mettre cette courbe en bleu en changeant la taille de la courbe avec l'argument lwd.
lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 4)

# 8. Pour voir la courbe density en entier, modifier les bornes de l'axe des ordonnées de l'histogramme avec l'argument ylim. 
# Relancer l'ensemble des commandes pour tracer à nouveau le graphique.
hist(x = df$capacity, 
     main = "Histogramme des capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08))

lines(density(df$capacity),
      lty = 2,
      col = "blue",
      lwd = 2)

# Exercice 3 - Le boxplot

# La fonction boxplot()

# 1. Construire une boîte à moustache de la distribution des capacity à l'aide de la fonction boxplot(). 
# N'oublier pas de mettre un titre.
boxplot(x = df$capacity,
        main = "Boxplot des capacité des stations")

# 2. Construire le même graphique mais pivoter horizontalement.
boxplot(x = df$capacity,
        main = "Boxplot des capacité des stations",
        horizontal = TRUE)

# 3. Construire le même graphique en le remettant à la verticale et en n'affichant pas les valeurs atypiques.
boxplot(x = df$capacity,
        main = "Boxplot des capacité des stations",
        horizontal = FALSE,
        outline =  FALSE)

# 4. Ajouter un point supplémentaire qui correspond à la moyenne de la série avec la fonction points(). 
# On souhaite que ce point soit un gros carré rouge
moyenne = mean(df$capacity, na.rm = TRUE)
points (moyenne,
          col = "red", 
          pch =  15, 
          cex = 2)

# La fonction par()

# 5. On souhaite comparer les vélos disponibles sur le 7ème et le 8ème arrondissement. 
# Diviser la fenêtre graphique en deux puis constuire un boxplot pour ces deux arrondissement. 
# Que peut-on dire ?
par(mfrow=c(1,2))
#7ème
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))
#C'est plus simple d'analyser les deux graphiques si la borne des ordonnées est la même.
# On remarque que la disponibilité des stations est plus homogènes sur le 8ème.

# 6. Sur le même graphique, on souhaite analyser le nombre de vélos disponibles en fonction de la variable bonus. 
# Que peut-on dire ?
par(mfrow=c(1,1)) 
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")

# Les fonctions points() et tapply().

# 7. Ajouter les moyennes de chaque groupes sur le graphique à l'aide de la fonction tapply() et points().
moyennes = tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X))
points(moyennes, col = "blue", pch = 19)

# Exercice 4 - Le diagramme

# Les fonctions barplot() et table().

# 1. Créer un diagramme en barre de la réparition du nombre de station bonus à l'aide de la fonction barplot(). 
# N'oublier pas de mettre un titre.
effectif = table(df$bonus)
barplot(height = effectif,
        main = "Répartition du nombre de station bonus"
        )

# 2. Construire le même graphique mais pivoter horizontalement.
barplot(height = effectif,
        main = "Répartition du nombre de station bonus",
        horiz = TRUE
        )

# Les fonctions barplot() ,prop.table() et legend().
# 3. Construire le même graphique mais en pourcentage.
frequence = prop.table(effectif)
barplot(height = frequence,
        main = "Répartition en % du nombre \n de station bonus",
        horiz = TRUE)

# 4. Construire un diagramme bivariés avec la répartition du nombre de station bonus en fonction du nombre de station avec un terminal de paiement. 
# Les deux variables ayant les mêmes modalités TRUE / FALSE, il est important de définir le nom de l'axe des abscisses. 
# Que remarque t-on ?
effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")
#On remarque qu'on ne sait pas distinguer les deux modalités car il n'y a pas de légende.

# 6. Même question mais en pourcentage colonne.
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

# 7. Même question mais avec un diagramme bivarié non empilé à l'aide de l'argument beside.
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"),
        beside = TRUE)

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

# 8. Créer un diagramme circulaire de la réparition du nombre de station bonus à l'aide de la fonction pie() en différenciant les deux catégories avec la couleur jaune et vert. 
# N'oublier pas de mettre un titre.
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"))

# 9. Construire le même graphique à l'aide de l'argument labels et la fonction paste() afin d'ajouter les etiquettes de données avec les effectifs.
etiquette = paste(rownames(effectif),"\n",effectif)
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"),
    labels = etiquette)

# Les fonctions palette() et colors().
# 10. Construire dans un diagramme en barre le top 10 des codes postaux avec le plus de station velo'v. 
# On peut pivoter les étiquettes à l'aide de l'argument las pour une meilleur lecture du graphique. Utiliser la fonction palette() comme couleur pour les barres. 
# Que remarque t-on ?
effectif = table(df$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = palette(),
        las = 2)  # Rotation des étiquettes à 90 degrés
#On remarque que les deux premières couleurs se répetent.
print(palette()) # la fonction `palette()` ne dispose que de 8 couleurs

# 11. Même question mais avec la fonction colors(). 
# Elle donne accès à plus de 650 couleurs.
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = colors(),
        las = 2)  # Rotation des étiquettes à 90 degrés

print(colors())

# La fonction dev.print().
#12. Exporter ce graphique dans un format .PNG à l'aide de la fonction dev.print().
dev.print(device = png, file = "export.png", width = 600)

# Exercice 5 - Nuage de points

# La fonction plot().

# 1. A l'aide de la fonction plot(), construire un nuage de point pour étudier la corrélation entre le nombre de place disponible sur les stations et leur capacité. 
# N'oublier pas d'ajouter un titre.
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité")

# 2. Construire le même graphique en zoomant avec des abscisses et ordonnées qui vont de 0 à 60 et avec des points avec un fond noir.
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     pch=19)

# Les fonctions plot() et levels().
#1. Construire le même graphique en affichant deux couleurs différentes selon la colonne bornes créé précédemment. 
# La colonne bornes doit avoir le type factor.
df$bornes = as.factor(df$bornes)
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     col = df$bornes,
     pch=19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = palette(), pch = 19)

# 4. Pour pouvoir choisir ses couleurs, il suffit d'utiliser un vecteur avec les couleurs qu'on souhaite.
myColors <- c("red", "blue", "green")  
# Ajoutez plus de couleurs si nécessaire avec le code HTML des couleurs à la place des noms

# Tracer le graphique
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0, 60),
     ylim = c(0, 60),
     col = myColors[df$bornes],
     pch = 19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = myColors, pch = 19)

# 5. Ajouter un carré vert sur le graphique représentant la moyenne du nombre de places disponibles et la capacité des stations.
moy_stands = mean(df$stands)
moy_capacity = mean(df$capacity)
points(x = moy_stands,y = moy_capacity, 
       pch = 15,
       col = myColors[3],
       cex = 2)

# Exercice 6 - Cartographie (spoil sur le SD2)
# Librairies nécessaires
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(
    ~position_longitude,
    ~position_latitude, 
    popup = ~address,
    clusterOptions = markerClusterOptions()
  )
