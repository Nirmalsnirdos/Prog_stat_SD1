# TD3 - OBJECTIF : Importation, Type Factor, Agrégation et Découpage de données

# Exercice 1 - Importer les données

# Charger la librairie nécessaire pour lire les fichiers Excel
library(readxl)

# 1. Importer le jeu de données
# On précise le chemin du fichier et le nom de l'onglet (sheet)
pokemon = read_excel(path = "pokemon.xlsx", sheet = "pokemon")

# 2. Vérifier les dimensions du dataset
dim(pokemon)   # Affiche (Lignes, Colonnes)
nrow(pokemon)  # Nombre de lignes uniquement
ncol(pokemon)  # Nombre de colonnes uniquement

# 3. Premier résumé des données
# Note : les colonnes textuelles ne donnent pas de stats intéressantes ici
summary(pokemon)

# 4. Conversion en type 'factor'
# Le type factor dit à R que ce sont des catégories (variables qualitatives)
pokemon$is_legendary = as.factor(pokemon$is_legendary)
pokemon$generation = as.factor(pokemon$generation)
pokemon$type = as.factor(pokemon$type)

summary(pokemon)


# Exercice 2 - Création de colonne

# La fonction ifelse()

# 1. Créer attack_group (basé sur la médiane)
med = median(pokemon$attack)
pokemon$attack_group = ifelse(pokemon$attack >= med, "attack+", "attack-")
pokemon$attack_group = as.factor(pokemon$attack_group)
summary(pokemon$attack_group)

# 2. Créer water_fire (utilisation de %in% pour tester plusieurs valeurs)
pokemon$water_fire = ifelse(pokemon$type %in% c("water", "fire"), "yes", "no")
pokemon$water_fire = as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

# 3. Créer la colonne 'best' (Top 25% en Attack, Defense ET Speed)
q3_attack  = quantile(pokemon$attack, probs = 0.75)
q3_defense = quantile(pokemon$defense, probs = 0.75)
q3_speed   = quantile(pokemon$speed, probs = 0.75)

pokemon$best = ifelse(pokemon$attack > q3_attack & 
                         pokemon$defense > q3_defense & 
                         pokemon$speed > q3_speed, "yes", "no")
pokemon$best = as.factor(pokemon$best)
summary(pokemon$best)

# La fonction is.na() (Gestion des valeurs manquantes)

# 1. Filtrer les lignes avec poids manquant
requete_manquant = subset(pokemon, is.na(weight_kg))

# 2. Filtrer les lignes SANS poids manquant (! signifie "NOT")
requete_complet = subset(pokemon, !is.na(weight_kg))

# 3. Remplacer les NA par la médiane (imputation)
med_weight_kg = median(pokemon$weight_kg, na.rm = TRUE) # na.rm = TRUE pour ignorer les NA dans le calcul
pokemon$weight_kgNa = ifelse(is.na(pokemon$weight_kg), med_weight_kg, pokemon$weight_kg)

med_height_m = median(pokemon$height_m, na.rm = TRUE)
pokemon$height_mNA = ifelse(is.na(pokemon$height_m), med_height_m, pokemon$height_m)

#  La fonction cut() (Discrétisation / Création de tranches)

# 1. Découpage automatique en 3 tranches de même largeur
pokemon$weight_group = cut(pokemon$weight_kg, 
                            breaks = 3, 
                            labels = c("léger", "moyen", "lourd"))

# 2. Découpage avec des bornes spécifiques
pokemon$height_m_group = cut(pokemon$height_m, 
                              breaks = c(0, 1, 2, 3, max(pokemon$height_m, na.rm = TRUE)))

# 3. Découpage par quartiles
pokemon$defense_group = cut(pokemon$defense, 
                             breaks = quantile(pokemon$defense, na.rm = TRUE),
                             include.lowest = TRUE)
summary(pokemon$defense_group)


# Exercice 3 - Agrégation

# 1. Moyenne d'attaque par type
aggregate(x = attack ~ type, data = pokemon, FUN = mean)

# 2. Médiane d'attaque par génération et type
aggregate(x = attack ~ generation + type, data = pokemon, FUN = median)

# 3. Effectif par type (on compte les numéros de pokedex)
aggregate(x = pokedex_number ~ type, data = pokemon, FUN = length)

# 4. Stats multiples (Moyenne, Médiane, Effectif) pour la vitesse
# On crée une fonction personnalisée à l'intérieur de FUN
aggregate(speed ~ generation + type, 
          data = pokemon, 
          FUN = function(x) {
            c(moyenne = mean(x), 
              mediane = median(x), 
              effectif = length(x))
          })