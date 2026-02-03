# EXERCICE 1 : Fonctions de base (c, rm, print)

# 1. Créer un objet 'a' avec la valeur 10 et un objet 'b' avec la valeur 5.
a = 10
b = 5

# 2. Multiplier les deux objets et stocker le résultat dans 'resultat'. Afficher.
resultat = a * b
print(resultat)

# 3. Créer un objet 'A' (7.2) et 'B' (10.1). Différence avec a et b ?
A = 7.2
B = 10.1
print("Le langage R est sensible à la casse (majuscule/minuscule) car nous avons 4 objets distincts : a, b, A et B")

# 4. Additionner A et B, stocker dans 'resultat'. Qu'advient-il de l'ancien 'resultat' ?
resultat = A + B
print(resultat)
print("La précédente valeur de l'objet resultat a été écrasée et remplacée par la somme de A et B.")

# 5. Supprimer l'ensemble des objets avec rm().
rm(list = ls())

# EXERCICE 2 : Fonctions usuelles (seq, rep, length, class)

# --- Les vecteurs et types ---

# 1. Vecteur numérique 1 à 5, afficher classe et 3ème élément.
vecteur = c(1, 2, 3, 4, 5)
class(vecteur)
vecteur[3]

# 2. Créer v1 (1:5) et v2 (v1 + 3).
v1 = 1:5
v2 = v1 + 3

# 3. Créer v3 (1:6), v4 (carrés de v3) et v5 (v4 / 2).
v3 = 1:6
v4 = v3^2
v5 = v4 / 2

# 4. Vecteur jours de la semaine, classe, 2ème et 7ème éléments.
jours = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
class(jours)
jours[c(2, 7)]

# 5. Vecteur booléen et classe.
bool_vec = c(TRUE, FALSE, TRUE, FALSE, TRUE)
class(bool_vec)

# 6. Vecteur décimal, classe, afficher tout sauf le 3ème.
dec_vec = c(1.2, 2.5, 3.8, 4.1, 5.6)
class(dec_vec)
dec_vec[-3]

# 7. Vecteur mois, classe, premier trimestre.
mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
class(mois)
mois[1:3]

# 8. Vecteur négatif, classe, dernier et premier éléments.
neg_vec = c(-1, -2, -3, -4, -5)
class(neg_vec)
neg_vec[c(length(neg_vec), 1)]

# 9. Vecteur fruits, classe, tout sauf les deux premiers.
fruits = c("Pomme", "Banane", "Orange", "Fraise", "Ananas")
class(fruits)
fruits[-c(1, 2)]

# 10. Vecteur avec NA et classe.
missing_vec = c(1, 2, NA, 4, 5)
class(missing_vec)

# --- Séquences (seq) et Longueurs (length) ---

# Séquence 1 à 10
s1 = seq(1, 10); length(s1)
# Séquence pairs 2 à 20
s2 = seq(2, 20, by = 2); length(s2)
# Séquence décroissante 0 à -5
s3 = seq(0, -5); length(s3)
# Séquence 5 à 50 par pas de 5
s4 = seq(5, 50, by = 5); length(s4)
# Séquence 10 à 1
s5 = seq(10, 1, by = -1); length(s5)
# Séquence 0 à 1 par pas de 0.1
s6 = seq(0, 1, by = 0.1); length(s6)
# Séquence 5 à -5 par pas de -1
s7 = seq(5, -5, by = -1); length(s7)
# Séquence 1 à 10 (impairs)
s8 = seq(1, 10, by = 2); length(s8)

# --- Répétitions (rep) ---

# Répéter 3 cinq fois
r1 = rep(3, times = 5)
# Répéter A, B, C trois fois
r2 = rep(c("A", "B", "C"), times = 3)
# Répéter séquence 1:3 trois fois
r3 = rep(1:3, times = 3)
# Répéter TRUE/FALSE quatre fois
r4 = rep(c(TRUE, FALSE), times = 4)

# Nettoyage
rm(list = ls())

# EXERCICE 3 : Statistiques et Simulation (runif, rnorm, sample, etc.)

# --- Distribution Uniforme (runif) ---

# 1. 5 nombres entre 0 et 1
v_unif1 = runif(n = 5, min = 0, max = 1)
print(v_unif1)
mean(v_unif1)
median(v_unif1)
min(v_unif1)
max(v_unif1)

# 2. 10 nombres entre -5 et 5
v_unif2 = runif(n = 10, min = -5, max = 5)
print(v_unif2)
mean(v_unif2)
median(v_unif2)
min(v_unif2)
max(v_unif2)

# 3. 100 nombres entre 10 et 20
v_unif3 = runif(n = 100, min = 10, max = 20)
mean(v_unif3)
median(v_unif3)

# 4. 15 nombres entre 50 et 100
v_unif4 = runif(15, 50, 100)
mean(v_unif4)
max(v_unif4)

# --- Distribution Normale (rnorm) ---

# 1. n=20, moyenne=-2, écart-type=3
echan1 = rnorm(n = 20, mean = -2, sd = 3)
hist(echan1)
print(paste("Moyenne:", mean(echan1), "| SD:", sd(echan1)))

# 2. n=2000, moyenne=-2, écart-type=3
echan2 <- rnorm(n = 2000, mean = -2, sd = 3)
hist(echan2)

# 3. n=2000, moyenne=0, écart-type=1 (Normale centrée réduite)
echan3 <- rnorm(n = 2000, mean = 0, sd = 1)
hist(echan3)        #pour répresenter en histogramme

# 4. Quantiles, Déciles et Centiles (sur echan3)
quantile(echan3, probs = c(0.25))
quantile(echan3, probs = c(0.50))
quantile(echan3, probs = c(0.75))
quantile(echan3, probs = seq(0, 1, 0.1)) # Déciles
quantile(echan3, probs = seq(0, 1, 0.01)) # Centiles

# --- Cas pratique : Salaires ---

# 1. Simulation 3000 salaires (moyenne=2400, écart-type=300)
echan_sal = rnorm(n = 3000, mean = 2400, sd = 300)
# 2. Arrondir à 2 décimales
echan_sal = round(echan_sal, 2)
# 3. Masse salariale
Masse_salariale = sum(echan_sal)
# 4. Médiane
print(paste("Salaire médian :", median(echan_sal)))
# 5. Interprétation Quantile 99%
q99 = quantile(echan_sal, probs = 0.99)

# --- Dés et Probabilités (sample) ---

# 1. Un lancé de dé
sample(1:6, size = 1)
# 2. 12 lancés
simulation = sample(1:6, size = 12, replace = TRUE)
# 3. Valeurs uniques apparues
unique(simulation)
# 4. Fréquences et tri
table(simulation)
sort(table(simulation))
# 5. Pourcentages
prop.table(table(simulation))
print("Avec n=12, les fréquences sont éloignées de la probabilité théorique (1/6 ≈ 0.166).")

# 6. Simulation 100 000 lancés
simulation_large = sample(1:6, 100000, replace = TRUE)
frequences = prop.table(table(simulation_large))

print(sort(frequences, decreasing = TRUE))
print("Avec n=100 000, la loi des grands nombres s'applique : on se rapproche de 16.6% pour chaque face.")