# TP4

# Construire des structures conditionnelles
# Construire des boucles
# Créer des fonctions
# Utilisation des fonctions de test logique

# Exercice 1 - Création de fonction

# La commande function()

# 1. Créer une fonction salaire_net_cadre() qui prend en entrée le salaire mensuel brut d'un cadre et qui retourne le salaire net avant impôt. 
# Ne pas tenir compte du temps de travail. 
# Tester la fonction pour vérifier.
salaire_net_cadre = function(salaire_brut) {
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) }
#Test de la fonction
salaire_net_cadre(salaire_brut = 3000)

# 2. Modifier la fonction pour que ce salaire mensuel brut avant impôt soit de 2500€ si l'utilisateur ne renseigne pas ce paramètre.
salaire_net_cadre = function(salaire_brut = 2500) {
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) }

salaire_net_cadre()

# 3. Modifier la fonction salaire_net_cadre() en ajoutant le paramètre temps de travail avec comme valeur par défaut 100%. 
# Ne pas tenir compte du taux de prélèvement à la source. 
# Tester la fonction pour vérifier.
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
#Test de la fonction
salaire_net_cadre(salaire_brut = 3000,
                  temps_travail = 0.8)

# La commande if() {}
# 4. Modifier la fonction salaire_net_cadre() pour que la fonction retourne une erreur si le salaire mensuel brut en entrée n'est pas une valeur numerique. 
# Tester la fonction pour vérifier.
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
#Test de la fonction
salaire_net_cadre(salaire_brut = "2000€")
salaire_net_cadre(salaire_brut = 2000)

# 5. Modifier la fonction salaire_net_cadre() pour que la fonction retourne une erreur si le temps de travail n'est pas numérique et n'est pas compris entre 0 et 1. 
# Tester la fonction pour vérifier.
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail)) {
    return("Erreur :  le temps de travail doit doit être une valeur numérique")
  }
  
  if ( (temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
#Test de la fonction
salaire_net_cadre(salaire_brut = 2000, temps_travail = "100%")
salaire_net_cadre(salaire_brut = 2000, temps_travail = 0.8)
salaire_net_cadre(salaire_brut = 2000, temps_travail = 100)

# La commande else() {}

# 6. Créer une nouvelle fonction salaire_net() identique à la précédente mais avec un paramètre en plus qui est le statut cadre/non cadre du salarié. 
# La fonction retourne également une erreur si le statut n'est pas cadre ou non cadre. 
# Tester la fonction pour vérifier.
salaire_net = function(salaire_brut = 2500,temps_travail = 1, statut) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail)) {
    return("Erreur :  le temps de travail doit doit être une valeur numérique")
  }
  
  if ( (temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  if (!statut %in% c("cadre","non cadre")) {
    return("Erreur :  le statut doit être cadre ou non cadre")
  }
  
  if (statut == "cadre") {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  
  return(salaire_net_avant_impot) 
}
#Test de la fonction
salaire_net(salaire_brut = 2000, statut = "cadre")
salaire_net(salaire_brut = 2000, statut = "non cadre")
salaire_net(salaire_brut = 2000, statut = "technicien")

# La commande else if() {}

# 7.Modifier la fonction précédente pour calculer le salaire net mensuel après prélèvement à la source. 
# Tester la fonction pour vérifier.
salaire_net = function(salaire_brut = 2500,temps_travail = 1, statut) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail)) {
    return("Erreur :  le temps de travail doit doit être une valeur numérique")
  }
  
  if ( (temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  if (!statut %in% c("cadre","non cadre")) {
    return("Erreur :  le statut doit être cadre ou non cadre")
  }
  
  if (statut == "cadre") {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  
  if (salaire_net_avant_impot <= 1591) {
    salaire_net_apres_impot <- salaire_net_avant_impot
  } else if (salaire_net_avant_impot <= 2006) {
    salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.029)
  } else if (salaire_net_avant_impot <= 3476) {
    salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.099)
  } else if (salaire_net_avant_impot <= 8557) {
    salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.20)
  } else {
    salaire_net_apres_impot <- salaire_net_avant_impot * (1 - 0.43)
  }
  
  return(salaire_net_apres_impot) 
}

# 8. Créer une fonction shifumi() qui demande à l'utilsateur de saisir une valeur dans la console entre pierre , papier ou ciseaux. 
# La fonction simule également un de ces trois choix à l'aide de la fonction sample() puis retourne le résultat. 
# Tester la fonction pour vérifier.
shifumi <- function() {
  # Demander à l'utilisateur de saisir une valeur
  choix_utilisateur <- readline(prompt = "Choisissez entre pierre, papier ou ciseaux : ")
  
  # Vérifier si l'utilisateur a saisi une valeur valide
  if (choix_utilisateur %in% c("pierre", "papier", "ciseaux")) {
    # Simuler un choix aléatoire pour l'ordinateur
    choix_ordi <- sample(c("pierre", "papier", "ciseaux"), 1)
    
    # Afficher les choix de l'utilisateur et de l'ordinateur
    cat("Votre choix :", choix_utilisateur, "\n")
    cat("Choix de l'ordinateur :", choix_ordi, "\n")
    
    # Retourner le résultat du jeu
    if (choix_utilisateur == choix_ordi) {
      return("Égalité !")
    } else if ((choix_utilisateur == "pierre" & choix_ordi == "ciseaux") |
               (choix_utilisateur == "papier" & choix_ordi == "pierre") |
               (choix_utilisateur == "ciseaux" & choix_ordi == "papier")) {
      return("Vous avez gagné !")
    } else {
      return("L'ordinateur a gagné !")
    }
  } else {
    return("Valeur invalide. Veuillez choisir entre pierre, papier ou ciseaux.")
  }
}

#Test de la fonction
shifumi()

# Exercice 2 - Création des boucles

# 1. Somme cummulée : Créer une boucle for() qui parcourt les éléments du vecteur c(1,2,3,4,5) un par un. 
# À chaque itération de la boucle, ajouter l'élément en cours au résultat précédent et afficher le résultat.
resultat = 0
for (element in c(1,2,3,4,5)) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
}

# 2. Somme cummulée : Créer une boucle while() qui calcule la somme cumulative des nombres entiers à partir de 1 jusqu'à ce que la somme dépasse 50, en affichant le résultat à chaque étape ainsi que la valeur actuelle de l'élément à laquelle la boucle s'est arrêtée.
element = 1
resultat = 0
while (resultat <= 50) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
  print(paste("le programme s'est arrêté à la valeur : ", element))
  element = element + 1
}

# 3. Parcourir toutes les colonnes du dataframe iris à l'aide d'une boucle for(). 
# Pour chaque itération afficher la type de la colonne.
for (colonne in colnames(iris)) {
  type_colonne = class(iris[ , colonne])
  print(paste("la colonne ", colonne, " est de type : ", type_colonne))
}

# 4. Même question mais avec une boucle while().
# Initialisation de l'indice de colonne
indice_colonne <- 1

# Tant qu'il reste des colonnes à parcourir dans iris
while (indice_colonne <= ncol(iris)) {
  # Récupération du nom de la colonne
  nom_colonne <- colnames(iris)[indice_colonne]
  
  # Récupération du type de données de la colonne
  type_colonne <- class(iris[, nom_colonne])
  
  # Affichage du résultat
  print(paste("la colonne ", nom_colonne, " est de type : ", type_colonne))
  
  # Passage à la colonne suivante
  indice_colonne <- indice_colonne + 1
}

# Exercice 3 - GOAT : Cas pratiques

# 1. Demander à l'utilisateur de saisir un nombre à l'aide de la fonction readline() pour afficher son carré. 
# Faire cette opération 5 fois avec une boucle for().
# Boucle pour demander 5 fois un nombre à l'utilisateur
for (i in 1:5) {
  
  # Demander à l'utilisateur d'entrer un nombre
  nombre <- readline(prompt = "Entrez le nombre :")
  nombre <- as.numeric(nombre)
  
  # Calculer le carré du nombre
  carre <- nombre^2
  
  # Afficher le carré du nombre
  print(paste("Le carré de", nombre, "est", carre))
}

# 2. Construire une boucle qui parcourt les fichiers d'une dossier pour connaître leur taille en octets.
# Chemin du dossier à explorer
dossier <- "C:/Users/nirma/Downloads"

# Liste les fichiers dans le dossier spécifié
fichiers <- list.files(dossier, full.names = TRUE)

# Affiche la taille de chaque fichier
for (fichier in fichiers) {
  info <- file.info(fichier)
  taille <- info$size
  cat("Le fichier", basename(fichier), "a une taille de", taille, "octets.\n")
}

# 3. Créer une boucle qui parcout toutes les colonnes du dataframe iris. 
# Si la colonne est de type numeric contruire un boxplot() avec un titre, sinon construire un barplot() avec un titre.
# Parcourir toutes les colonnes du dataframe iris
for (colonne in colnames(iris)) {
  # Vérifier si la colonne est de type numeric
  if (is.numeric(iris[,colonne])) {
    # Si c'est le cas, construire un boxplot avec un titre
    boxplot(iris[,colonne], main = paste("Boxplot de", colonne))
  } else {
    # Sinon, construire un barplot avec un titre
    barplot(table(iris[,colonne]), main = paste("Barplot de", colonne))
  }
}

# 4. Construire une boucle while() qui permet à un utilisateur de jouer au shifumi jusqu'à ce qu'il souhaite arreter.
# Boucle while pour jouer au shifumi jusqu'à ce que l'utilisateur décide d'arrêter
continuer <- TRUE
while (continuer) {
  # Appeler la fonction shifumi et afficher le résultat
  resultat <- shifumi()
  cat("Résultat du jeu :", resultat, "\n")
  
  # Demander à l'utilisateur s'il souhaite continuer
  reponse <- readline(prompt = "Voulez-vous continuer à jouer ? (oui/non) : ")
  
  # Vérifier la réponse de l'utilisateur
  if (tolower(reponse) == "non") {
    print("Arrêt du jeu.")
    continuer <- FALSE  # Mettre fin à la boucle
  }
}

# 5. Le juste prix : Créer une fonction juste_prix() qui demande à l'utilisateur de saisir un nombre jusqu'à trouver le bon nombre généré par le programme. Le programme retourne c'est plus ou c'est moins jusqu'à ce que l'utilisateur trouve la bonne réponse. 
# Tester votre fonction
# Fonction pour le jeu du juste prix
juste_prix <- function() {
  # Génération d'un nombre aléatoire entre 1 et 100
  nombre_a_deviner <- sample(1:100, 1)
  
  # Initialisation de la réponse de l'utilisateur
  reponse <- -1
  
  # Boucle tant que l'utilisateur n'a pas trouvé le bon nombre
  while (reponse != nombre_a_deviner) {
    # Demande à l'utilisateur de saisir un nombre
    reponse <- as.integer(readline(prompt = "Devinez le nombre : "))
    
    # Vérifie si le nombre est trop grand, trop petit ou correct
    if (reponse < nombre_a_deviner) {
      cat("C'est plus !\n")
    } else if (reponse > nombre_a_deviner) {
      cat("C'est moins !\n")
    } else {
      cat("Bravo, vous avez trouvé le juste prix !\n")
    }
  }
}

# Appel de la fonction juste_prix pour commencer le jeu
juste_prix()
