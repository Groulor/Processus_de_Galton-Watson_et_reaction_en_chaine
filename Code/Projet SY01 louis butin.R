#EXERCICE 1 :
#Question 1)
generation_n_exo1_DETAILLEE <- function(n) {
  cat("Génération 0", ":", 1, "\n")
  # On commence avec 1 individu
  nbrINDIVIDU <- 1

  # Boucle pour chaque génération de 1 à n
  for (generation in 1:n) {
    # Si le nbrINDIVIDU (la population) est éteint, on retourne 0 directement
    if (nbrINDIVIDU == 0) {
       cat("Extinction à la génération", generation, "\n")
      return(0)
    }

    # Pour chaque individu de nbrINDIVIDU actuel,on simule son nombre d'enfants (0 ou 2)
    enfantsGENERATION <- 0
    for (i in 1:nbrINDIVIDU) {
      # Xi,n/2 suit une loi de Bernoulli(1/2) donc Xi,n = 2 * Bernoulli(1/2)
      nbENFANTS <- 2 * rbinom(1, 1, 0.5)
      enfantsGENERATION <- enfantsGENERATION + nbENFANTS
    }
    nbrINDIVIDU <- enfantsGENERATION
    cat("Génération", generation, ":", nbrINDIVIDU, "\n")
  }

  return(nbrINDIVIDU)
}
#Je simule pour n =5 mes générations afin de vérifier que tout fonctionne bien
generation_n_exo1_DETAILLEE(5)

#Question 2 :
#je vais utiliser la fonction de la q1 pour cette question, mais je ne veux pas print le
#nombre d'individu donc je supprime les 'cat' dedans et la change de nom :
generation_n_exo1 <- function(n) {
  nbrINDIVIDU <- 1

  for (generation in 1:n) {
    if (nbrINDIVIDU == 0) {
      return(0)
    }

    enfantsGENERATION <- 0
    for (i in 1:nbrINDIVIDU) {
      nbENFANTS <- 2 * rbinom(1, 1, 0.5)
      enfantsGENERATION <- enfantsGENERATION + nbENFANTS
    }
    nbrINDIVIDU <- enfantsGENERATION
  }

  return(nbrINDIVIDU)
}

#voici ma fonction pour calculer la taille moyenne
#nbSIMULATION = nombre de simulation souhaité
taille_moyenne_n_exo1 <- function(n, nbSIMULATION = 1000) {
  # Je stock les resultats de mes simulations :
  resultats <- numeric(nbSIMULATION)

  # Boucle pour réaliser le nombre de simulation souhaité
  for(i in 1:nbSIMULATION) {
    resultats[i] <- generation_n_exo1(n)
  }

  # Je calcul ma moyenne des resultats des simulations
  moyenne <- mean(resultats)
  return(moyenne)
}

# Je test de calculer la taille moyenne a la gen 3 uniquement
#taille_moyenne_n_exo1(3)

# Je fais ici une boucle pour vérifier l'évolution sur plusieurs générations
for(i in 0:5) {
  moy <- taille_moyenne_n_exo1(i)
  cat("Génération", i, ": taille moyenne =", moy, "\n")
}


#Question 3 :
# voici ma fonction pour calculer la probabilité d'extinction à une génération donnée
probaMORT_generation <- function(generation) {
  # Nombre de simulations souhaité
  nbSIMULATIONS <- 1000

  # Nombre de simulations où l'extinction s'est produite
  nombreEXTINCTION <- 0

  # Boucle pour réaliser le nombre de simulations voulu
  for(i in 1:nbSIMULATIONS) {
    # Si la taille de la génération est 0 on incrémente le compteur d'extinction
    if(generation_n_exo1(generation) == 0) {
      nombreEXTINCTION <- nombreEXTINCTION + 1
    }
  }
  return(nombreEXTINCTION / nbSIMULATIONS)
}


# je regarde les resultats sur plusieurs génération pour voir l'évolution
for(i in 1:5) {
  cat("Probabilité d'extinction à la génération", i, ":", probaMORT_generation(i), "\n")
}

#Question 4
# Fonction pour simuler un processus de Galton-Watson et vérifier l'extinction
simulerEXTINCTION <- function() {
  population <- 1  # 1 individu à gen 0

  # Je simule jusqu'à l'extinction
  while (population > 0) {
    enfants <- sapply(1:population, function(x) sample(0:1, 1, prob = c(0.5, 0.5)))
    population <- sum(enfants)  # La nouvelle taille de la population
  }

  # Je retourne 1 si extinction (si population == 0) sinon 0
  return(1)
}

# Nombre de simulations souhaité pour obtenir une estimation
nbSIMULATIONS <- 1000

# Moyenne de mes estimations
probaEXTINCTION <- mean(replicate(nbSIMULATIONS, simulerEXTINCTION()))

# affichage la probabilité d'extinction estimée
cat("Proba extinction : ", probaEXTINCTION, "\n")

#Question 5

# Voici ma fonction pour calculer le nombre moyen de génération avant extinction
generationsAVANT_extinction <- function() {
  population <- 1  # Population initiale (1 individu)
  generations <- 0  # Compteur des générations

  while (population > 0) {
    generations <- generations + 1
    enfants <- sapply(1:population, function(x) sample(c(0, 2), 1, prob = c(0.5, 0.5)))
    population <- sum(enfants)  # Mise à jour de la population
  }

  return(generations)  # Nombre de générations avant extinction
}

# Simuler plusieurs trajectoires
nbSIMULATION <- 1000
tempsEXTINCTION <- replicate(nbSIMULATION, generationsAVANT_extinction())

# Calculer la moyenne du nombre de générations avant extinction
moyenneGENERATION <- mean(tempsEXTINCTION)
cat("Nombre moyen avant extinction : ", moyenneGENERATION, "\n")

#EXERCICE 2
#Question 6

generation_n_exo2_DETAILLE <- function(n, p) {
  # Vérification du paramètre p
  if (p <= 0 || p >= 1) {
    stop("p doit être strictement entre 0 et 1")
  }

  cat("Génération 0 B(3,",p,")", ":", 1, "\n")
  # On commence avec 1 individu
  nbrINDIVIDU <- 1

  # Boucle pour chaque génération de 1 à n
  for (generation in 1:n) {
    # Si le nbrINDIVIDU (la population) est éteint, on retourne 0 directement
    if (nbrINDIVIDU == 0) {
       cat("Extinction à la génération", generation, "\n")
      return(0)
    }

    # Pour chaque individu de nbrINDIVIDU actuel, on simule son nombre d'enfants selon B(3,p)
    enfantsGENERATION <- 0
    for (i in 1:nbrINDIVIDU) {
      # Xi,n suit une loi binomiale B(3,p)
      nbENFANTS <- rbinom(1, 3, p)
      enfantsGENERATION <- enfantsGENERATION + nbENFANTS
    }
    nbrINDIVIDU <- enfantsGENERATION
    cat("Génération B(3,",p,")", generation, ":", nbrINDIVIDU, "\n")
  }

  return(nbrINDIVIDU)
}
#Je test ma fonction avec p= 0.5 et pour 5 générations
generation_n_exo2_DETAILLE(5, 0.5)

#Question 7 et question 8
generation_n_exo2 <- function(n, p) {
  nbrINDIVIDU <- 1

  for (generation in 1:n) {
    if (nbrINDIVIDU == 0) {
      return(0)
    }

    enfantsGENERATION <- 0
    for (i in 1:nbrINDIVIDU) {
      # Xi,n suit une loi binomiale B(3,p)
      nbENFANTS <- rbinom(1, 3, p)
      enfantsGENERATION <- enfantsGENERATION + nbENFANTS
    }
    nbrINDIVIDU <- enfantsGENERATION
  }

  return(nbrINDIVIDU)
}

# Voici ma fonction pour calculer la taille moyenne
taille_moyenne_n_exo2 <- function(n, p, nbSIMULATION = 1000) {
  # Vérification du paramètre p
  if (p <= 0 || p >= 1) {
    stop("p doit être strictement entre 0 et 1")
  }

  # Je stock les resultats de mes simulations
  resultats <- numeric(nbSIMULATION)

  # Boucle pour réaliser le nombre de simulation souhaité
  for(i in 1:nbSIMULATION) {
    resultats[i] <- generation_n_exo2(n, p)
  }

  # Je calcul ma moyenne des resultats des simulations
  moyenne <- mean(resultats)
  return(moyenne)
}
#je test avec une boucle pour voir l'évolution sur plusieurs générations
for(i in 0:10) {
  moy <- taille_moyenne_n_exo2(i, p = 1/3)
  cat("Génération", i, ": taille moyenne =", moy, "\n")
}

#Question 9
# Fonction pour calculer P[Zn=0] pour une valeur de p donnée
calculPROBA_extinction <- function(n, p, nbSIMULATIONS = 1000) {
  # Compte combien de fois on arrive à l'extinction
  nbEXTINCTION <- 0

  # Pour chaque simulation
  for(i in 1:nbSIMULATIONS) {
    # On utilise notre fonction generation_n_exo2
    populationFINALE <- generation_n_exo2(n, p)
    if(populationFINALE == 0) {
      nbEXTINCTION <- nbEXTINCTION + 1
    }
  }

  # Calcul de la probabilité
  proba <- nbEXTINCTION / nbSIMULATIONS
  return(proba)
}

# Vecteur des valeurs de p à tester
pVALEURS <- seq(0.1, 0.5, by = 0.05)

# Calcul pour différentes générations (par exemple jusqu'à n=5)
nMAX <- 5
resultats <- matrix(0, nrow = length(pVALEURS), ncol = nMAX + 1)
rownames(resultats) <- paste("p =", pVALEURS)
colnames(resultats) <- paste("n =", 0:nMAX)

# Remplissage de la matrice
for(i in 1:length(pVALEURS)) {
  for(n in 0:nMAX) {
    resultats[i, n+1] <- calculPROBA_extinction(n, pVALEURS[i])
  }
}

# Affichage des résultats
print(round(resultats, 3))

#Question 10
# Fonction pour simuler jusqu'à ce qu'on atteigne soit l'extinction soit une très grande génération
calculEXTINCTION_totale <- function(p, nMAX = 10, nbSIMULATIONS = 10) {
  nbEXTINCTION <- 0

  for(sim in 1:nbSIMULATIONS) {
    nbrINDIVIDU <- 1 # On commence avec 1 individu
    eteint <- FALSE

    # On simule jusqu'à extinction ou jusqu'à nMAX générations
    for(n in 1:nMAX) {
      if(nbrINDIVIDU == 0) {
        eteint <- TRUE
        break
      }

      enfantsGENERATION <- 0
      for(i in 1:nbrINDIVIDU) {
        nbENFANTS <- rbinom(1, 3, p)
        enfantsGENERATION <- enfantsGENERATION + nbENFANTS
      }
      nbrINDIVIDU <- enfantsGENERATION
    }

    if(eteint) {
      nbEXTINCTION <- nbEXTINCTION + 1
    }
  }

  return(nbEXTINCTION / nbSIMULATIONS)
}

# Vecteur des valeurs de p à tester
pVALEURS <- seq(0.1, 0.5, by = 0.05)

# Calcul pour chaque valeur de p
resultats <- numeric(length(pVALEURS))
for(i in 1:length(pVALEURS)) {
  resultats[i] <- calculEXTINCTION_totale(pVALEURS[i])
}

# Affichage des résultats
for(i in 1:length(pVALEURS)) {
  cat("p =", sprintf("%.2f", pVALEURS[i]), ": PExtinction :", sprintf("%.3f", resultats[i]), "\n")
}

#question 11
# Fonction pour simuler jusqu'à l'extinction et compter le nombre de générations
generationsAVANT_extinction <- function(lambda, nMAX = 1000, nbSIMULATIONS = 1000) {
  generationsTOTALES <- numeric(nbSIMULATIONS)

  for(sim in 1:nbSIMULATIONS) {
    nbrINDIVIDU <- 1 # On commence avec 1 individu
    eteint <- 0

    # On simule jusqu'à extinction ou nMAX générations
    while(nbrINDIVIDU > 0 && eteint < nMAX) {
      eteint <- eteint + 1
      enfantsGENERATION <- 0

      for(i in 1:nbrINDIVIDU) {
        # On utilise lambda comme probabilité
        nbENFANTS <- rbinom(1, 3, lambda)
        enfantsGENERATION <- enfantsGENERATION + nbENFANTS
      }

      nbrINDIVIDU <- enfantsGENERATION
    }

    # Si on n'a pas atteint nMAX, c'est qu'il y a eu extinction
    if(eteint < nMAX) {
      generationsTOTALES[sim] <- eteint
    } else {
      generationsTOTALES[sim] <- NA  # On marque les cas sans extinction
    }
  }

  # Calculer la moyenne en ignorant les cas sans extinction
  moyenne_GENERATIONS <- mean(generationsTOTALES, na.rm = TRUE)
  return(moyenne_GENERATIONS)
}

# Vecteur des valeurs de lambda à tester
p_VALEURS <- seq(0.1, 0.3, by = 0.05)

# Calcul pour chaque valeur de lambda
resultats <- numeric(length(p_VALEURS))

for(i in 1:length(p_VALEURS)) {
  resultats[i] <- generationsAVANT_extinction(p_VALEURS[i])
  cat("p =", sprintf("%.2f", p_VALEURS[i]),
      ": Nombre moyen de générations avant extinction :",
      sprintf("%.2f", resultats[i]), "\n")
}

#question12
generation_n_exo3_DETAILLEE <- function(n, lambda) {
  cat("Génération 0", ":", 1, "\n")

  # On commence avec 1 individu
  nbrINDIVIDU <- 1

  # Boucle pour chaque génération de 1 à n
  for (generation in 1:n) {
    # Si la population s'éteint, on retourne directement 0
    if (nbrINDIVIDU == 0) {
      cat("Extinction à la génération", generation, "\n")
      return(0)
    }

    # Calcul du nombre total d'enfants dans cette génération
    enfantsGENERATION <- 0
    for (i in 1:nbrINDIVIDU) {
      # Xi,n suit une loi de Poisson P(lambda)
      nbENFANTS <- rpois(1, lambda)
      enfantsGENERATION <- enfantsGENERATION + nbENFANTS
    }
    nbrINDIVIDU <- enfantsGENERATION
    cat("Génération", generation, ":", nbrINDIVIDU, "\n")
  }

  return(nbrINDIVIDU)
}

# Exemple de test pour n = 5 et lambda = 1.5
generation_n_exo3_DETAILLEE(5, lambda = 1.5)

#Question 13

# Fonction pour simuler l'état de la population à la génération n (loi de Poisson P(lambda))
generation_n_exo3 <- function(n, lambda) {
  nbrINDIVIDU <- 1  # Population initiale (1 individu)

  for (generation in 1:n) {
    if (nbrINDIVIDU == 0) {
      return(0)  # Si la population est éteinte, on retourne 0
    }

    enfantsGENERATION <- 0
    for (i in 1:nbrINDIVIDU) {
      # Nombre de descendants suit une loi de Poisson P(lambda)
      nbENFANTS <- rpois(1, lambda)
      enfantsGENERATION <- enfantsGENERATION + nbENFANTS
    }
    nbrINDIVIDU <- enfantsGENERATION
  }

  return(nbrINDIVIDU)  # Retourne la taille de la population à la génération n
}

# Fonction pour calculer la taille moyenne de la population à la génération n
taille_moyenne_n_exo3 <- function(n, lambda, nbSIMULATION = 1000) {
  # Stockage des résultats des simulations
  resultats <- numeric(nbSIMULATION)

  # Boucle pour réaliser le nombre de simulations souhaité
  for (i in 1:nbSIMULATION) {
    resultats[i] <- generation_n_exo3(n, lambda)
  }

  # Calcul de la moyenne des résultats
  moyenne <- mean(resultats)
  return(moyenne)
}

# Tester la fonction sur plusieurs générations avec lambda = 1.5
lambda <- 1  # Paramètre de la loi de Poisson
nbSIMULATION <- 1000  # Nombre de simulations

for (i in 1:10) {
  moy <- taille_moyenne_n_exo3(i, lambda, nbSIMULATION)
  cat("Génération", i, ": taille moyenne =", moy, "\n")
}

#question 14
# Fonction pour simuler l'état de la population à la génération n avec une loi de Poisson
generation_n_exo3 <- function(n, lambda) {
  nbrINDIVIDU <- 1  # Population initiale (1 individu)

  for (generation in 1:n) {
    if (nbrINDIVIDU == 0) {
      return(0)  # Si la population est éteinte, on retourne 0
    }

    enfantsGENERATION <- 0
    for (i in 1:nbrINDIVIDU) {
      # Nombre de descendants suit une loi de Poisson P(lambda)
      nbENFANTS <- rpois(1, lambda)
      enfantsGENERATION <- enfantsGENERATION + nbENFANTS
    }
    nbrINDIVIDU <- enfantsGENERATION
  }

  return(nbrINDIVIDU)  # Retourne la taille de la population à la génération n
}

#Question 15
# Fonction pour calculer P[Zn=0] pour une valeur de lambda donnée
calculPROBA_extinction <- function(n, lambda, nbSIMULATIONS = 1000) {
  # Compte combien de fois on arrive à l'extinction
  nbEXTINCTION <- 0

  # Pour chaque simulation
  for (i in 1:nbSIMULATIONS) {
    populationFINALE <- generation_n_exo3(n, lambda)
    if (populationFINALE == 0) {
      nbEXTINCTION <- nbEXTINCTION + 1
    }
  }

  # Calcul de la probabilité
  proba <- nbEXTINCTION / nbSIMULATIONS
  return(proba)
}

# Vecteur des valeurs de lambda à tester
lambda_VALEURS <- c(0.25, 0.5, 0.75, 1, 1.5, 2, 5)

# Calcul pour différentes générations (par exemple jusqu'à n=5)
nMAX <- 5
resultats <- matrix(0, nrow = length(lambda_VALEURS), ncol = nMAX + 1)
rownames(resultats) <- paste("λ =", lambda_VALEURS)
colnames(resultats) <- paste("n =", 0:nMAX)

# Remplissage de la matrice
nbSIMULATIONS <- 1000  # Nombre de simulations
for (i in 1:length(lambda_VALEURS)) {
  for (n in 0:nMAX) {
    resultats[i, n+1] <- calculPROBA_extinction(n, lambda_VALEURS[i], nbSIMULATIONS)
  }
}

# Affichage des résultats
print(round(resultats, 3))

#question 16)

# Fonction pour simuler jusqu'à extinction ou une très grande génération avec une loi de Poisson
calculEXTINCTION_totale <- function(lambda, nMAX = 10, nbSIMULATIONS = 10) {
  nbEXTINCTION <- 0  # Compteur des extinctions

  for (sim in 1:nbSIMULATIONS) {
    nbrINDIVIDU <- 1  # On commence avec 1 individu
    eteint <- FALSE

    # On simule jusqu'à extinction ou jusqu'à nMAX générations
    for (n in 1:nMAX) {
      if (nbrINDIVIDU == 0) {
        eteint <- TRUE
        break
      }

      # Calcul des descendants pour cette génération
      enfantsGENERATION <- 0
      for (i in 1:nbrINDIVIDU) {
        nbENFANTS <- rpois(1, lambda)  # Loi de Poisson avec paramètre lambda
        enfantsGENERATION <- enfantsGENERATION + nbENFANTS
      }
      nbrINDIVIDU <- enfantsGENERATION
    }

    # Vérifie si la population est éteinte
    if (eteint) {
      nbEXTINCTION <- nbEXTINCTION + 1
    }
  }

  # Retourne la proportion d'extinctions
  return(nbEXTINCTION / nbSIMULATIONS)
}

# Vecteur des valeurs de lambda à tester
lambda_VALEURS <- c(0.25, 0.5, 0.75, 1, 1.5, 2, 5)

# Calcul pour chaque valeur de lambda
resultats <- numeric(length(lambda_VALEURS))
nbSIMULATIONS <- 10  # Nombre de simulations
nMAX <- 10          # Nombre maximum de générations à simuler

for (i in 1:length(lambda_VALEURS)) {
  resultats[i] <- calculEXTINCTION_totale(lambda_VALEURS[i], nMAX, nbSIMULATIONS)
}

# Affichage des résultats
cat("Résultats :\n")
for (i in 1:length(lambda_VALEURS)) {
  cat("λ =", sprintf("%.2f", lambda_VALEURS[i]),
      ": PExtinction =", sprintf("%.3f", resultats[i]), "\n")
}

#question 17

# Fonction pour simuler jusqu'à l'extinction et compter le nombre de générations
generationsAVANT_extinction <- function(lambda, nMAX = 1000, nbSIMULATIONS = 1000) {
  generationsTOTALES <- numeric(nbSIMULATIONS)

  for (sim in 1:nbSIMULATIONS) {
    nbrINDIVIDU <- 1  # On commence avec 1 individu
    eteint <- 0

    # Simuler jusqu'à extinction ou jusqu'à nMAX générations
    while (nbrINDIVIDU > 0 && eteint < nMAX) {
      eteint <- eteint + 1
      enfantsGENERATION <- 0  # Initialisation à zéro pour chaque génération

      # Simuler les enfants pour chaque individu
      for (i in 1:nbrINDIVIDU) {
        nbENFANTS <- rpois(1, lambda)  # Loi de Poisson avec paramètre lambda
        enfantsGENERATION <- enfantsGENERATION + nbENFANTS
      }

      nbrINDIVIDU <- enfantsGENERATION  # Mise à jour du nombre d'individus
    }

    # Stocker le nombre de générations pour chaque simulation
    if (nbrINDIVIDU == 0) {
      generationsTOTALES[sim] <- eteint
    } else {
      generationsTOTALES[sim] <- NA  # Marque les cas où on n'atteint pas l'extinction
    }
  }

  # Calculer la moyenne en ignorant les cas sans extinction
  moyenne_GENERATIONS <- mean(generationsTOTALES, na.rm = TRUE)
  return(moyenne_GENERATIONS)
}

# Vecteur des valeurs de lambda à tester
lambda_VALEURS <- seq(0.1, 0.9, by = 0.1)

# Calcul pour chaque valeur de lambda
resultats <- numeric(length(lambda_VALEURS))

for (i in 1:length(lambda_VALEURS)) {
  resultats[i] <- generationsAVANT_extinction(lambda_VALEURS[i])
  cat("λ =", sprintf("%.1f", lambda_VALEURS[i]),
      ": Nombre moyen de générations avant extinction :",
      sprintf("%.2f", resultats[i]), "\n")
}