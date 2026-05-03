test_mann_whitney <- function(){
  
  # saisie utilisateur
  X1_input <- readline("Entrer les valeurs de X1 séparées par des virgules (ex: 10,12,16) : ")
  X2_input <- readline("Entrer les valeurs de X2 séparées par des virgules (ex: 8,9,11) : ")
  
  alpha <- as.numeric(readline("Entrer le niveau de risque alpha (ex: 0.05) : "))
  type  <- readline("Entrer le type (bilateral / unilateral_gauche / unilateral_droite) : ")
  
  # transformation en vecteurs numériques
  X1 <- as.numeric(strsplit(X1_input, ",")[[1]])
  X2 <- as.numeric(strsplit(X2_input, ",")[[1]])
  
  # tailles
  n1 <- length(X1)
  n2 <- length(X2)
  
  # concaténer les données
  X <- c(X1, X2)
  
  # calcul des rangs
  rangs <- rank(X)
  
  # séparer les rangs
  R1 <- rangs[1:n1]
  R2 <- rangs[(n1+1):(n1+n2)]
  
  # sommes des rangs
  W1 <- sum(R1)
  W2 <- sum(R2)
  
  # calcul des U
  U1 <- W1 - n1*(n1+1)/2
  U2 <- W2 - n2*(n2+1)/2
  
  U <- min(U1, U2)
  
  # approximation normale
  mu_U <- n1*n2/2
  sigma_U <- sqrt(n1*n2*(n1+n2+1)/12)
  
  Z <- (U - mu_U) / sigma_U
  
  # valeur critique
  if(type == "bilateral"){
    z_crit <- qnorm(1 - alpha/2)
    borne_inf <- -z_crit
    borne_sup <- z_crit
  } else if(type == "unilateral_gauche"){
    z_crit <- qnorm(alpha)
  } else if(type == "unilateral_droite"){
    z_crit <- qnorm(1 - alpha)
  }
  
  # affichage
  cat("\nTest de Mann-Whitney\n")
  cat("H0: les deux populations ont la même distribution\n")
  cat("H1: les distributions sont différentes\n\n")
  
  cat("n1 =", n1, " n2 =", n2, "\n")
  cat("Somme des rangs W1 =", W1, "\n")
  cat("Somme des rangs W2 =", W2, "\n")
  
  cat("U1 =", U1, " U2 =", U2, "\n")
  cat("Statistique U =", U, "\n")
  cat("Statistique Z =", Z, "\n\n")
  
  if(type == "bilateral"){
    cat("Zone de non-rejet = [", borne_inf, ";", borne_sup, "]\n\n")
  }
  
  # décision (inchangée)
  if(type == "bilateral"){
    
    if(Z >= borne_inf && Z <= borne_sup){
      decision <- "ne pas rejeter H0"
      conclusion <- paste(
        "Au risque de se tromper de", alpha*100, "%,",
        "les deux populations sont similaires."
      )
    } else {
      decision <- "rejeter H0"
      conclusion <- paste(
        "Au risque de se tromper de", alpha*100, "%,",
        "les deux populations sont différentes."
      )
    }
  }
  
  cat("Décision :", decision, "\n")
  cat("Conclusion :", conclusion, "\n")
}

# appel
test_mann_whitney()