test_correlation <- function(){
  
  # saisie utilisateur
  X_input <- readline("Entrer les valeurs de X séparées par des virgules (ex: 1,2,3,4) : ")
  Y_input <- readline("Entrer les valeurs de Y séparées par des virgules (ex: 2,4,6,8) : ")
  
  alpha <- as.numeric(readline("Entrer le niveau de risque alpha (ex: 0.05) : "))
  type  <- readline("Entrer le type (bilateral / unilateral_gauche / unilateral_droite) : ")
  
  # transformation en vecteurs numériques
  X <- as.numeric(strsplit(X_input, ",")[[1]])
  Y <- as.numeric(strsplit(Y_input, ",")[[1]])
  
  # taille
  n <- length(X)
  
  if(length(X) != length(Y)){
    stop("Les deux échantillons doivent avoir la même taille")
  }
  
  # moyennes
  xbar <- mean(X)
  ybar <- mean(Y)
  
  # covariance
  cov_xy <- sum((X - xbar)*(Y - ybar)) / n
  
  # écarts-types
  sx <- sd(X)
  sy <- sd(Y)
  
  r <- cov_xy / (sx * sy)
  
  # statistique de test (Student)
  t_obs <- (r * sqrt(n - 2)) / sqrt(1 - r^2)
  
  ddl <- n - 2
  
  if(type == "bilateral"){
    t_crit <- qt(1 - alpha/2, ddl)
    borne_inf <- -t_crit
    borne_sup <- t_crit
  } else if(type == "unilateral_gauche"){
    t_crit <- qt(alpha, ddl)
  } else if(type == "unilateral_droite"){
    t_crit <- qt(1 - alpha, ddl)
  }
  
  cat("\nTest de corrélation de Pearson\n\n")
  
  cat("H0 : ρ = 0 (pas de corrélation linéaire)\n")
  
  if(type == "bilateral"){
    cat("H1 : ρ ≠ 0 (corrélation linéaire)\n\n")
  } else if(type == "unilateral_droite"){
    cat("H1 : ρ > 0 (corrélation positive)\n\n")
  } else if(type == "unilateral_gauche"){
    cat("H1 : ρ < 0 (corrélation négative)\n\n")
  }
  
  # affichage
  cat("Coefficient de corrélation r =", r, "\n")
  cat("Valeur du test t =", t_obs, "\n")
  cat("Point critique =", t_crit, "\n")
  cat("Degrés de liberté =", ddl, "\n")
  
  if(type == "bilateral"){
    cat("Zone de non-rejet = [", borne_inf, ";", borne_sup, "]\n\n")
  }
  
  # décision (inchangée)
  if(type == "bilateral"){
    
    if(t_obs >= borne_inf && t_obs <= borne_sup){
      decision <- "ne pas rejeter H0"
      conclusion <- paste(
        "Au risque de", alpha*100, "%,",
        "il n'existe pas de corrélation linéaire entre X et Y."
      )
    } else {
      decision <- "rejeter H0"
      conclusion <- paste(
        "Au risque de", alpha*100, "%,",
        "il existe une corrélation linéaire entre X et Y."
      )
    }
    
  } else if(type == "unilateral_droite"){
    
    if(t_obs <= t_crit){
      decision <- "ne pas rejeter H0"
      conclusion <- paste(
        "Au risque de", alpha*100, "%,",
        "pas de corrélation positive significative."
      )
    } else {
      decision <- "rejeter H0"
      conclusion <- paste(
        "Au risque de", alpha*100, "%,",
        "corrélation positive significative."
      )
    }
    
  } else if(type == "unilateral_gauche"){
    
    if(t_obs >= t_crit){
      decision <- "ne pas rejeter H0"
      conclusion <- paste(
        "Au risque de", alpha*100, "%,",
        "pas de corrélation négative significative."
      )
    } else {
      decision <- "rejeter H0"
      conclusion <- paste(
        "Au risque de", alpha*100, "%,",
        "corrélation négative significative."
      )
    }
  }
  
  cat("Décision :", decision, "\n")
  cat("Conclusion :", conclusion, "\n")
}

# appel
test_correlation()