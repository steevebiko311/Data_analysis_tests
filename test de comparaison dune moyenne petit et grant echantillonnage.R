test_z <- function() {
  
  # saisie utilisateur
  xbar <- as.numeric(readline("Entrer la moyenne observée (xbar) : "))
  μ0   <- as.numeric(readline("Entrer la moyenne théorique (μ0) : "))
  s    <- as.numeric(readline("Entrer l'écart-type (s) : "))
  n    <- as.numeric(readline("Entrer la taille de l'échantillon (n) : "))
  α    <- as.numeric(readline("Entrer le niveau de risque (α, ex: 0.05) : "))
  type <- readline("Entrer le type de test (bilateral / unilateral_gauche / unilateral_droite) : ")
  
  # choix de la loi
  if(n >= 30){
    loi <- "loi normale"
    Val_Obs <- (xbar - μ0) / (s / sqrt(n))
  } else {
    loi <- "loi de Student"
    Val_Obs <- (xbar - μ0) / (s / sqrt(n))
    ddl <- n - 1
  }
  
  # calcul des points critiques
  if (type == "bilateral") {
    
    if(n >= 30){
      pt_crit <- qnorm(1 - α/2)
    } else {
      pt_crit <- qt(1 - α/2, ddl)
    }
    
    borne_inf <- -pt_crit
    borne_sup <- pt_crit
    
  } else if(type == "unilateral_gauche") {
    
    if(n >= 30){
      pt_crit <- qnorm(α)
    } else {
      pt_crit <- qt(α, ddl)
    }
    
  } else if(type == "unilateral_droite"){
    
    if(n >= 30){
      pt_crit <- qnorm(1 - α)
    } else {
      pt_crit <- qt(1 - α, ddl)
    }
  }
  
  cat("\nReformulation des hypothèses :\n H0: μ = μ0 \n H1: μ ≠ μ0\n")
  cat("Loi utilisée =", loi, "\n")
  cat("Valeur du test =", Val_Obs, "\n")
  
  if(type == "bilateral"){
    cat("Point critique =", pt_crit, "\n")
    cat("Zone de non-rejet = [", borne_inf, ";", borne_sup, "]\n\n")
    
  } else if(type == "unilateral_droite") {
    cat("Point critique =", pt_crit, "\n")
    cat("Zone de non-rejet = ]-∞ ;", pt_crit, "]\n\n")
    
  } else if(type == "unilateral_gauche") {
    cat("Point critique =", pt_crit, "\n")
    cat("Zone de non-rejet = [", pt_crit, "; +∞[\n\n")
  }
  
  # décision
  if (type == "bilateral") {
    
    if (Val_Obs >= borne_inf && Val_Obs <= borne_sup) {
      decision <- "ne pas rejeter H0"
    } else {
      decision <- "rejeter H0"
    }
    
  } else if(type == "unilateral_gauche") {
    
    if (Val_Obs >= pt_crit) {
      decision <- "ne pas rejeter H0"
    } else {
      decision <- "rejeter H0"
    }
    
  } else if(type == "unilateral_droite"){
    
    if (Val_Obs <= pt_crit) {
      decision <- "ne pas rejeter H0"
    } else {
      decision <- "rejeter H0"
    }
  }
  
  conclusion <- paste(
    "Au risque de se tromper de", α*100, "%,", decision
  )
  
  cat("Décision :", decision, "\n")
  cat("Conclusion :", conclusion, "\n")
}

# appel de la fonction
test_z()

