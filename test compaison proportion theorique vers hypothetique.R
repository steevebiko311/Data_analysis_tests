test_A <- function(){
  
  # saisie utilisateur
  p0    <- as.numeric(readline("Entrer la proportion théorique p0 : "))
  pChap <- as.numeric(readline("Entrer la proportion observée pChap : "))
  n     <- as.numeric(readline("Entrer la taille de l'échantillon n : "))
  α     <- as.numeric(readline("Entrer le niveau de risque α (ex: 0.05) : "))
  type  <- readline("Entrer le type (bilateral / unilateral_gauche / unilateral_droite) : ")
  
  # condition d'application
  if(n*p0 >= 5 && n*(1-p0) >= 5){
    loi <- "loi normale"
    Val_Obs <- (pChap - p0) / sqrt(p0*(1-p0)/n)
  } else {
    stop("Conditions non respectees")
  }
  
  # valeur pt_critique
  if (type == "bilateral") {
    
    pt_crit <- qnorm(1 - α/2)
    borne_inf <- -pt_crit
    borne_sup <- pt_crit
    
  } else if(type == "unilateral_gauche") {
    
    pt_crit <- qnorm(α)
    
  } else if(type == "unilateral_droite"){
    
    pt_crit <- qnorm(1 - α)
  }
  
  # affichage
  cat("\nReformulation de l'hypothese\n")
  cat("H0: p =", p0, "\n")
  cat("H1: p ≠", p0, "\n\n")
  
  cat("Loi utilisée =", loi, "\n")
  cat("Valeur du test =", Val_Obs, "\n")
  cat("Point critique =", pt_crit, "\n")
  
  if(type == "bilateral"){
    cat("Zone de non-rejet = [", borne_inf, ";", borne_sup, "]\n\n")
    
  }else if(type == "unilateral_droite") {
    cat("Zone de non-rejet = ]-∞ ;", pt_crit, "]\n\n")
    
  } else if(type == "unilateral_gauche") {
    cat("Zone de non-rejet = [", pt_crit, "; +∞[\n\n")
  }
  
  # décision (inchangée)
  if (type == "bilateral") {
    
    if (Val_Obs >= borne_inf && Val_Obs <= borne_sup) {
      decision <- "ne pas rejeter H0"
      conclusion <- paste(
        "Au risque de se tromper de", α*100, "%,",
        "l'hypothèse nulle H0 est acceptee."
      )
    } else {
      decision <- "rejeter H0"
      conclusion <- paste(
        "Au risque de se tromper de", α*100, "%,",
        "l'hypothèse nulle H0 est rejetee."
      )
    }
  }
  
  # affichage final
  cat("Décision :", decision, "\n")
  cat("Conclusion :", conclusion, "\n")
}

# appel
test_A()