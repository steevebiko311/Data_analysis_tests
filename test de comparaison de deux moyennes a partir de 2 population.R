#test de comparaison de deux moyennes a partir de 2 population

test_b <- function(X1bar, X2bar, n1, n2, δ1, δ2, α , type = "bilateral"){
  
  if(n1>=30 && n2>= 30){
    loi <- "loi normale"
    Val_Obs <- (X1bar-X2bar)/sqrt((δ1/n1)+(δ2/n2))
  }else{
    loi <- "loi de student"
    Sp <- sqrt(((n1-1)*δ1^2 + (n2-1)*δ2^2) / (n1 + n2 - 2))
    stat <- (X1bar - X2bar) / (Sp * sqrt(1/n1 + 1/n2))
    ddl <- n1 + n2 - 2
  }
  
  
  
  if (type == "bilateral") {
    test_b <- function(){
      
      # saisie utilisateur
      X1bar <- as.numeric(readline("Entrer la moyenne X1bar : "))
      X2bar <- as.numeric(readline("Entrer la moyenne X2bar : "))
      n1    <- as.numeric(readline("Entrer la taille n1 : "))
      n2    <- as.numeric(readline("Entrer la taille n2 : "))
      δ1    <- as.numeric(readline("Entrer l'écart-type δ1 : "))
      δ2    <- as.numeric(readline("Entrer l'écart-type δ2 : "))
      α     <- as.numeric(readline("Entrer le niveau de risque α (ex: 0.05) : "))
      type  <- readline("Entrer le type (bilateral / unilateral_gauche / unilateral_droite) : ")
      
      if(n1>=30 && n2>= 30){
        loi <- "loi normale"
        Val_Obs <- (X1bar-X2bar)/sqrt((δ1/n1)+(δ2/n2))
      }else{
        loi <- "loi de student"
        Sp <- sqrt(((n1-1)*δ1^2 + (n2-1)*δ2^2) / (n1 + n2 - 2))
        Val_Obs <- (X1bar - X2bar) / (Sp * sqrt(1/n1 + 1/n2))
        ddl <- n1 + n2 - 2
      }
      
      if (type == "bilateral") {
        
        if(n1>=30 && n2>= 30){
          pt_crit <- qnorm(1 - α/2)
        } else {
          pt_crit <- qt(1 - α/2, ddl)
        }
        
        borne_inf <- -pt_crit
        borne_sup <- pt_crit
        
      } else if(type == "unilateral_gauche") {
        
        if(n1>=30 && n2>= 30){
          pt_crit <- qnorm(α)
        } else {
          pt_crit <- qt(α, ddl)
        }
        
      } else if(type == "unilateral_droite"){
        
        if(n1>=30 && n2>= 30){
          pt_crit <- qnorm(1 - α)
        } else {
          pt_crit <- qt(1 - α, ddl)
        }
      }
      
      cat("\nReformulation des hypothèses :\n H0: μ1 = μ2 \n H1: μ1 ≠ μ2\n")
      
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
      
      cat("Décision :", decision, "\n")
      cat("Conclusion :", conclusion, "\n")
    }
    
    # appel
    test_b()
    if(n1>=30 && n2>= 30){
      pt_crit <- qnorm(1 - α/2)
    } else {
      pt_crit <- qt(1 - α/2, ddl)
    }
    
    borne_inf <- -pt_crit
    borne_sup <- pt_crit
    
  } else if(type == "unilateral_gauche") {
    
    if(n1>=30 && n2>= 30){
      pt_crit <- qnorm(α)
    } else {
      pt_crit <- qt(α, ddl)
    }
    
  } else if(type == "unilateral_droite"){
    
    if(n1>=30 && n2>= 30){
      pt_crit <- qnorm(1 - α)
    } else {
      pt_crit <- qt(1 - α, ddl)
    }
  }
  
  cat("reformulation de l'hypothese,  :\n H0: μ0 = μ1 \n H1: μ0 ≠ μ0  \n")
  
  
  cat("Loi utilisée =", loi, "\n")
  cat("Valeur du test =", Val_Obs, "\n")
  cat("point crituque " , pt_crit, "\n")
  
  if(type == "bilateral"){
    cat("Zone de non-rejet = [", borne_inf, ";", borne_sup, "]\n\n")
  }else if(type == "unilateral_droite") {
    cat("Zone de non-rejet = ]-∞ ;", pt_crit, "]\n\n")
  } else if(type == "unilateral_gauche") {
    cat("Zone de non-rejet = [", pt_crit, "; +∞[\n\n")
  }
  
  
  # décision
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

test_b(
  X1bar = 10,
  X2bar = 12,
  n1 = 35,
  n2 = 40,
  α = 0.02,
  δ1 = 2,
  δ2 = 1.5,
  type = "bilateral"
)


