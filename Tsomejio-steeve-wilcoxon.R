# Saisie des données directement dans le terminal
cat("Entrez les valeurs du premier échantillon (séparées par des espaces), puis Entrée :\n")
x <- scan(what = numeric())

cat("Entrez les valeurs du second échantillon (séparées par des espaces), puis Entrée :\n")
y <- scan(what = numeric())

# Appel du test de Wilcoxon
resultat <- wilcox.test(x, y, paired = TRUE)

# Affichage des résultats
cat("\n--- Résultats du test de Wilcoxon ---\n")
print(resultat)

# Modulation des conclusions selon les hypothèses
if (resultat$p.value < 0.05) {
  cat("\nConclusion : On rejette H0. Il existe une différence significative entre les deux échantillons.\n")
} else {
  cat("\nConclusion : On ne rejette pas H0. Aucune différence significative détectée.\n")
}
