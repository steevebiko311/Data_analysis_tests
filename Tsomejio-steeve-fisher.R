# Script interactif pour le test de Fisher

# 1. Saisie des données par l'utilisateur
cat("Entrez le nombre de Succès pour le groupe A : ")
a_succes <- as.numeric(readline())

cat("Entrez le nombre d'Échecs pour le groupe A : ")
a_echec <- as.numeric(readline())

cat("Entrez le nombre de Succès pour le groupe B : ")
b_succes <- as.numeric(readline())

cat("Entrez le nombre d'Échecs pour le groupe B : ")
b_echec <- as.numeric(readline())

# 2. Création de la table de contingence
donnees <- matrix(c(a_succes, a_echec, b_succes, b_echec),
                  nrow = 2,
                  byrow = TRUE)

dimnames(donnees) <- list(
  Groupe = c("A", "B"),
  Issue  = c("Succès", "Échec")
)

cat("\nTable de contingence :\n")
print(donnees)

# 3. Test de Fisher
resultat <- fisher.test(donnees)

cat("\nRésultats du test de Fisher :\n")
print(resultat)

# 4. (Optionnel) Visualisation avec ggplot2
library(ggplot2)
df <- as.data.frame(as.table(donnees))

ggplot(df, aes(x = Groupe, y = Freq, fill = Issue)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Test de Fisher - Données saisies par l'utilisateur",
       subtitle = paste("p-value =", signif(resultat$p.value, 3),
                        "| Odds ratio =", signif(resultat$estimate, 3)))
