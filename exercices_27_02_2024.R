
#1-CREER UNE BASE DE DONNEES fictive ayant au moins 5 variables avec
#les types numériques,character,facteut...;)
base <- data.frame(
  PRENOM = c("PAN", "NIASS", "FAYE", "KD", "SENE"),
  MENTION = factor(c("TRES-BIEN","Passable", "Bien", "Tres-bien", "Recale")),
  TAILLE = c(1.83, 2.0, 1.32, 2.1, 1.75),
  REGION = c("Thies", "Kaolack", "Diourbel", "Dakar", "Fatick"),
  DISTANCE_ECOLE_MAISON = c(1, 3, 2, 8, 0.03)
)
tam <- c("Tamsir","Passable",1,"MATAM",2)
base <- rbind(base,tam)
View(base)
#2-creer une matrice à partir de vos données; renommer
#les lignes et les colonnes
matrice <- as.matrix(base)
colnames(matrice) <- c("ind1","ind2","ind3","ind4","ind5")
row.names(matrice)<- c("Etudiant1","Etudiant2","Etudiant3","Etudiant4","Etudiant5")
matrice

#3- Faites des statistiques descriptives: mean, max, quartiles
summary(base)
#4- Faites des graphiques
library(ggplot2)
ggplot(base, aes(x = TAILLE, y = DISTANCE_ECOLE_MAISON)) +
  geom_point(color = "darkgreen") +
  labs(title = "Relation entre la taille et la distance école-maison", x = "Taille", y = "Distance école-maison")

#
ggplot(base, aes(x = TAILLE)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution de la taille des étudiants", x = "Taille", y = "Fréquence")


#
mentions <- table(base$MENTION)
couleurs <- c("skyblue", "salmon", "lightgreen", "orange", "lightcoral")
pie(mentions, labels = names(mentions), col = couleurs, main = "Répartition des mentions")


#5 OPTIMISATION
lagrangien <- function(x, y, lambda) {
  return(x^2 + y^2 + lambda * (1 - x - y))
}

# Dérivées partielles du Lagrangien par rapport à x, y et lambda
dL_dx <- function(x, y, lambda) {
  return(2 * x - lambda)
}

dL_dy <- function(x, y, lambda) {
  return(2 * y - lambda)
}

dL_dlambda <- function(x, y) {
  return(1 - x - y)
}

# Algorithme itératif pour trouver les valeurs de x, y et lambda
tolerance <- 1e-6
x <- 0.5  # Initial guess for x
y <- 0.5  # Initial guess for y
lambda <- 0  # Initial guess for lambda
while (TRUE) {
  # Mise à jour de x, y et lambda
  x_new <- x - dL_dx(x, y, lambda)
  y_new <- y - dL_dy(x, y, lambda)
  lambda_new <- lambda - dL_dlambda(x, y)
  
  # Condition d'arrêt
  if (abs(x_new - x) < tolerance && abs(y_new - y) < tolerance && abs(lambda_new - lambda) < tolerance) {
    break
  }
  
  # Mise à jour des valeurs de x, y et lambda
  x <- x_new
  y <- y_new
  lambda <- lambda_new
}

# Affichage de la solution optimale
print(paste("La valeur optimale de x est:", x))
print(paste("La valeur optimale de y est:", y))
print(paste("La valeur optimale de lambda est:", lambda))

writexl::write_xlsx(base,"baseisep2.xlsx")

#EXO1
#Complééter la base en ajoutant des variables et des observations
#au minimum 300 observations et 15 variables(utiliser les fonctions de R pour créer ces variables)
#Ecrire un code import/export; renommer; faire des stats des sur les variablessur notre base
#exporter puis réimporter la base
##EXO2
#implémenter manuellement le test de khi deux manuellement(avec notre base calculer les effectifs marginaux)

