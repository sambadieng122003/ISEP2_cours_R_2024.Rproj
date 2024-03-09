#EXO1
#Complééter la base en ajoutant des variables et des observations
#au minimum 300 observations et 15 variables(utiliser les fonctions de R pour créer ces variables)var1 <- rnorm(300) # variable numérique générée aléatoirement selon une distribution normale
prenoms <- c("Samba", "PAN", "Ahmadou", "KD", "AMETH")
noms <- c("Dieng","Niang","Niass","Diakhaté","Faye")
villes <- c("Dakar", "Thiès", "Saint-Louis", "Kaolack", "Diourbel")
emplois <- c("Economiste", "data scientist", "Médecin", "Actuaire", "Professeur")
passe_temps <- c("Lecture", "Sport", "Manger", "Dormir", "Cuisine")
age <- sample(25:65, 300, replace = TRUE)
taille <- sample(150:200, 300, replace = TRUE)
annee_experience <- sample(0:20, 300, replace = TRUE)
nombre_enfants <- sample(0:5, 300, replace = TRUE)
salaire <- runif(300, min = 20000, max = 100000)
note <- runif(300, min = 0, max = 20)
satisfaction <- runif(300, min = 1, max = 10)

# Générer les données pour les variables catégorielles
niveau_education <- factor(sample(c("Bfem", "Bac", "Licence", "Master", "Doctorat"), 300, replace = TRUE))
genre <- factor(sample(c("Homme", "Femme"), 300, replace = TRUE))
groupe_age <- factor(sample(c("18-25", "26-35", "36-45", "46-55", "56+"), 300, replace = TRUE))
situation_familiale <- factor(sample(c("Célibataire", "Marié(e)", "Divorcé(e)"), 300, replace = TRUE))

# Créer le dataframe
data <- data.frame(Prenom = sample(prenoms, 300, replace = TRUE),
                   Nom = sample(noms, 300, replace = TRUE),
                   age,
                   taille,
                   Ville = sample(villes, 300, replace = TRUE),
                   Emploi = sample(emplois, 300, replace = TRUE),
                   Passe_Temps = sample(passe_temps, 300, replace = TRUE),
                  annee_experience,
                   nombre_enfants,
                   salaire,
                   note,
                   satisfaction,
                   niveau_education,
                   genre,
                  groupe_age,
                   situation_familiale)

# Afficher les premières lignes du dataframe
head(data)


#Ecrire un code qui exporte
writexl::write_xlsx(data,"devoir_R.xlsx")

#Ecrire un code qui importe
install.packages("readxl")
library(readxl)


chemin_fichier <- "C:/Users/samba/Desktop/ISEP2_COURS_R/devoir_R.xlsx"
donnees_excel <- read_excel(chemin_fichier)
head(donnees_excel)
View(donnees_excel)

#Statistiques descriptives 

summary(donnees_excel)
##EXO2
#implémenter manuellement le test de khi deux manuellement(avec notre base calculer les effectifs marginaux)

tableau_contingence <- table(data$groupe_age, data$situation_familiale)
print(tableau_contingence)
# Calculer les fréquences attendues
total <- sum(tableau_contingence)
effectifs_théoriques<- outer(rowSums(tableau_contingence), colSums(tableau_contingence)) / total

# Calculer la statistique du test du khi-deux
khi_2 <- sum((tableau_contingence - effectifs_théoriques)^2 / effectifs_théoriques)



# Afficher la statistique du test du khi-deux
print(paste("Statistique du test du khi-deux:",khi_2 ))




