#IMPORTATION BASE EXCEL
library(readxl)
Table_de_conversion<- read_excel(
  "C:/Users/samba/Desktop/EHCVM/Table de conversion phase 2.xlsx")

Table_de_conversion$...8 <- NULL
Table_de_conversion$...9 <- NULL

#FUSIONS DES BASES
colnames(Table_de_conversion) <- c("cereales__id","Nom_Prod",
                                   "Unite_cons","Nom_Unite",
                                   "Taille_cons","Nom_Taille","poids_cons")
cereales <- merge(cereales, Table_de_conversion, 
                  by = c("cereales__id", "Unite_cons", "Taille_cons"), all.x = TRUE)

colnames(Table_de_conversion) <- c("cereales__id","Nom_Prod",
                                   "Unite_achat","Nom_Unite",
                                   "Taille_achat","Nom_Taille","poids_achat")

cereales3 <- merge(cereales,Table_de_conversion, 
                  by = c("cereales__id", "Unite_achat", "Taille_achat"), all.x = TRUE)


#QUANTITE CONS
cereales3$poids_cons <- as.numeric(cereales3$poids_cons)
cereales3$quantite_cons_kg <- cereales3$Qtty_cons * cereales3$poids_cons/1000

# Calculer la quantite achetee en kg
cereales3$poids_achat <- as.numeric(cereales3$poids_achat)
cereales3$quantite_achetee_kg <- cereales3$Qtty_achat * cereales3$poids_achat/1000

#le prix unitaire
cereales3$prix_unitaire <- cereales3$Value_achat/cereales3$quantite_achetee_kg


# calculer les depenses de consommation
cereales3$depenses_cons_menages <- cereales3$quantite_cons_kg*cereales3$prix_unitaire


#Valeurs abberantes:: détection et corections
visualisation <- boxplot(cereales3$quantite_cons_kg, 
                         main = "Boxplot quantite consommé", # Titre du graphique
                         ylab = "Dépenses de consommation", # Nom de l'axe des ordonnées
                         col = "skyblue", 
                         border = "blue", 
                         horizontal = TRUE)
Q1 <- quantile(cereales3$quantite_cons_kg, 0.25)
Q3 <- quantile(cereales3$quantite_cons_kg, 0.75)
EQR <- Q3 - Q1

sup <- Q3 + 1.5 * EQR
inf <- Q1 - 1.5 * EQR

valeurs_aberrantes_inf <- cereales3$quantite_cons_kg < inf
valeurs_aberrantes_sup <- cereales3$quantite_cons_kg > sup

# Remplacer les valeurs aberrantes par la moyenne des données non aberrantes
moyenne_sans_aberrantes <- mean(cereales3$quantite_cons_kg[!valeurs_aberrantes_inf & !valeurs_aberrantes_sup])
cereales3$quantite_cons_kg[valeurs_aberrantes_inf | valeurs_aberrantes_sup] <- moyenne_sans_aberrantes



