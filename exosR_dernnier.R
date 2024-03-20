library("tidyverse")
glimpse(cereales)

#--Renommer variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

##-Recodage de la variable cereales__id
cereales$cereales_id_recoded <- factor(cereales$cereales_id,
                                       levels = unname(attr(cereales$cereales__id,
                                                            "labels")),
                                       labels =names(attr(cereales$cereales__id,
                                                          "labels")))
edit(cereales$cereales__id_recoded)
View(cereales)

##-Recodage de la variable Unite_cons 
cereales$Unite_cons_recoded <- factor(cereales$Unite_cons,
                                      levels = unname(attr(cereales$Unite_cons,
                                                           "labels")),
                                      labels =names(attr(cereales$Unite_cons,
                                                         "labels")))
edit(cereales$Unite_cons_recoded)

##-Recodage de la variable Taille_cons
cereales$Taille_cons_recoded <- factor(cereales$Taille_cons,
                                       levels = unname(attr(cereales$Taille_cons,
                                                            "labels")),
                                       labels =names(attr(cereales$Taille_cons,
                                                          "labels")))
edit(cereales$Taille_cons_recoded)


library(readxl)
Table_de_conversion_phase_2 <- read_excel("C:/Users/samba/Desktop/EHCVM/Table de conversion phase 2.xlsx", 
                                          sheet = "nationale")
View(Table_de_conversion_phase_2)
colnames(Table_de_conversion_phase_2) <- c("cereales_id","produitNom")

#changer de type
Qtty_cons <- as.factor(Qtty_cons)

# Fusionner les données avec la base R cereales en utilisant la colonne produitID comme clé de fusion
library(readxl)
Table_de_conversion <- read_excel(
  "C:/Users/samba/Desktop/EHCVM/Table de conversion phase 2.xlsx")

Table_de_conversion$...8 <- NULL
Table_de_conversion$...9 <- NULL
View(Table_de_conversion)

colnames(Table_de_conversion)[1:6] <- c("cereales__id","Nom_Prod",
                                                "Unite_cons","Nom_Unite",
                                                "Taille_cons","Nom_Taille")

fusion <- merge(cereales, Table_de_conversion, 
               by = c("cereales__id", "Unite_cons", "Taille_cons"), all.x = TRUE)

