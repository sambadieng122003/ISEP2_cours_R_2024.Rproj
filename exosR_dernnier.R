                                                                                                                                                                    
#renommer, crer,labeliser les variables, recoder
recodage <- data.frame(
code = c(100, 108, 109, 115, 129, 135, 136, 137, 138, 139, 
                                143, 145, 147, 149, 251, 255, 568, 569, 570, 571, 
                                123, 126, 125, 562, 564),
        description = c("Kg", "Boite de tomate", "Bol", "Calebasse", "Paquet",
                                       "Sac (100 Kg)", "Sac (25 Kg)", "Sac (5 Kg)", "Sac (50 Kg)", "Sachet",
                                       "Tas", "Tine", "Unité", "Yorouba", "Quart-Yorouba", "Demi-Yorouba",
                                       "miche", ":Démi-miche", "Tiers de miche", "Quart de miche",
                                       "Cup gobelet", "Morceau", "Louche traditionnelle", "Cornet", "Cuillère à soupe")
                     )
                     
                    
cereales$Unite_cons <- recodage$description[match(cereales$Unite_cons, recodage$code)]


recodage_taille <- data.frame(
  code = c(0, 1, 2, 3, 4, 5, 6, 7),
  description = c("Taille unique", "Petit", "Moyen", "Grand", "Quart", "Demi", "Entier", "Très Petite")
)


cereales$Taille_cons <- recodage_taille$description[match(cereales$Taille_cons, recodage_taille$code)]                     
View(cereales)


recodage_unite2<- data.frame(
  code = c(100, 108, 109, 115, 129, 135, 136, 137, 138, 139, 
           143, 145, 147, 149, 251, 255, 568, 569, 570, 571, 
           123, 126, 125, 562, 564),
  description = c("Kg", "Boite de tomate", "Bol", "Calebasse", "Paquet",
                  "Sac (100 Kg)", "Sac (25 Kg)", "Sac (5 Kg)", "Sac (50 Kg)", "Sachet",
                  "Tas", "Tine", "Unité", "Yorouba", "Quart-Yorouba", "Demi-Yorouba",
                  "miche", ":Démi-miche", "Tiers de miche", "Quart de miche",
                  "Cup gobelet", "Morceau", "Louche traditionnelle", "Cornet", "Cuillère à soupe")
)


cereales$Unite_achat <- recodage_unite2$description[match(cereales$Unite_achat, recodage_unite2$code)]


recodage_taille2 <- data.frame(
  code = c(0, 1, 2, 3, 4, 5, 6, 7),
  description = c("Taille unique", "Petit", "Moyen", "Grand", "Quart", "Demi", "Entier", "Très Petite")
)


cereales$Taille_achat <- recodage_taille2$description[match(cereales$Taille_achat, recodage_taille2$code)]                     


library(readxl)
Table_de_conversion_phase_2 <- read_excel("C:/Users/samba/Desktop/EHCVM/Table de conversion phase 2.xlsx", 
                                          sheet = "nationale")
View(Table_de_conversion_phase_2)
colnames(Table_de_conversion_phase_2) <- c("cereales_id","produitNom")

#changer de type
Qtty_cons <- as.factor(Qtty_cons)

# Fusionner les données avec la base R cereales en utilisant la colonne produitID comme clé de fusion
cereales <- left_join(cereales, Table_de_conversion_phase_2, by = "cereales_id", all.x = TRUE)
