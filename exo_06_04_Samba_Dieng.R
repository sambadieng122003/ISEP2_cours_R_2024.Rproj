# 
source("exoR_27_03_Samba_Dieng.R")




##IMPUTATION DES PRIX ABBERANTS PAR LA METHODE DE TUCKEY
library(dplyr)
cereales4 <- cereales4 %>%
  group_by(cereales__id) %>%
  mutate(pu = ifelse(!is.na(pu) & pu > quantile(pu, 0.75, na.rm = TRUE), 
                     quantile(pu, 0.75, na.rm = TRUE), pu))
##Prendre la moyenne pour chaque combinaison (p,u,t)
prix_gr <- prixunitaire %>%
  group_by(pu, Unite_achat, Taille_achat) %>%
  summarise(
    prix = mean(pu, na.rm = TRUE)
  )

##Ramener la sous-base dans la base initiale(fusion)
cereales4 <- cereales4 %>%
  left_join(prix_gr, by = c("pu", "Unite_achat", "Taille_achat"))

##Calculer les dépenses de consommation

cereales4$dépenses_cons <- cereales4$prix * cereales4$Qtty_achat
 
#' 1:: evaluer le taux de matching : n(Pc,Uc,Tc) aynt un prix P sur le
#' le nombre total de combinaison n(Pc,Uc,Tc); 
nbre_total_combi <- nrow(cereales4)
