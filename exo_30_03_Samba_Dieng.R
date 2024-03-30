# 
source("exoR_27_03_Samba_Dieng.R")

glimpse(cereales)
## TABLE DE CONVERSION
library(readxl)
Table_de_conversion_phase_2 <- read_excel("~/Library/CloudStorage/OneDrive-Personal/ENSAE/ISEP 2024/Bases/Table de conversion phase 2.xlsx")
View(Table_de_conversion_phase_2)
names(Table_de_conversion_phase_2)
##
tableconves<- Table_de_conversion_phase_2 %>% 
  select(-c(8,9)) %>%
  mutate(produit=factor(produitID, 
                        levels=produitID,
                        labels=produitNom
                        ), 
         unite_cons=factor(uniteID,levels=uniteID,
                           labels=uniteNom),
         taille_cons=factor(tailleID,levels=tailleID,
                            labels=tailleNom))

glimpse(tableconves)

## Fusion des bases, pour obtenir le poids des alimements 
 ## consomommes; 

# cereales2 <- merge(cereales,tableconves,
#                    by=c("produit","unite_cons","taille_cons"),
#                    all.x = T) ## Ne marche pas ; 
# glimpse(cereales2)


glimpse(cereales)
glimpse(tableconves)
cereales3 <- merge(cereales,tableconves, 
                  by.x = c("cereales__id","Unite_cons","Taille_cons"),
                 by.y = c("produitID","uniteID","tailleID"),
                 all.x = T)
glimpse(cereales3)

## Poids 
library(data.table)
cereales3 <- data.table(cereales3)
setnames(cereales3,"poids","poids_cons")

## NA dans poids 
anyNA(cereales3$poids_cons)
sum(is.na(cereales3$poids_cons))

## Quantity conso en unite standard (kg)
cereales3 [, qtty_cons_kg:= poids_cons*Qtty_cons/1000]

is.numeric(cereales3$Qtty_cons)
is.numeric(cereales3$poids_cons)
# on convertit poids en numeric 
cereales3[,poids_cons:=as.numeric(poids_cons)]
is.numeric(cereales3$poids_cons)

#cereales3 %>% summarise(qtty_cons_kg) :: a regarder 

cereales3 [, qtty_cons_kg:= poids_cons*Qtty_cons/1000]
cereales3[,summary(qtty_cons_kg)]

## valeur anormale 

cereales3_anormal <- cereales3[qtty_cons_kg>1000]
glimpse(cereales3_anormal)

### HomeWork 
boxplot(cereales3$qtty_cons_kg)

#' calculer la quantite achete en kg; 

## 
glimpse(cereales3)
glimpse(tableconves)

cereales3 <- cereales3 [, Unite_achat := as.double(Unite_achat)]
cereales3 <- cereales3 [, Taille_achat := as.double(Taille_achat)]
cereales3 <- cereales3 [, cereales__id := as.double(cereales__id)]

cereales4 <- merge(cereales3, tableconves, 
                   by.x = c("cereales__id","Unite_achat","Taille_achat"),
                   by.y = c("produitID","uniteID","tailleID"),
                   all.x = T)

glimpse(cereales4)
cereales4 <- data.table(cereales4)
setnames(cereales4, "poids", "poids_achat")
cereales4 <- cereales4[,poids_achat:=as.numeric(poids_achat)]

summary(cereales4$poids_achat)
# if (!is.na.data.frame(cereales4$poids_achat)) {
#   summary(cereales4$poids_achat)
# }
summary(cereales4[!is.na(cereales4$poids_achat), "poids_achat"])

table(cereales4$tailleNom.y)
# table(cereales4$poids_achat) 
test <- cereales4[!is.na(cereales4$poids),"poids"]

cereales4 [, qtty_achat_kg := Qtty_achat*poids_achat/1000]
boxplot(cereales4$qtty_achat_kg)

#' calculer le prix unitaire ;
## Un prix unitaire poiur chaque combinaison (produit,unite, taille)
cereales4$pu <- cereales4$Value_achat/cereales4$Qtty_achat
cereales4[Unite_achat==100, summary(pu)]
cereales4[cereales__id<5 & Unite_achat==100, summary(pu)]
cereales4[cereales__id<5 & Unite_achat==100 & pu <2000, summary(pu)]
## Traiter les valeurs aberrantes ; 

#' Calculer les depenses de consommations ; 

### Extraire les Prix 


prixunitaire <- subset(cereales4, !is.na(pu), 
                       select =c("cereales__id", "Unite_achat", "Taille_achat", "pu") )
glimpse(prixunitaire)

## Traitement des pu aberrants ; 

idc <- unique(cereales4$cereales__id)

####EXO DE LA SCEANCE PRECEDENTE

##IMPUTATION DES PRIX ABBERANTS
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
