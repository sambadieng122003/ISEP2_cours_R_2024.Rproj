#'Coulibaly Khadidiatou, Samba Dieng 
#'ISEP2
#'
#'      Affecter des poids aux dépenses de consommation qui n'en ont pas en utilisant 
#'      UNE METHODE D'IMPUTATION DES VALEURS MANQUANTES


# Importing libraries
library("tidyverse")
library("haven")
library("questionr")
library("readxl")
library("data.table")
# Importing dataframe------------------------------------------------------------------------------
cereales <-read_dta("cereales.dta")

## Renaming the variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

table_conv <- read_xlsx("Table de conversion phase 2.xlsx", sheet = "nationale")
cereales <- merge(cereales,table_conv, 
                  by.x = c("cereales__id","Unite_cons","Taille_cons"),
                  by.y = c("produitID","uniteID","tailleID"),
                  all.x = T)

cereales <- cereales[!(is.na(cereales$Qtty_cons)),] # On enlève les NA


setnames(cereales,"poids", "poids_cons")

cereales <- data.table(cereales)  # Converting it into a data table

# Quantités totales consommées
cereales[, Qtty_cons_tt:= Qtty_cons*as.numeric(poids_cons)/1000] 


# Calcul de la quantité consommée, et du prix unitaire; 

# We merge using cereales__id, Unite_achat et Taille_achat

cereales <- left_join(cereales,table_conv,
                      by =c("cereales__id"= "produitID",
                            "Unite_achat"="uniteID",
                            "Taille_achat"="tailleID"))

# Renaming
setnames(cereales,"poids", "poids_achat")

#Quantité totale achetée et prix unitaire
cereales[, Qtty_achat_tt:= Qtty_achat*as.numeric(poids_achat)/1000] 
cereales[, Price_p_u_t:= Value_achat/Qtty_achat]

#Ecrire un code qui traite les valeurs  aberrantes de prix----------------------------------------------

#' On remplace les valeurs sup au 3e quartile par ce quartile

j <- 1
for(i in cereales$cereales__id){
  
  if ( !is.na(cereales$Price_p_u_t[j]) & 
       cereales$Price_p_u_t[j] > quantile(cereales$Price_p_u_t[cereales$cereales__id ==i]
                                          , 0.75, na.rm = T))
  {
    cereales$Price_p_u_t[j] <- quantile(cereales$Price_p_u_t[cereales$cereales__id ==i]
                                        , 0.75, na.rm = T)
  }
  j= j+1
}


#Decider de prendre la moyenne ou la mediane de pu, pour chaque combinaison (p,u,t)---------------------------------------------------------
#' Vous aurez une base prixunitaire dont chque (p,u,t) aura 
# un seul prix p ; 

# On décide de prendre la moyenne

pricUnit <- subset(cereales, subset = !is.na(Price_p_u_t),
                   select= c("cereales__id", "Unite_achat", "Taille_achat", "Price_p_u_t"))
pricUnit <-pricUnit %>% group_by(cereales__id, Unite_achat, Taille_achat) %>%
  mutate(p_u_t = mean(Price_p_u_t, na.rm = T))

pricUnit$Price_p_u_t <- NULL

# Final step...
pricUnit <- distinct(pricUnit)


#Ramener cette sous-base dans la base cereales pour calculer les depenses de consommations--------------------------------------

cereales <- merge(cereales, pricUnit,
                  by.x=c("cereales__id", "Unite_cons", "Taille_cons"),
                  by.y= c("cereales__id", "Unite_achat", "Taille_achat"), all.x=T) 
# Last time, I made the mistake to forget the "all.x=T" argument
#  My Bad...

sum(is.na(cereales$p_u_t)) #  69

# Calcul des dépenses de cons
cereales[, dpses_cons := p_u_t*Qtty_cons] 


#1:: evaluer le taux de matching : n(Pc,Uc,Tc) aynt un prix P sur le-----------------------------------------
#' le nombre total de combinaison n(Pc,Uc,Tc);

# A ce niveau, nous avions commis une erreur pour l'exercie de la séance d'avant...
matchg_rate <- 100*sum(!is.na(cereales$p_u_t))/ length(cereales$p_u_t)

# 99.3786%


# Reflechir à comment valoriser les quantites n'ayant de prix ----------------------------------------------------


### _________________________________________HomeWork SEANCE 11______________________________________________________

#' Les cas de non-matching sont expliqués par des céréales achetées mais consommées soit 
#' avec une unité ou une taille différente de l'achat
#' soit avec une untité et une taille différentes de l'achat
#' 
#' Nous décidons de faire une imputation par la moyenne pour chaque céréale
#' en regardant les cas les plus proches :
#' S'il y a des cas où l'untié est la même (sorte de matching partiel), 
#' on prend la moyenne des prix de ces cas;
#' S'il y des cas où seule la taille est la même ou bien ni l'unité, 
#' ni la taille ne sont les mêmes, on prend la moyenne des prix sans autre considération.
 

# Saving it all_ for tests...
write.csv(cereales, "cereales1.csv")
cereales <- read.csv("cereales1.csv") 
cereales <- data.table(cereales)  # Converting it into a data table



#'On extrait pour cela une base qui contient les céréales, l'unité et la taille
#'pour les cas de mismatching


Mis_match <- subset(cereales, subset = is.na(p_u_t),
                     select= c("cereales__id", "Unite_cons", "Taille_cons")) %>% distinct()
# Turning it into a database
Mis_match <- data.table(Mis_match)

# On initialise la colonne contenant les prix estimés
Mis_match$Prix_estim <- 0

#'Rappel: la base pricUnit contient cereales_id, Unit_achat, Taille_achat et 
#'le prix p_u_t moyen correspondant
#'C'est un peu notre table de référence. (On le transforme en data.table)

pricUnit <- data.table(pricUnit)

j <- 1
for (cer in Mis_match$cereales__id ){
  
  # On extrait la colonne des prix pour lesquels la céréale et l'unité sont les mêmes
  interest <- pricUnit[cereales__id == cer & Unite_achat == Mis_match$Unite_cons[j],p_u_t]
                     
  if (length(interest)!=0){
    
    # Si ces cas existent, on remplace par la moyenne des prix dans la base mis_match
    Mis_match$Prix_estim[j] <- mean(interest)
  }
  else{
# Sinon, on remplace par la moyenne des prix seulenment pour les cas où la céréale est la même
    
    Mis_match$Prix_estim[j] <- mean(pricUnit[cereales__id== cer , p_u_t])
  }
  
  j <- j+1
}


#   On remet dans la base les prix estimés

cereales1 <- merge(cereales, Mis_match,
                  by=c("cereales__id", "Unite_cons", "Taille_cons"), all.x = T) 


# La colonne Prix_estim contient donc les prix estimés pour les cas de "unmatching"
