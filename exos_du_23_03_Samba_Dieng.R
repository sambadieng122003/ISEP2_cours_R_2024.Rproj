# Calculer la quantite achetee en kg
fusion$poids <- as.numeric(fusion$poids)
fusion$quantite_achetee_kg <- fusion$Qtty_achat * fusion$poids/1000
view(fusion)
str(fusion)

#le prix unitaire
fusion$prix_unitaire <- fusion$Value_achat/fusion$quantite_achetee_kg

## calculer les depenses de consommation
fusion$depenses_cons_menages <- fusion$Qtty_cons*fusion$prix_unitaire




#Valeurs abberantes:: détection et corections
visualisation <- boxplot(fusion$Qtty_cons, 
                         main = "Boxplot quantite consommé", # Titre du graphique
                         ylab = "Dépenses de consommation", # Nom de l'axe des ordonnées
                         col = "skyblue", 
                         border = "blue", 
                         horizontal = TRUE)
Q1 <- quantile(fusion$qtty_cons_kg, 0.25)
Q3 <- quantile(fusion$qtty_cons_kg, 0.75)
EQR <- Q3 - Q1

sup <- Q3 + 1.5 * EQR
inf <- Q1 - 1.5 * EQR
fusion$quantite_cons_kg <- fusion$Qtty_cons * fusion$poids/1000
view(fusion)
valeurs_aberrantes_inf <- fusion$fusion$quantite_cons_kg < inf
valeurs_aberrantes_sup <- fusion$fusion$quantite_cons_kg > sup

# Remplacer les valeurs aberrantes par la moyenne des données non aberrantes
moyenne_sans_aberrantes <- mean(fusion$quantite_cons_kg[!valeurs_aberrantes_inf & !valeurs_aberrantes_sup])
fusion$fusion$quantite_cons_kg[valeurs_aberrantes_inf | valeurs_aberrantes_sup] <- moyenne_sans_aberrantes

