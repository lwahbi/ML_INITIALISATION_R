w = read.csv(file = "c:/data/Etude1.csv", header = TRUE, sep = ",")
head(w)
attach(w)

reg = lm(Y ~ X1 + X2)
summary(reg)

AIC(reg)
BIC(reg)

# Test de Fisher : p-valeur < 0:001, l'utilisation du modèle de rlm est pertinente.

############################################
# Prédiction : X1 = 157 et X2 = 11
##############################################
predict(reg, data.frame(X1 = 157, X2 = 11))
# une horloge qui a 157 ans et sur laquelle 11 personnes ont fait une offre sera vendue,
# en moyenne, 1606,828 pounds.


###################################################
###### Validation des hypothèses ###############
##################################################


############ Analyse graphic ######################

plot(w)
# On remarque qu'une liaison linéaire entre Y et X1 est effectivement envisageable. 
#C'est un peu moins clair entre Y et X2.


# Analyse graphique des résidus

e = residuals(reg)
plot(e)
abline(h = 0, col = "red")

# On constate une relative symétrie des résidus par rapport à l'axe des abscisses et 
# pas de structure apparente. Cela est encourageant pour la validation des hypothèses
# standards.
# On constate que le nuage de points obtenu est difficilement ajustable par une "ligne" et la moyenne

# Indépendance de erreur et X1;X2
plot(reg, 1)
# des valeurs de la ligne rouge est quasi nulle ; on admet que erreur et X1;X2 sont indépendantes.



# Indépendance des erreurs

par(mfrow = c(1, 2))
acf(e)
pacf(e)

# On ne constate aucune structure particulière (et pas de bâtons dépassent les bornes limites, à part
# le premier, ce qui est normal) ; on admet l'indépendance de



# Égalité des variances
plot(reg, 3)
# On ne constate pas de structure particulière, ce qui traduit une égalité des variances

# Normalité des erreurs
plot(reg, 2)
# On constate que les points sont à peu près alignés, ce qui traduit la normalité



########################################
## Étude de la multicolinéarité
#########################################

library(car)
vif(reg)
# Comme les vif sont inférieurs à 5, il n'y a pas de lien linéaire entre X1 et X2

##################################"
## Détection des valeurs anormales 
####################################"
# On étudie les distances de Cook des observations
plot(reg, 4)
# Aucune d'entre elles ne dépasse 1, il n'y a pas de valeur anormale a priori



##################################
#####" Conclusion 
# L'étude statistique mise en oeuvre montre que le modèle de rlm est adapté au problème ; les hypothèses
# permettant la validation des principaux résultats d'estimation sont vérifiées.
#################################

# https://rstudio-pubs-static.s3.amazonaws.com/205694_3b195f29e9504d23aeb483ff1ffafeba.html#la-recherche-pas-a-pas


library(leaps)
library(datasets)
data(state)
statedata <-data.frame (state.x77, row.names=state.abb)
head(statedata,n = 3)

mf <- lm(Life.Exp~.,data=statedata) # modele complet
m0 <- lm(Life.Exp~1,data=statedata)  # choix du modèle initiale avec constante seulement

# La sélection ascendante utilisant le critère AIC
step(m0, scope=list(lower=m0, upper=mf),data=statedata, direction="forward")

step(mf, data=statedata,direction="backward")


bestModel <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,data=statedata)

summary(bestModel)




bestModel1 <- lm(Life.Exp ~ Murder + HS.Grad + Frost,data=statedata)

summary(bestModel1)



AIC(bestModel)
AIC(bestModel1)

