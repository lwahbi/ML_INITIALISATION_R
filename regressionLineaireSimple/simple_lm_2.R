#########################################################################"
############### Régression linéaire simple : 
##############  https://statistique-et-logiciel-r.com/regression-lineaire-simple-quand-les-hypotheses-ne-sont-pas-satisfaites/
##############################################################################################################
library(car)
scatterplot(prestige~education, data=Prestige) # Ici l'hypothèse de linéarité est satisfaite

scatterplot(prestige~income, data=Prestige) # Ici l'hypothèse de linéarité est non  satisfaite

############################################################################################
########## Quand l' hypothèse de linéarité n'est pas satisfaite ###############"
############################################################################################
#### 2 approche abandonner la linéarité, ou essayer de l'améliorer.

###  Améliorer la linéarité en utilisant une transformation



### la fonction "log1p" qui applique la transformation log(1+x). Cela vous évitera d'aboutir à des valeurs négatives lorsque 
### la valeur de x est proche de 0. (log1p(0.007) = 0.007 alors que log(0.07)=-4.96)
# evaluation de la linéarité aprés transformation de la variable prédictive
scatterplot(prestige~log1p(income), data=Prestige)

# Pour la racine carrée, il suffit d'utiliser la fonction "sqrt".

scatterplot(prestige~sqrt(income), data=Prestige) # Ici, la transformation par la racine carrée fonctionne bien


mod_sqrt <- lm(prestige~sqrt(income), data=Prestige)
summary(mod_sqrt)


############################################################################################
########## Quand l'hypothèse d'indépendance des résidus n'est pas satisfaite ###############"
############################################################################################

 # Un cas classique de non-indépendance des résidus est rencontré lorsque la variable prédictive est une variable temporelle.
# Dans cette situation, les résidus sont dits "auto-corrélés".

library(tidyverse)

aq9 <- airquality %>% filter(Month==9)

head(aq9)

# régression linéaire simple
aq9.lm <- lm(Ozone~Day, data=aq9)

# lag plot
acf(residuals(aq9.lm), main="aq9.lm") # on a autocorrélation

durbinWatsonTest (aq9.lm) # methode statistique

# Le test de Durbin-Watson confirme l'auto-corrélation de lag1, puisque la pvalue est égale à 0.

# Cela n'est pas étonnant, car on peut légitimement penser que le niveau d'Ozone du jour j+1 dépend de celui observé au temps précédent, c'est-à-dire au jour j


############# Prise en compte de l'auto-corrélation par ajout d'une structure de corrélation


## Sol : ajouter une structure corrélation des résidus, dans le modèle de régression, afin de modéliser cette dépendance
##  il s'agit d'ajouter une matrice de variance co-variance de type auto-regressive 1 (AR-1). 

library(nlme)

# modèle avec structure de corrélation
aq9.gls <- gls(Ozone~Day, data=aq9, correlation=corAR1(form=~Day),na.action=na.omit)

# modèle sans structure de corrélation
aq9.gls0 <- gls(Ozone~Day, data=aq9,na.action=na.omit)


AIC(aq9.gls,aq9.gls0) # L'AIC du modèle avec structure de variance est nettement plus faible ==>  sa qualité est meilleure


summary(aq9.gls0)
summary(aq9.gls)

# le coefficient de la pente a un peu bougé : -1.8 pour le modèle sans structure contre -2.1
# En revanche, l'erreur standard de la pente varie énormément, puisqu'elle est multipliée par un facteur environ 2.5
# Avec évidemment une répercussion sur la p-value qui passe de 1e-04 à 0.04.

############################################################"""
# De manière générale, lorsque l'auto-corrélation des résidus n'est pas prise en compte, 
# la régression linéaire conduit à des faux positifs, c'est-à-dire à conclure que la pente est significativement 
#différente de 0, alors qu'en réalité elle ne l'est pas.
############################################################



############################################################################################
########## Quand la normalité n'est pas satisfaite ###############"
############################################################################################









