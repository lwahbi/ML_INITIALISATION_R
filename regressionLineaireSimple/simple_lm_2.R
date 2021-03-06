#########################################################################"
############### R�gression lin�aire simple : 
##############  https://statistique-et-logiciel-r.com/regression-lineaire-simple-quand-les-hypotheses-ne-sont-pas-satisfaites/
##############################################################################################################
library(car)
scatterplot(prestige~education, data=Prestige) # Ici l'hypoth�se de lin�arit� est satisfaite

scatterplot(prestige~income, data=Prestige) # Ici l'hypoth�se de lin�arit� est non  satisfaite

############################################################################################
########## Quand l' hypoth�se de lin�arit� n'est pas satisfaite ###############"
############################################################################################
#### 2 approche abandonner la lin�arit�, ou essayer de l'am�liorer.

###  Am�liorer la lin�arit� en utilisant une transformation



### la fonction "log1p" qui applique la transformation log(1+x). Cela vous �vitera d'aboutir � des valeurs n�gatives lorsque 
### la valeur de x est proche de 0. (log1p(0.007) = 0.007 alors que log(0.07)=-4.96)
# evaluation de la lin�arit� apr�s transformation de la variable pr�dictive
scatterplot(prestige~log1p(income), data=Prestige)

# Pour la racine carr�e, il suffit d'utiliser la fonction "sqrt".

scatterplot(prestige~sqrt(income), data=Prestige) # Ici, la transformation par la racine carr�e fonctionne bien


mod_sqrt <- lm(prestige~sqrt(income), data=Prestige)
summary(mod_sqrt)


############################################################################################
########## Quand l'hypoth�se d'ind�pendance des r�sidus n'est pas satisfaite ###############"
############################################################################################

 # Un cas classique de non-ind�pendance des r�sidus est rencontr� lorsque la variable pr�dictive est une variable temporelle.
# Dans cette situation, les r�sidus sont dits "auto-corr�l�s".

library(tidyverse)

aq9 <- airquality %>% filter(Month==9)

head(aq9)

# r�gression lin�aire simple
aq9.lm <- lm(Ozone~Day, data=aq9)

# lag plot
acf(residuals(aq9.lm), main="aq9.lm") # on a autocorr�lation

durbinWatsonTest (aq9.lm) # methode statistique

# Le test de Durbin-Watson confirme l'auto-corr�lation de lag1, puisque la pvalue est �gale � 0.

# Cela n'est pas �tonnant, car on peut l�gitimement penser que le niveau d'Ozone du jour j+1 d�pend de celui observ� au temps pr�c�dent, c'est-�-dire au jour j


############# Prise en compte de l'auto-corr�lation par ajout d'une structure de corr�lation


## Sol : ajouter une structure corr�lation des r�sidus, dans le mod�le de r�gression, afin de mod�liser cette d�pendance
##  il s'agit d'ajouter une matrice de variance co-variance de type auto-regressive 1 (AR-1). 

library(nlme)

# mod�le avec structure de corr�lation
aq9.gls <- gls(Ozone~Day, data=aq9, correlation=corAR1(form=~Day),na.action=na.omit)

# mod�le sans structure de corr�lation
aq9.gls0 <- gls(Ozone~Day, data=aq9,na.action=na.omit)


AIC(aq9.gls,aq9.gls0) # L'AIC du mod�le avec structure de variance est nettement plus faible ==>  sa qualit� est meilleure


summary(aq9.gls0)
summary(aq9.gls)

# le coefficient de la pente a un peu boug� : -1.8 pour le mod�le sans structure contre -2.1
# En revanche, l'erreur standard de la pente varie �norm�ment, puisqu'elle est multipli�e par un facteur environ 2.5
# Avec �videmment une r�percussion sur la p-value qui passe de 1e-04 � 0.04.

############################################################"""
# De mani�re g�n�rale, lorsque l'auto-corr�lation des r�sidus n'est pas prise en compte, 
# la r�gression lin�aire conduit � des faux positifs, c'est-�-dire � conclure que la pente est significativement 
#diff�rente de 0, alors qu'en r�alit� elle ne l'est pas.
############################################################



############################################################################################
########## Quand la normalit� n'est pas satisfaite ###############"
############################################################################################









