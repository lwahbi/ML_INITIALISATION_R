w = read.csv(file = "c:/data/Etude1.csv", header = TRUE, sep = ",")
head(w)
attach(w)

reg = lm(Y ~ X1 + X2)
summary(reg)

AIC(reg)
BIC(reg)

# Test de Fisher : p-valeur < 0:001, l'utilisation du mod�le de rlm est pertinente.

############################################
# Pr�diction : X1 = 157 et X2 = 11
##############################################
predict(reg, data.frame(X1 = 157, X2 = 11))
# une horloge qui a 157 ans et sur laquelle 11 personnes ont fait une offre sera vendue,
# en moyenne, 1606,828 pounds.


###################################################
###### Validation des hypoth�ses ###############
##################################################


############ Analyse graphic ######################

plot(w)
# On remarque qu'une liaison lin�aire entre Y et X1 est effectivement envisageable. 
#C'est un peu moins clair entre Y et X2.


# Analyse graphique des r�sidus

e = residuals(reg)
plot(e)
abline(h = 0, col = "red")

# On constate une relative sym�trie des r�sidus par rapport � l'axe des abscisses et 
# pas de structure apparente. Cela est encourageant pour la validation des hypoth�ses
# standards.
# On constate que le nuage de points obtenu est difficilement ajustable par une "ligne" et la moyenne

# Ind�pendance de erreur et X1;X2
plot(reg, 1)
# des valeurs de la ligne rouge est quasi nulle ; on admet que erreur et X1;X2 sont ind�pendantes.



# Ind�pendance des erreurs

par(mfrow = c(1, 2))
acf(e)
pacf(e)

# On ne constate aucune structure particuli�re (et pas de b�tons d�passent les bornes limites, � part
# le premier, ce qui est normal) ; on admet l'ind�pendance de



# �galit� des variances
plot(reg, 3)
# On ne constate pas de structure particuli�re, ce qui traduit une �galit� des variances

# Normalit� des erreurs
plot(reg, 2)
# On constate que les points sont � peu pr�s align�s, ce qui traduit la normalit�



########################################
## �tude de la multicolin�arit�
#########################################

library(car)
vif(reg)
# Comme les vif sont inf�rieurs � 5, il n'y a pas de lien lin�aire entre X1 et X2

##################################"
## D�tection des valeurs anormales 
####################################"
# On �tudie les distances de Cook des observations
plot(reg, 4)
# Aucune d'entre elles ne d�passe 1, il n'y a pas de valeur anormale a priori



##################################
#####" Conclusion 
# L'�tude statistique mise en oeuvre montre que le mod�le de rlm est adapt� au probl�me ; les hypoth�ses
# permettant la validation des principaux r�sultats d'estimation sont v�rifi�es.
#################################

# https://rstudio-pubs-static.s3.amazonaws.com/205694_3b195f29e9504d23aeb483ff1ffafeba.html#la-recherche-pas-a-pas


library(leaps)
library(datasets)
data(state)
statedata <-data.frame (state.x77, row.names=state.abb)
head(statedata,n = 3)

mf <- lm(Life.Exp~.,data=statedata) # modele complet
m0 <- lm(Life.Exp~1,data=statedata)  # choix du mod�le initiale avec constante seulement

# La s�lection ascendante utilisant le crit�re AIC
step(m0, scope=list(lower=m0, upper=mf),data=statedata, direction="forward")

step(mf, data=statedata,direction="backward")


bestModel <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost,data=statedata)

summary(bestModel)




bestModel1 <- lm(Life.Exp ~ Murder + HS.Grad + Frost,data=statedata)

summary(bestModel1)



AIC(bestModel)
AIC(bestModel1)

