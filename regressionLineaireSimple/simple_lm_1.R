#########################################################################"
############### Régression linéaire simple : 
##############  https://statistique-et-logiciel-r.com/tutoriel-r-comment-evaluer-si-deux-variables-numeriques-continues-sont-liees/
##############################################################################################################

# conc : concentration element chimic
# pSurface_cult : pourcentage de surface cultivé


mydata <-structure(list(conc = c(0.72, 0.72, 0.88, 0.99, 1.2, 1.303538175,
                                 0.93, 0.97, 1.54, 1.47, 1.61, 2.5, 1.29, 1.17, 1.06, 1.9, 1.88,
                                 2.06, 1.76, 2.37, 2.13, 2.12, 2.11, 1.81, 7.784433, 5.1, 1.95,
                                 3.49), pSurface_cult = c(6.269481783, 0, 0, 100, 77.55458855,
                                                          0, 22.04150899, 40.40614454, 40.60597188, 22.04150899, 22.04150899,
                                                          77.55458855, 40.40614454, 100, 52.58843992, 34.12077483, 98.94265481,
                                                          48.12059561, 67.60579421, 22.04150899, 5.024702025, 67.60579421,
                                                          42.14948675, 52.58843992, 60.99253217, 48.12059561, 100, 100)), 
                   class = "data.frame", row.names = c(NA,-28L), .Names = c("conc", "pSurface_cult"))
mydata


# 


library(ggplot2)
### guide pour démarer ggplot : https://bioinfo-fr.net/guide-de-demarrage-pour-ggplot2-un-package-graphique-pour-r

####
ggplot(mydata, aes(y=conc, x=pSurface_cult))+ geom_point()+ geom_smooth(colour="red", method="lm", se=FALSE)


########################################################################
########################### Régresion linaire ################################
########################################################################

mod_lm <-lm(conc ~ pSurface_cult,data=mydata)

summary(mod_lm)
######################## Evaluation des hypotèse ########################

########### Evaluation de l'hypothèse de normalité des résidus

shapiro.test(residuals(mod_lm))

# l'hypothèse est donc rejetée. Ce rejet est vraisemblablement du aux deux points extrêmes.

###### Evaluation de l'hypothèse d'homogénéité des résidus

# Methode graphic
plot(mod_lm$fitted.values, mod_lm$residuals)
# L'hypothèse d'homogénéité est acceptée si les points sont globalement distribués dans un rectangle ; c'est le cas ici.

# avec test statistique : test de Breusch-Pagan
library(lmtest)
bptest(mod_lm)

# les p-values est supérieures à 0.05 l'hypothèse d'homogénéité des résidus est acceptée.

######################## autocorrelation des erreurs ?
# lag plot
acf(residuals(mod_lm), main="mod_lm") # on a autocorrélation
pacf(residuals(mod_lm), main="mod_lm") # on a autocorrélation

durbinWatsonTest(residuals(mod_lm)) # methode statistique





##################################################################
##L'hypothèse de normalité ayant été rejetée par le test de Shapiro Wilk, une transformation log10 peut être appliquée
## à la variable réponse, (ici "conc") pour essayer de l'améliorer. ##############"""

# Ajustement du modèle

mod_lm2 <-lm(log10(conc) ~ pSurface_cult, data=mydata)

shapiro.test(residuals(mod_lm2))

## cette fois supérieure (mais tout juste) à 0.05. Au final, les résidus souffrent d'un défaut de normalité, 
## mais celui ci reste dans la limite de l'acceptable. Il faudra néanmoins rester vigilant sur la validité des résultats.
plot(mod_lm2,3)

bptest(mod_lm2)

## L'hypothèse d'homogénéité des résidus est donc acceptée.


##################################################################
###  pt influent + pt abérant 
######################################################"

library(car)

## Methode graphic
influenceIndexPlot(mod_lm2)

# le premier (en partant du bas) :  l'effet de levier (ou poids).Une donnée est atypique lorsque cette valeurest <0.05.
# le second plot celui des p-value de Bonferroni permet de mettre en évidence les outliers. Est considérée comme outlier une donnée ayant une p-value inférieure à 0.05.
# le troisième plot, celui des résidus studentizés permet également de mettre en évidence les outliers
# le quatrième plot, celui des distance de Cook permet d'évaluer l'influence des données sur les paramètres de régression.
# Plus la distance est élevée, plus la modification des paramètres de régression est importante. Le seuil de 1 est couramment utilisé pour considéré qu'une donnée est très influente.


## methode statistique : la méthode de Bonferonni

outlierTest(mod_lm2)

############################################################
#####################" Résultats #######################"
#########################################################"

summary(mod_lm2) 

# La partie Residuals : les valeurs absolues de Q1 et Q3 doivent être proches. nous savions que les résidus souffrent d'un défaut de normalité.
# p-value : aucun lien linéaire significatif entre les deux variable n'est mis en évidence.
# les résultats nous montrent qu'en moyenne le log10 de la concentration augmente d'environ 2.26.10^-3 lorsque 
#la surface cultivée augmente d'un pour cent.







######################################################"
###### que deviennent ces résultats lorsque la donnée 25 est retirée ##
###################################################################"

mod_lm3 <-lm(log10(conc) ~ pSurface_cult, data=mydata[-25,]) 

summary(mod_lm3)

#############################################################
## Lorsque la pvalue d'un test est comprise dans l'intervalle [0.05 ;0.1] certains considèrent qu'elle est presque 
## significative. Et de ce fait, qu'elle constitue une faible preuve (weak evidence ) de la relation linéaire. 
##Dans cette situation le terme de "tendance" est généralement employé, comme par exemple :
##"les résultats mettent en évidence une tendance linéaire entre les deux variables"
#############################################################################################


# qu'une transformation log de la concentration à du être employée pour satisfaire l' hypothèse de normalité des résidus. 
#De ce fait, la relation qui est étudiée n'est plus celle entre la concentration et 
#le pourcentage de surfaces cultivées mais entre le log10 de la concentration et le pourcentage de surfaces cultivées



#que malgré la transformation log de la réponse, la normalité des résidus reste médiocre

#que la taille de l'échantillon est assez faible :28

#qu'une donnée est outlier

# et que la pvalue est égale à 0.1.





