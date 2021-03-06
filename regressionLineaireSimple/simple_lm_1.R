#########################################################################"
############### R�gression lin�aire simple : 
##############  https://statistique-et-logiciel-r.com/tutoriel-r-comment-evaluer-si-deux-variables-numeriques-continues-sont-liees/
##############################################################################################################

# conc : concentration element chimic
# pSurface_cult : pourcentage de surface cultiv�


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
### guide pour d�marer ggplot : https://bioinfo-fr.net/guide-de-demarrage-pour-ggplot2-un-package-graphique-pour-r

####
ggplot(mydata, aes(y=conc, x=pSurface_cult))+ geom_point()+ geom_smooth(colour="red", method="lm", se=FALSE)


########################################################################
########################### R�gresion linaire ################################
########################################################################

mod_lm <-lm(conc ~ pSurface_cult,data=mydata)

summary(mod_lm)
######################## Evaluation des hypot�se ########################

########### Evaluation de l'hypoth�se de normalit� des r�sidus

shapiro.test(residuals(mod_lm))

# l'hypoth�se est donc rejet�e. Ce rejet est vraisemblablement du aux deux points extr�mes.

###### Evaluation de l'hypoth�se d'homog�n�it� des r�sidus

# Methode graphic
plot(mod_lm$fitted.values, mod_lm$residuals)
# L'hypoth�se d'homog�n�it� est accept�e si les points sont globalement distribu�s dans un rectangle ; c'est le cas ici.

# avec test statistique : test de Breusch-Pagan
library(lmtest)
bptest(mod_lm)

# les p-values est sup�rieures � 0.05 l'hypoth�se d'homog�n�it� des r�sidus est accept�e.

######################## autocorrelation des erreurs ?
# lag plot
acf(residuals(mod_lm), main="mod_lm") # on a autocorr�lation
pacf(residuals(mod_lm), main="mod_lm") # on a autocorr�lation

durbinWatsonTest(residuals(mod_lm)) # methode statistique





##################################################################
##L'hypoth�se de normalit� ayant �t� rejet�e par le test de Shapiro Wilk, une transformation log10 peut �tre appliqu�e
## � la variable r�ponse, (ici "conc") pour essayer de l'am�liorer. ##############"""

# Ajustement du mod�le

mod_lm2 <-lm(log10(conc) ~ pSurface_cult, data=mydata)

shapiro.test(residuals(mod_lm2))

## cette fois sup�rieure (mais tout juste) � 0.05. Au final, les r�sidus souffrent d'un d�faut de normalit�, 
## mais celui ci reste dans la limite de l'acceptable. Il faudra n�anmoins rester vigilant sur la validit� des r�sultats.
plot(mod_lm2,3)

bptest(mod_lm2)

## L'hypoth�se d'homog�n�it� des r�sidus est donc accept�e.


##################################################################
###  pt influent + pt ab�rant 
######################################################"

library(car)

## Methode graphic
influenceIndexPlot(mod_lm2)

# le premier (en partant du bas) :  l'effet de levier (ou poids).Une donn�e est atypique lorsque cette valeurest <0.05.
# le second plot celui des p-value de Bonferroni permet de mettre en �vidence les outliers. Est consid�r�e comme outlier une donn�e ayant une p-value inf�rieure � 0.05.
# le troisi�me plot, celui des r�sidus studentiz�s permet �galement de mettre en �vidence les outliers
# le quatri�me plot, celui des distance de Cook permet d'�valuer l'influence des donn�es sur les param�tres de r�gression.
# Plus la distance est �lev�e, plus la modification des param�tres de r�gression est importante. Le seuil de 1 est couramment utilis� pour consid�r� qu'une donn�e est tr�s influente.


## methode statistique : la m�thode de Bonferonni

outlierTest(mod_lm2)

############################################################
#####################" R�sultats #######################"
#########################################################"

summary(mod_lm2) 

# La partie Residuals : les valeurs absolues de Q1 et Q3 doivent �tre proches. nous savions que les r�sidus souffrent d'un d�faut de normalit�.
# p-value : aucun lien lin�aire significatif entre les deux variable n'est mis en �vidence.
# les r�sultats nous montrent qu'en moyenne le log10 de la concentration augmente d'environ 2.26.10^-3 lorsque 
#la surface cultiv�e augmente d'un pour cent.







######################################################"
###### que deviennent ces r�sultats lorsque la donn�e 25 est retir�e ##
###################################################################"

mod_lm3 <-lm(log10(conc) ~ pSurface_cult, data=mydata[-25,]) 

summary(mod_lm3)

#############################################################
## Lorsque la pvalue d'un test est comprise dans l'intervalle [0.05 ;0.1] certains consid�rent qu'elle est presque 
## significative. Et de ce fait, qu'elle constitue une faible preuve (weak evidence ) de la relation lin�aire. 
##Dans cette situation le terme de "tendance" est g�n�ralement employ�, comme par exemple :
##"les r�sultats mettent en �vidence une tendance lin�aire entre les deux variables"
#############################################################################################


# qu'une transformation log de la concentration � du �tre employ�e pour satisfaire l' hypoth�se de normalit� des r�sidus. 
#De ce fait, la relation qui est �tudi�e n'est plus celle entre la concentration et 
#le pourcentage de surfaces cultiv�es mais entre le log10 de la concentration et le pourcentage de surfaces cultiv�es



#que malgr� la transformation log de la r�ponse, la normalit� des r�sidus reste m�diocre

#que la taille de l'�chantillon est assez faible :28

#qu'une donn�e est outlier

# et que la pvalue est �gale � 0.1.





