install.packages("multcomp")
library(multcomp)
data(cholesterol)
cholesterol
summary(cholesterol)

#Visualisation des données
library(ggplot2)
ggplot(cholesterol, aes(y=response, x=trt,colour=trt ,fill=trt))+
geom_boxplot(alpha=0.5, outlier.alpha=0)+
geom_jitter(width=0.25)+
theme_classic()

#Réalisation de l'ANOVA One Way
anova=lm(response~trt,data=cholesterol)

#Visualisation du résultat
library(car)
Anova(anova)

#Vérification des hypothèses
#1-Indépendance des résidus
library(car)
durbinWatsonTest(anova)

#2-Normalité des résidus
#Graphiquement
plot(anova,2)
#Par test
shapiro.test(residuals(anova))

#3-Homogénéité des variances
#Graphiquement
plot(anova,3)
#Par test
bartlett.test(residuals(anova)~cholesterol$trt)

#Interprétation
Anova(anova)

#TEST DE TUKEY 
tukey_test <- glht(anova, linfct = mcp(trt = "Tukey"))
summary(tukey_test)
par(mar=c(3,7,3,3))
plot(tukey_test)







