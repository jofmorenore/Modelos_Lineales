x<-1:100
z<-5*x+rnorm(100,0,45)
cor(x,z)
data("trees")
View(trees)
library(corrplot)
cor(trees)
corrplot(cor(trees))
boxplot(trees$Girth,trees$Height)
boxplot(trees$Volume)
dim(trees)
dim(na.omit(trees))
#No hay problema de datos faltantes

mod<-lm(trees$Volume~trees$Girth+trees$Height)
summary(mod)
anova(mod)
plot(mod)

#SUPUESTOS
#NORMALIDAD
library(lmtest)
library(nortest)
shapiro.test(mod$residuals)#shapiro
lillie.test(residuals(mod))#ks
#HOMOGENEIDAD DE VARIANZAS
library(lmtest)
bptest(trees$Volume~trees$Girth+trees$Height)
#INDEPENDENCIA DE ERRORES
dwtest(trees$Volume~trees$Girth+trees$Height)
#FALLA!!!

#MULTICOLINEALIDAD
cor(trees$Girth,trees$Height)
library(car)
vif(mod)

# TRANSFORMACIÓN DE BOX Y COX
library(car)
boxCox(mod)
library(MASS)
b<-boxcox(mod)
optimo<-b$x[b$y==max(b$y)]
optimo
#0.3


#f

mod<-lm(trees$Volume^0.3~trees$Girth+trees$Height)
summary(mod)
anova(mod)
plot(mod)

#SUPUESTOS
#NORMALIDAD
library(lmtest)
library(nortest)
shapiro.test(mod$residuals)#shapiro
lillie.test(residuals(mod))#ks
#HOMOGENEIDAD DE VARIANZAS
library(lmtest)
bptest(trees$Volume~trees$Girth+trees$Height)
#INDEPENDENCIA DE ERRORES
dwtest(trees$Volume~trees$Girth+trees$Height)
#FALLA!!!

mod<-lm(trees$Volume~poly(trees$Girth,2)+poly(trees$Height,2)+trees$Girth*trees$Height)
summary(mod)
step(mod)


mod<- lm(trees$Volume^0.3 ~ trees$Girth+I(trees$Girth^2) + trees$Height)
summary(mod)
plot(mod)
#SUPESTOS
library(lmtest)
shapiro.test(mod$residuals)
dwtest(mod)
bptest(mod)
plot(mod)
#Como satisface todos los supuestos

