
##########################################################################
#####################################################
######### UNIVERSIDAD NACIONAL DE COLOMBIA, SEDE BOGOT?.
######### DEPARTAMENTO DE ESTAD?STICA.
############PROFESOR: LUIS FERNANDO GRAJALES H. ####
### Enviarme correcciones o inquietudes a 
############ lfgrajalesh@unal.edu.co
###### 2022_CURSO: Modelos Lineales.
####### R version 4.1.2
#### EJEMPLOS DE regresi?n lineal SIMPLE.
library(MASS)
library(lmtest)
library(nortest)
#############################################################
###################### EJEMPLOS RLS con TALLA DE BEB?S 
########## Base de datos (parcial) de 7 beb?s reci?n nacidos.
########## ### Una variable respuesta: Talla_hoy (en cm),
######### 4 Covariables, continuas: Edad_d?as,
###### Talla_nacer, Peso_nacer y Torax_nacer.  
###### Los datos completos est?n en Walpole y Myers (2017).
###########################################################
#### REGRESI?N tallas.beb?s 
babies<-read.table("c:/Datos.clase/Tallas.babies.csv", header=T,sep=";")
babies
attach(babies)
head(babies)
summary(babies)
dim(babies)
library(MASS)
library(lmtest)
library(nortest)
##### MODELO DE RLS CON TORAX
baby.fit11<-lm(Talla_hoy~Torax_nacer) # 1) AJUSTAR EL MODELO 
baby.fit11     ### 2) COEFICIENTES ESTIMADOS bo y b1
resid.torax<-residuals(baby.fit11)  ### Residuales (errores estimados)
resid.torax
########## COMIENZO A VALIDAR SUPUESTOS.
shapiro.test(residuals(baby.fit11)) ## normalidad 
lillie.test(residuals(baby.fit11)) ### normalidad
bptest(Talla_hoy~ Torax_nacer) ### Homocedasticidad Breusch-Pagan 
gqtest(lm(Talla_hoy~ Torax_nacer)) ### Homocedasticidad Goldfeld-Quandt
dwtest(Talla_hoy ~ residuals(baby.fit11)) # Durbin Watson independencia
### EN RESUMEN: No hay normalidad, no hay varianza constante, s? independencia
### en nuestra estrategia: detenemos el proceso y volvemos al modelo te?rico 1. 

########## INTENTO OTRO MODELO DE RLS datos babies.
plot(Peso_nacer, Talla_hoy)
baby.fit12<-lm(Talla_hoy~Peso_nacer) # 1) AJUSTAR EL MODELO 
baby.fit12     ### 2) COEFICIENTES ESTIMADOS bo y b1
resid.peso<-residuals(baby.fit12)  ### Residuales (errores estimados)
resid.peso
shapiro.test(residuals(baby.fit12)) ## test normalidad 
lillie.test(residuals(baby.fit12)) ### test normalidad
bptest(Talla_hoy~ Peso_nacer) ### Test Homocedasticidad Breusch-Pagan 
gqtest(lm(Talla_hoy~ Peso_nacer)) ### Test Homocedasticidad Goldfeld-Quandt
dwtest(Talla_hoy ~ residuals(baby.fit12)) # Test Durbin Watson independencia
### EN RESUMEN: Se cumplen los tres supuestos !!!!!
### ?qu? sigue en nuestra estrategia?   Tiene sentido Tabla ANOVA.
##### ?Existe modelo de RLS?
qf(0.99,1,5)
anova(baby.fit12) ### S? SE PUEDE INTERPRETAR LA PENDIENTE.
summary(baby.fit12)
summary(Peso_nacer) ### ?tiene sentido interpretar bo estimado?

########## INTENTO OTRO MODELO DE RLS datos babies. Talla_nacer.
## plot(Peso_nacer, Talla_hoy)
baby.fit13<-lm(Talla_hoy~Talla_nacer) # 1) AJUSTAR EL MODELO 
baby.fit13   ### 2) COEFICIENTES ESTIMADOS bo y b1
resid.3<-residuals(baby.fit13)  ### Residuales (errores estimados)
resid.3
shapiro.test(residuals(baby.fit13)) ## test normalidad 
lillie.test(residuals(baby.fit13)) ### test normalidad
bptest(Talla_hoy~ Talla_nacer) ### Test Homocedasticidad Breusch-Pagan 
gqtest(lm(Talla_hoy~ Talla_nacer)) ### Test Homocedasticidad Goldfeld-Quandt
dwtest(Talla_hoy ~ residuals(baby.fit13)) # Test Durbin Watson independencia
anova(baby.fit13)
data(road)
summary(road)
attach(road)
help(road)
















