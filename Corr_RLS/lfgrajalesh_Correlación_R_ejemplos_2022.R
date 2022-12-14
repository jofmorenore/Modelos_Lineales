##########################################################################
#####################################################
########### UNIVERSIDAD NACIONAL DE COLOMBIA
############PROFESOR: LUIS FERNANDO GRAJALES H. ####
########### CURSO: M?TODOS DE REGRESI?N
###### MODELOS LINEALES.
###### 2022
####### R version 4.1.2
#### EJEMPLOS DE COEFICIENTES DE CORRELACI?N PEARSON Y SPEARMAN
###### DECISI?N CON BASE EN PRUEBA DE MARDIA: AC?, NORMALIDAD BIVARIADA
library(MASS)
library(MVN) ### Buscar su equivalente en v. 4.1.2 de R 
#### Buscar el Test De Mardia...
################ EJEMPLO 1: 8 GRANOLAS.
ma?z <- c(2.4, 3.4, 4.6, 3.7, 2.2, 3.3,  4.0, 2.1)
Cacahuates <- c(1.33, 2.12, 1.8, 1.65, 2.0, 1.76, 2.11,1.63)
ma?z
par(mfrow=c(1,2))
plot(ma?z, Cacahuates,main="lfgrajalesh. Apoyo a diapositivas CORRELACI?N")
plot(Cacahuates, ma?z, main="lfgrajalesh. Apoyo a diapositivas CORRELACI?N")
#### IDEA VISUAL DE CORRELACI?N LINEAL POSITIVA.
##### ?Cu?l coeficiente de correlaci?n es apropiado?
######## Primero hacemos la prueba de Mardia donde Ho es
####### Ho: el vector aleatorio proviene de una normal bivariada.
####### Ha: el vector aleatorio NO proviene de una normal bivariada.
GRANOLAS = data.frame (cbind(ma?z, Cacahuates))
GRANOLAS
library(MVN)
mvn(GRANOLAS) ### 
###### valores.p: 0.95 (Skewness) y 0.32 (Kurtosis)
###################################################
###### SOLICITANDO A R el coeficiente de correlaci?n de Pearson. 
########## Tambi?n produce el contraste de hip?tesis
############# Ha: rho es distinto de cero. 
cor.test(ma?z, Cacahuates, data="GRANOLAS") 
########## No rechazo H0. (? rho puede ser cero (valor.p=0.4))
################# r = 0.35 (Interpretar significancia y signo).

################ EJEMPLO 2:CIGARRILLOS.
alquitran=c(14,17,28,17,16,13,24,25,18,31)
nicotina=c(0.9,1.1,1.6,1.8,1.0,0.8,1.5,1.4,1.2,2)
par(mfrow=c(2,1))
plot(alquitran,nicotina,main="lfgrajalesh. Apoyo a dipositivas CORRELACI?N")
plot(nicotina, alquitran, main="lfgrajalesh. Apoyo a dipositivas CORRELACI?N")

#### IDEA VISUAL DE CORRELACI?N LINEAL POSITIVA.
##### ?Cu?l coeficiente de correlaci?n es apropiado?
######## Primero hacemos la prueba de Mardia donde Ho es
####### Ho: el vector aleatorio distribuye normal bivariado.
cigarrillo = data.frame (cbind(alquitran,nicotina))
cigarrillo
library(MVN)
mvn(cigarrillo)

### Conclusi?n nuestra: El vector aleatorio no proviene de normal bivariada.  
### Apropiado: el coeficiente de correlaci?n no param?trico de Spearman.

###### SOLICITANDO A R el coeficiente de correlaci?n de Spearman. 
########## Tambi?n produce el contraste de hip?tesis
############# Ha: rho es distinto de cero. 
cor.test(alquitran, nicotina, data="cigarrillo", method="spearman") 
### RESULTADOS
### 1) p.value= 0.002 
#### 2) sample estimates:
###      rho 
#### 0.8389097 
###### INTERPRETAR!!!!









