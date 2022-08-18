##########################################################################
#####################################################
########### UNIVERSIDAD NACIONAL DE COLOMBIA
############PROFESOR: LUIS FERNANDO GRAJALES H. ####
########### CURSO: MÉTODOS DE REGRESIÓN
###### MODELOS LINEALES.
###### 2022
####### R version 4.1.2
#### EJEMPLOS DE COEFICIENTES DE CORRELACIÓN PEARSON Y SPEARMAN
###### DECISIÓN CON BASE EN PRUEBA DE MARDIA: ACÁ, NORMALIDAD BIVARIADA
library(MASS)
library(MVN) ### Buscar su equivalente en v. 4.1.2 de R 
#### Buscar el Test De Mardia...
################ EJEMPLO 1: 8 GRANOLAS.
maíz <- c(2.4, 3.4, 4.6, 3.7, 2.2, 3.3,  4.0, 2.1)
Cacahuates <- c(1.33, 2.12, 1.8, 1.65, 2.0, 1.76, 2.11,1.63)
maíz
par(mfrow=c(1,2))
plot(maíz, Cacahuates,main="lfgrajalesh. Apoyo a diapositivas CORRELACIÓN")
plot(Cacahuates, maíz, main="lfgrajalesh. Apoyo a diapositivas CORRELACIÓN")
#### IDEA VISUAL DE CORRELACIÓN LINEAL POSITIVA.
##### ¿Cuál coeficiente de correlación es apropiado?
######## Primero hacemos la prueba de Mardia donde Ho es
####### Ho: el vector aleatorio proviene de una normal bivariada.
####### Ha: el vector aleatorio NO proviene de una normal bivariada.
GRANOLAS = data.frame (cbind(maíz, Cacahuates))
GRANOLAS
library(MVN)
mvn(GRANOLAS) ### 
###### valores.p: 0.95 (Skewness) y 0.32 (Kurtosis)
###################################################
###### SOLICITANDO A R el coeficiente de correlación de Pearson. 
########## También produce el contraste de hipótesis
############# Ha: rho es distinto de cero. 
cor.test(maíz, Cacahuates, data="GRANOLAS") 
########## No rechazo H0. (ó rho puede ser cero (valor.p=0.4))
################# r = 0.35 (Interpretar significancia y signo).

################ EJEMPLO 2:CIGARRILLOS.
alquitran=c(14,17,28,17,16,13,24,25,18,31)
nicotina=c(0.9,1.1,1.6,1.8,1.0,0.8,1.5,1.4,1.2,2)
par(mfrow=c(2,1))
plot(alquitran,nicotina,main="lfgrajalesh. Apoyo a dipositivas CORRELACIÓN")
plot(nicotina, alquitran, main="lfgrajalesh. Apoyo a dipositivas CORRELACIÓN")

#### IDEA VISUAL DE CORRELACIÓN LINEAL POSITIVA.
##### ¿Cuál coeficiente de correlación es apropiado?
######## Primero hacemos la prueba de Mardia donde Ho es
####### Ho: el vector aleatorio distribuye normal bivariado.
cigarrillo = data.frame (cbind(alquitran,nicotina))
cigarrillo
library(MVN)
mvn(cigarrillo)

### Conclusión nuestra: El vector aleatorio no proviene de normal bivariada.  
### Apropiado: el coeficiente de correlación no paramétrico de Spearman.

###### SOLICITANDO A R el coeficiente de correlación de Spearman. 
########## También produce el contraste de hipótesis
############# Ha: rho es distinto de cero. 
cor.test(alquitran, nicotina, data="cigarrillo", method="spearman") 
### RESULTADOS
### 1) p.value= 0.002 
#### 2) sample estimates:
###      rho 
#### 0.8389097 
###### INTERPRETAR!!!!









