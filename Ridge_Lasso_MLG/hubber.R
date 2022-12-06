### Regresión robusta ##


### Correlación
set.seed(123)
x = rnorm(10)
y = rnorm(10)
plot(x,y)

cor(x,y)



x[11] = 20
y[11] = 20*0.6287

plot(x,y)
cor(x,y)


par(mfrow = c(1,1), bg = "white")
require(foreign)
require(MASS)
library(lmtest)
library(olsrr)


# Cargamos los datos
cdata <- read.dta("https://stats.idre.ucla.edu/stat/data/crime.dta")
summary(cdata)


# Hagamos una primera aproximación de los datos
plot(crime ~ single, data = cdata)
cor(cdata$crime, cdata$single)

# Hagamos una regresión lineal simple
summary(ols <- lm(crime ~ single, data = cdata))

# Los residuales van de -767.42 hasta 719.7... ¿Bueno o malo...?

# Verificación de supuestos
# Normalidad
shapiro.test(ols$residuals)

# Varianza constante

bptest(ols)  # No se cumple el supuesto
plot(ols$residuals)


# Qué hacer?

library(ggplot2)

# Gráfico con ggplot2
ggplot(data = cdata, aes(x = single, y = crime, label = state)) +
   geom_point(size = 1) #+ geom_text()



ggplot(data = cdata, aes(x = single, y = crime, label = state)) +
  geom_point(size = 1)  + geom_smooth(method = "lm", se = F)


# Hay puntos que parecen no ajustarse bien a la recta


# Puntos con valores extremos de X se dice que tienen alto leverage

# Puntos con alto leverage tienden a mover la recta de regresión

# Si estos puntos están fuera del patrón general, pueden ser puntos influyentes

x11()
par(bg = "gray90")
plot(cdata$crime ~ cdata$single, pch = 16)
grid(col = "black")
abline(ols, col = "red", lw = 2)
identify(x = cdata$single, y = cdata$crime, plot =T, n = 3)



opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)


#plot1: Residuals vs fitted
# Este gráfico permite ver si los errores siguen alguna tendencia
# no lineal.
# BUENO: que se encuentren la misma cantidad de puntos tanto por
# encima como por debajo de la línea y que formen una figura 
# horizontal...


# plot2:  Normal Q-Q plot
# Este gráfico permite ver si los residuales se pueden asumir normales
# BUENO: que casi todos los puntos se encuentren sobre esa línea
# diagonal.


# plot 3: Scale-location
# Este gráfico es para apreciar si hay homogeneidad de varianzas o no.
# BUENO: Que los puntos se encuentren distribuidos uniformemente
# a lo largo de la línea. Es decir, que no se vea tendencia

# plot 4: Residuals vs Leverage
# Este gráfico sirve para encontrar observaciones extremas
# que podrían afectar el modelo de regresión
# Bueno: que el valor absoluto de cualquiera sea menor a 3.

# Hablar del levarage. Mostrar gráfico con la línea


## Distancia de Cook. Se suele usar como umbral 4/n
d1 <- cooks.distance(ols) # distancia de Cook
r <- stdres(ols)   # residuales estandarizados
a <- cbind(cdata, d1, r) # c
a[d1 > 4/51, ] # 4/51 = 0.078

# dc tiene una distancia de Cook muy grande.

ols_plot_cooksd_bar(ols)


# Veamos los mayores residuales (en valor absoluto)

rabs <- abs(r)
a <- cbind(cdata, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:5, ]




# Hagamos nuestra primera regresión robusta

summary(rr.huber <- rlm(crime ~ single, data = cdata))


hweights <- data.frame(state = cdata$state, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:15, ]


abline(rr.huber, col = "blue", lw = 2)
# Ven lo que pasa con la línea azul?



rr.bisquare <- rlm(crime ~  single, data=cdata, psi = psi.bisquare)
summary(rr.bisquare)

biweights <- data.frame(state = cdata$state, resid = rr.bisquare$resid, weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]


abline(rr.bisquare, col = "orange", lw = 2)

legend(x = "bottomright",legend = c("OLS", "Huber","BS"), col = c("red","blue","orange"), lty = 1)


## Volvamos al modelo de regresión lineal simple.

subdata = cdata[-c(9,25,51),]

ols2 = lm(crime ~ single, data = subdata)

models = list(ols, ols2)

models


