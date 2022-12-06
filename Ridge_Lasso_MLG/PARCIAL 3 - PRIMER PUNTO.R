x1 = c(2,4,6)
x2 = c(3,6,9)
x3 = 2*x1 - x2

X = cbind(x1,x2,x3)
X = as.matrix(X)
X


det(X)  ## determinante = 0
solve(t(X)%*%(X))  # no es invertible

##################-EXACTA-#############
x1 = c(2,4,6)
x2 = c(4,6,9)
y=3+2*x1+rnorm(3,0,2)
X = cbind(1,x1,x2)
X = as.matrix(X)
X

B=solve(t(X)%*%X)%*%(t(X)%*%y)

B
det(X)  ## determinante = 0
lm(y~x1+x2)
solve(t(X)%*%(X))
library(glmnet)
################PUNTO C########
# Ejemplo 8.7.2 ahora con n??meros simulados

set.seed(1234)
x = rnorm(7)
e = rt(7,3)  # Distribuci??n sim??trica 
y = 3*x + e

cor(x,y)

set.seed(123)
x[8] = rnorm(1)

y[8] = 20


plot(x,y)



install.packages("L1pack")






##### Punto 1 ####
# a
x1 = c(2,4,6)
x2 = c(3,6,9)
x3 = 2*x1 - x2

X = cbind(x1,x2,x3)
X = as.matrix(X)
X


det(X)  ## determinante = 0
solve(t(X)%*%(X))  # no es invertible


data("mtcars")
names(mtcars)

library(dplyr)


# mpg, disp, hp, dratm wt, qsec

numericas = mtcars[,c(1,3,4,5,6,7)]
round(cor(numericas),2)


# mpg variable dependiente

modelo = lm(mpg ~ ., data = numericas)
summary(modelo)

library(car)
car::vif(modelo)   #:: cuando uno solo quiere el metodo de la libreria

M = as.matrix(numericas)
det(t(M)%*%M)
solve(t(M)%*%M)


## 
install.packages("multiColl")

library(multiColl)

data(KG)

cor(KG)

modelo = lm(consumption ~ ., data = KG)
car::vif(modelo)


# R y D
manhours <- read.csv("https://github.com/EddytheDowdy/Students_Anova/blob/main/manhours.csv?raw=true")
fit = lm(manhours ~ occupanc + checkins + svcdesk + common + wings + berthing + rooms, data = manhours)
vif(fit)





### Punto c ####

manhours <- read.csv("https://github.com/EddytheDowdy/Students_Anova/blob/main/manhours.csv?raw=true")
fit = lm(manhours ~ occupanc + checkins + svcdesk + common + wings + berthing + rooms, data = manhours)
vif(fit)

## regresion ridge

library(nnet) #Par??metros

library(glmnet) #Modelo
#####################DTA
x = manhours[,-c(1,2)]
y = manhours[,2]


set.seed(123)
cvfitR <- cv.glmnet(as.matrix(x),y, alpha = 0)
coef(cvfitR)
coef(cvfitR, s = cvfitR$lambda.1se) # S?? :)

plot(cvfitR)

bestlambda = cvfitR$lambda.min
bestlambda

coef(cvfitR, lambda = bestlambda)
?glmnet
#alpha=1 is the lasso penalty, and alpha=0 the ridge penalty
fit<-glmnet(x,y,alpha=0)
fit$beta
### Lasso
cvfitRL <- cv.glmnet(as.matrix(x),y, alpha = 1)
cvfitRL$lambda.1se
cvfitRL$lambda.min
plot(cvfitRL)
regrlasso = glmnet::glmnet(x,y,alpha = 1, lambda = cvfitRL$lambda.min)
coef(regrlasso)

### punto e: Ridge y MCO

I = matrix(diag(rep(1,8)),nrow = 8) # Identidad
I

X = as.matrix(x)
X = cbind(c(rep(1,25)),X)
X = as.matrix(X)

lambda = 20#cvfitR$lambda.min
beridge = solve(t(X)%*%X +lambda*I)%*%t(X)%*%y
beridge
regridge = glmnet::glmnet(x,y,alpha = 0, lambda = 20)
coef(regridge)#VERFI
