library("car")
library(ggplot2)
library(olsrr)
#Si tratamos de hacer una regresi?n lineal 
#tradicional, podr?amos ver un patr?n anormal de los datos, 
#con algunos valores at?picos:
plot(Duncan$education, Duncan$income, data=Duncan)
view(Duncan)
ggplot(Duncan, aes(x = education, y = income)) + 
  geom_point() + stat_smooth(method = "lm", col = "red") + theme_minimal() +
  ggtitle("Income vs. education")

mod <- lm(income ~ education, data = Duncan)

#Intentemos hacer un gr?fico de barras de Cook
#para ver los valores que son demasiado influyentes.
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2)
abline(h = 4*mean(cooksd, na.rm=T), col="red") 
text(x=1:length(cooksd)+1, y=cooksd,
       labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),
                     names(cooksd),""),
       col="red")

fitLS <- lm(income ~ education, data = Duncan)
ols_plot_cooksd_bar(mod)
#Podemos ver en la gr?fica tres valores at?picos que est?n 
#estropeando el m?todo de regresi?n lineal. 
#Est? claro que tenemos que tratar de resolver esto con una
#versi?n robusta del m?todo de regresi?n.
#Eliminarlos no es una opci?n ya que no son errores de entrada, 
#sino datos reales que representan a personas que tienen ingresos 
#inusualmente altos teniendo en cuenta su nivel educativo.


library("MASS")
#Huber loss
fitH <- rlm(income ~ education, data = Duncan, k2 = 1.345) 
#LMS
fitLMS <- lqs(income ~ education, data = Duncan, method = "lms")
#LTS
fitLTS <- lqs(income ~ education, data = Duncan, method = "lts")
#S-estimator
fitS <- lqs(income ~ education, data = Duncan, method = "S")
#MM-estimator
fitMM <- rlm(income ~ education, data = Duncan, method = "MM")

plot(Duncan$education, Duncan$income,
     xlab = "education", ylab = "income", type = "p", 
     pch = 20, cex = .8)
abline(fitLS, col = 1) 
abline(fitH, col = 2) 
abline(fitLTS, col = 4) 
abline(fitLMS, col = 5) 
abline(fitS, col = 6) 
abline(fitMM, col = 7) 
legend(0, 80, c("LS", "Huber","LTS","LMS",
                "S-estimador","MM-estimador" ),
       lty = rep(1, 7), bty = "n",
       col = c(1, 2, 4, 5, 6, 7))

summary(fitLS)$r.squared
summary(fitLS)$coefficients[,4]