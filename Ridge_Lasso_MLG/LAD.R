# ---------------------------- Robust regression ---------------------------- # 

# Los modelos de regresión robustos son útiles para filtrar relaciones lineales 
# cuando la variación aleatoria de los datos no es normal o cuando los datos 
# contienen valores atípicos significativos.

# Validation of library existence

if(!require('L1pack')) {
  install.packages('L1pack')
  library('L1pack')
}

if(!require('olsrr')) {
  install.packages('olsrr')
  library('olsrr')
}

# Data from the operation of a plant for the oxidation of ammonia to nitric 
# acid, measured on 21 consecutive days.

require(stats)

# --------------------------------- #
# ----------- OLS Model ----------- # 
# --------------------------------- #

summary(lm.stack <- lm(stack.loss ~ stack.x))

# ---> Bar Plot of cook's <--- #
# Bar Plot of cook's distance to detect observations that strongly influence 
# fitted values of the model. It is used to identify influential data points. 
# It depends on both the residual and leverage. 

# Examine how much all of the fitted values change when the ith observation is
# deleted.

ols_plot_cooksd_bar(lm.stack) # There is an outlier that affects predictions 

# Draw predicted vs observed data
plot(predict(lm.stack),                                # Draw plot using Base R
     stack.loss,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)

# --------------------------------- #
# - Least absolute deviation (L1) - # 
# --------------------------------- #

fm <- lad(stack.loss ~ ., data = stackloss)
summary(fm)

# Draw predicted vs observed data
plot(predict(fm),                                # Draw plot using Base R
     stack.loss,
     xlab = "Predicted Values",
     ylab = "Observed Values")
abline(a = 0,                                        # Add straight line
       b = 1,
       col = "red",
       lwd = 2)

x <- cbind(1, stack.x)
fm <- lad.fit(x, stack.loss, method = "BR")           # Coefficients
fm

