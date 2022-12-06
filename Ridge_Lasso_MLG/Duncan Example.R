library(car)
mod.ls <- lm(prestige ~ income + education, data=Duncan)
summary(mod.ls)

mod.ls.2 <- update(mod.ls, subset=-c(6,16,27))
summary(mod.ls.2)

library(MASS)
mod.huber <- rlm(prestige ~ income + education, data=Duncan)
summary(mod.huber)

plot(mod.huber$w, ylab="Huber Weight")
bigweights <- which(mod.huber$w < 0.9)
showLabels(1:45, mod.huber$w, rownames(Duncan), id.method=bigweights, cex.=.6)

mod.bisq <- rlm(prestige ~ income + education, data=Duncan, method='MM')
summary(mod.bisq, cor=F)

plot(mod.bisq$w, ylab="Bisquare Weight")