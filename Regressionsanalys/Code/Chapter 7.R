load("worker and supervisors.RData")
## OLS
OLS <- lm(Supervisor ~ Worker, data = Data)
vcov(OLS)
summary(OLS)
## WLS, one illustration
WLS <- lm(Supervisor ~ Worker, data = Data,
          weights = 1 / Worker ^ 2) # w_i = 1 / worker ^ 2
summary(WLS)
## Sandwich estimator
library(sandwich)
vcovHC(OLS) 
sqrt(diag(vcovHC(OLS))) # sandwich estimator of standard errors 
coef(OLS) - 1.96 * sqrt(diag(vcovHC(OLS)))
coef(OLS) + 1.96 * sqrt(diag(vcovHC(OLS)))

## Delta method
head(Data)
Data$Lonsq <- Data$Lon ^ 2
LR <- lm(Temp ~ Lat + Lon + Lonsq, data = Data)
summary(LR)
Der <- matrix(c(0, 0, -0.5 / coef(LR)[4], 
                0.5 * coef(LR)[3] / coef(LR)[4] ^ 2), nrow = 4, ncol = 1)
t(Der) %*% vcov(LR) %*% Der # vcov(LR) = V(beta hat)
t(Der) %*% vcovHC(LR) %*% Der # vcov(LR) = V(beta hat), but sandwich estimator
## Confidence interval
-0.5 * coef(LR)[2] / coef(LR)[3] - 1.96 * sqrt(t(Der) %*% vcovHC(LR) %*% Der)
-0.5 * coef(LR)[2] / coef(LR)[3] + 1.96 * sqrt(t(Der) %*% vcovHC(LR) %*% Der)

## Bootstrap
library(car)
Focus <- function(object) -0.5 * coef(object)[3] / coef(object)[4]
Bst <- Boot(LR, f = Focus, R = 1000, method = "case") # R has to big big.
summary(Bst)
confint(Bst)
Bst$t0
Bst$t # Bootstrap sample based estimates
hist(Bst$t)
