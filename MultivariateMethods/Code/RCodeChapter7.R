data(HolzingerSwineford1939, package = "lavaan")
Data <- HolzingerSwineford1939
head(Data)
#### Classic regression ####
LM <- lm(x1 ~ x4 + x5 + x6 + x7 + x8 + x9, data = Data) # - 1
LM
Z <- cbind(1, as.matrix(Data[, c("x4", "x5", "x6", "x7", "x8", "x9")]))
Y <- as.matrix(Data[, "x1"])
Betahat <- solve(t(Z) %*% Z) %*% t(Z) %*% Y # OLS estimator
## predicted value
Z %*% solve(t(Z) %*% Z) %*% t(Z) %*% Y # Z %*% Betahat
predict(LM)
## Residual
residuals(LM)
Y - Z %*% Betahat
## Details
summary(LM)
## Confidence interval
confint.default(LM)
confint.default(LM, level = 1 - 0.05 / p) # p number of joint statements
## Confidence intervals for regression function (expectation)
z0 <- matrix(rnorm(6), 1, 6)
colnames(z0) <- c("x4", "x5", "x6", "x7", "x8", "x9")
predict(LM, newdata = data.frame(z0), interval = "confidence", 
        level = 0.95) # E(Y)
## Prediction interval
predict(LM, newdata = data.frame(z0), interval = "prediction", 
        level = 0.95) # Y

#### Multivariate regression ####
MLM <- lm(cbind(x1, x2, x3) ~ x4 + x5 + x6 + x7 + x8 + x9, data = Data)
MLM
LM
summary(MLM)

#### Back to MANOVA ####
data(iris)
head(iris)
ANOVA <- aov(Sepal.Length ~ Species, data = iris)
summary(ANOVA)
LMiris <- lm(Sepal.Length ~ Species, data = iris)
summary(LMiris)
