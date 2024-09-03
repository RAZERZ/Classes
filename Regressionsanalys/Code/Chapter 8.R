load("C:/Users/shaji948/Box/Teaching/Math Department/Regression analysis 1MS555/Data/Temperature.RData")
LR <- lm(Temp ~ Lat + Lon, data = Data)
## hat values
## In this example. H: 56 * 56
hatvalues(LR) # 56 hat values, diagonal values of H
## Residuals
resid(LR)
residuals(LR)
residuals(LR, "pearson")
rstandard(LR)
## Properties of residuals
sum(resid(LR))
cor(resid(LR), predict(LR))

## Residual plots
par(mfrow = c(2, 2))
plot(LR)

## Check function form in regression
library(car)
residualPlots(LR)
marginalModelPlots(LR)
## Check normality
qqPlot(LR)
## Check same-variance assumption
par(mfrow = c(2, 2))
plot(LR)
## Influence analysis
influenceIndexPlot(LR)

## DFBeta
dfbetaPlots(LR)

## Test outliers
Data$U <- rep(0, 56)
Data$U[c(41)] <- 1
LR <- lm(Temp ~ Lat + Lon + U, data = Data)
summary(LR)
Data$U[c(52)] <- 1
LR <- lm(Temp ~ Lat + Lon + U, data = Data)
summary(LR)

## Added variable plot
data("USairpollution", package = "HSAUR3")
LR1 <- lm(SO2 ~ manu + popul + wind, data = USairpollution)
LR2 <- lm(temp ~ manu + popul + wind, data = USairpollution)
plot(resid(LR1), resid(LR2))
library(ggplot2)
DF <- data.frame(rY = resid(LR1), 
                 rZ = resid(LR2))
ggplot(DF, aes(rZ, rY)) + geom_point() + geom_smooth(method = "lm")

## Box-Cox transformation
LR <- lm(SO2 ~ temp + manu + popul + wind + precip, data = USairpollution)
par(mfrow = c(2, 2))
plot(LR)
library(MASS)
BC <- boxcox(LR)
Lambda <- BC$x[which.max(BC$y)]
USairpollution$NewSO2 <- (USairpollution$SO2 ^ Lambda - 1) / Lambda 
LR <- lm(NewSO2 ~ temp + manu + popul + wind + precip, data = USairpollution)
summary(LR)

## Bootstrap
LR <- lm(SO2 ~ temp + manu + popul + wind + precip, data = USairpollution)
Bootstrap <- Boot(LR, R = 1000, method = "residual" # or "case"
                  ) # R has to be large enough
summary(Bootstrap)
confint(Bootstrap) # Bootstrap confidence intervals
confint(LR)
## If we are interested in a function of beta, instead of beta itself
Bootstrap <- Boot(LR, R = 1000, method = "residual", # or "case"
                  f = function(object) summary(object)$fstatistic["value"]
)
summary(Bootstrap)
confint(Bootstrap)
