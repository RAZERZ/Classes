## 5.4
library(alr4)
data(MinnLand)
#### Boxplot
boxplot(log(acrePrice) ~ factor(year), data = MinnLand)
#### Linear regression
LR <- lm(log(acrePrice) ~ year, data = MinnLand)
summary(LR)
LR <- lm(log(acrePrice) ~ factor(year), data = MinnLand)
summary(LR)

## 5.8
data(cakes)
#### Approach 1: create quadratic terms in lm()
LR <- lm(Y ~ X1 + X2 + I(X1 ^ 2) + I(X2 ^ 2) + I(X1 * X2), data = cakes)
#### Approach 2: create quadratic terms before lm()
cakes$X1sq <- cakes$X1 ^ 2
cakes$X2sq <- cakes$X2 ^ 2
cakes$X1X2 <- cakes$X1 * cakes$X2
## A little more modern way
library(dplyr)
cakes <- cakes %>% mutate(X1sq = X1 ^ 2,
                          X2sq = X2 ^ 2,
                          X1X2 = X1 * X2)
head(cakes)
LR <- lm(Y ~ X1 + X2 + X1sq + X2sq + X1X2, data = cakes)
## Interactions
LR <- lm(Y ~ X1 + X2 + block + X1sq + X2sq + X1X2, data = cakes)
LR <- lm(Y ~ X1 + X2 + block + X1:block + X2:block + X1sq + X2sq + X1X2, 
         data = cakes)
LR <- lm(Y ~ X1 + X2 + block  + X1sq + X2sq + X1X2 + 
             X1:block + X2:block + X1sq:block + X2sq:block + X1X2:block, 
         data = cakes)
## block = 0, 1
cakes$block
## Treat block as numeric
cakes.numeric <- cakes
cakes.numeric$block <- as.numeric(cakes.numeric$block) - 1 #(0, 1) coding
LR1 <- lm(Y ~ X1 + X2 + block, data = cakes)
LR2 <- lm(Y ~ X1 + X2 + block, data = cakes.numeric)

## 5.14
library(ggplot2)
data(BGSall)
BGSall$Sex <- factor(BGSall$Sex)
ggplot(BGSall, aes(x = HT9, y = HT18, color = Sex, shape = Sex)) + 
    geom_point() + geom_smooth(method = "lm")
LR <- lm(HT18 ~ HT9 * Sex, data = BGSall)
summary(LR)


## 6.3
data(UN11)
LR0 <- lm(lifeExpF ~ group, data = UN11)
LR1 <- lm(lifeExpF ~ group + log(ppgdp), data = UN11)
#### Own F test
RSS1 <- sum(resid(LR1) ^ 2)
RSS0 <- sum(resid(LR0) ^ 2)
Num <- (RSS0 - RSS1) / 1
Den <- RSS1 / (199 - 4)
Num / Den # F value
qf(0.95, 1, 199 - 4) # Critical value
#### built-in function in R to perform F test to compare 
####   two nested linear regression models
anova(LR0, LR1)
anova(LR1, LR0)

## 6.5
LR <- lm(lifeExpF ~ group + log(ppgdp), data = UN11)
summary(LR)
#### Change the reference level
LR1 <- lm(lifeExpF ~ relevel(group, ref = "other") + log(ppgdp), data = UN11)
summary(LR1)
#### t-test: Test one linear combination
## other - africa = 0
a <- matrix(c(0, 1, -1, 0), ncol = 1)
X <- model.matrix(LR)
Num <- t(a) %*% coef(LR) / sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
Den <- sqrt(sum(resid(LR) ^ 2) / (199 - 4))
Num / Den # t statistic
qt(0.95, 199 - 4)
#### F-test for general linear combination
L <- matrix(c(0, 1, -1, 0), nrow = 1)
Beta0 <- coef(LR) - solve(t(X) %*% X) %*% t(L) %*% solve(L %*% solve(t(X) %*% X) %*% t(L)) %*% L %*% coef(LR)
RSS1 <- sum(resid(LR1) ^ 2)
RSS0 <- sum((UN11$lifeExpF - X %*% Beta0) ^ 2)
Num <- (RSS0 - RSS1) / 1
Den <- RSS1 / (199 - 4)
Num / Den # F value
qf(0.95, 1, 199 - 4) # Critical value
#### built-in function
library(car)
linearHypothesis(LR, "groupother - groupafrica")

## 6.7
data("fuel2001")
fuel2001 <- fuel2001 %>% mutate(Fuel = FuelC / Pop,
                                Dlic = Drivers / Pop)
LR1 <- lm(Fuel ~ Tax + Dlic + Income + log(Miles), data = fuel2001)
LR2 <- lm(Fuel ~ log(Miles) + Income + Dlic + Tax, data = fuel2001)
Anova(LR1)
Anova(LR2)
summary(LR1)
summary(LR2)

## 7.13
data("Transact")
LR <- lm(time ~ t1 + t2, data = Transact)
summary(LR)
Der <- matrix(c(0, 1.0 / coef(LR)[3], 
                -1.0 * coef(LR)[2] / coef(LR)[3] ^ 2), nrow = 3, ncol = 1)
t(Der) %*% vcov(LR) %*% Der # vcov(LR) = V(beta hat)
t(Der) %*% vcovHC(LR) %*% Der # vcov(LR) = V(beta hat), but sandwich estimator
## Confidence interval, 95%
coef(LR)[2] / coef(LR)[3] - 1.96 * sqrt(t(Der) %*% vcovHC(LR) %*% Der)
coef(LR)[2] / coef(LR)[3] + 1.96 * sqrt(t(Der) %*% vcovHC(LR) %*% Der)

## Bootstrap
library(car)
Focus <- function(object) coef(object)[2] / coef(object)[3]
Bst <- Boot(LR, f = Focus, R = 1000, method = "case") # R has to big big.
confint(Bst)
