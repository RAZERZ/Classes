## Our data: heart disease data
Heart <- data.frame(disease = rep(c("Y", "N"), each = 16),
                    cholesterol = rep(c("<200", "200-219", "220-259", ">259"), each = 4),
                    bloodpress = rep(c("<127", "127-146", "147-166", ">166"), times = 8),
                    count = c(2, 3, 3, 4,
                              3, 2, 1, 3, 
                              8, 11, 6, 6, 
                              7, 12, 11, 11,
                              117, 121, 47, 22,
                              85, 98, 43, 20,
                              119, 209, 68, 43,
                              67, 99, 46, 33))
Heart$cholesterol <- factor(Heart$cholesterol, levels = c("<200", "200-219", "220-259", ">259"))
Heart$bloodpress <- factor(Heart$bloodpress, levels = c("<127", "127-146", "147-166", ">166"))

#### Approach 1: use glm() function ####
PoiReg <- glm(count ~ disease + cholesterol + bloodpress, data = Heart, family=poisson())

#### Approach 2: loglinear model in MASS ####
library(MASS)
logLin <- loglm(count ~ disease + cholesterol + bloodpress, data = Heart, fitted = TRUE, param = TRUE)
## The parameter estimates from `glm()` and `loglm()` are different. 
coef(PoiReg)
coef(logLin)
## But they are still equivalent, just with different identification rules.
## glm() sets one category to zero for identification
## loglm() uses sum-to-zero for identification
sum(coef(logLin)$disease)
sum(coef(logLin)$cholesterol)
sum(coef(logLin)$bloodpress)
rowSums(coef(logLin)$cholesterol.bloodpress)
colSums(coef(logLin)$cholesterol.bloodpress)

## But they have the same (log) odds
coef(PoiReg)
#### disease
coef(logLin)$disease["Y"] - coef(logLin)$disease["N"]
#### cholesterol
coef(logLin)$cholesterol["200-219"] - coef(logLin)$cholesterol["<200"]
coef(logLin)$cholesterol[">259"] - coef(logLin)$cholesterol["<200"]
#### bloodpress
coef(logLin)$bloodpress["127-146"] - coef(logLin)$bloodpress["<127"]
coef(logLin)$bloodpress[">166"] - coef(logLin)$bloodpress["<127"]

#### Model selection for nested models by difference in deviance ####
logLin1 <- loglm(count ~ disease + cholesterol + bloodpress, data = Heart, fitted = TRUE, param = TRUE)
logLin2 <- loglm(count ~ disease + cholesterol * bloodpress, data = Heart, fitted = TRUE, param = TRUE)
deviance(logLin1) - deviance(logLin2)
logLin1$df - logLin2$df # df of chisq test


#### Inference for conditional odds ratio (2 * 2 * 2 table) ####
## Our data frame
Data.GLM <- data.frame(Alcohol = factor(rep(c("Y", "N"), each = 4), levels = c("N", "Y")),
                       Cigarette = factor(rep(c("Y", "N"), times = 2), levels = c("N", "Y")),
                       Marijuana = factor(rep(c("Y", "N"), each = 2), levels = c("N", "Y")),
                       Count = c(911, 44, 538, 456, 3, 2, 43, 279))
## Fit log-linear model with homogeneous association
PoiReg <- glm(Count ~ Alcohol + Cigarette + Marijuana +
                  Alcohol:Cigarette + Alcohol:Marijuana +
                  Cigarette:Marijuana, data = Data.GLM, family=poisson())
PoiReg # Point estimators are simply the log odds in a 2*2*2 table.
## Compute log conditional AC odds ratio (given Marijuana = "Y"):
##     lambda_YY + lambda_NN - lambda_YN - lambda_NY
coef(PoiReg)[["AlcoholY:CigaretteY"]] + 0 - 0 - 0
## Confidence interval for such log conditional odds ratio under Poisson sampling
confint.default(PoiReg)
a <- matrix(c(0, 0, 0, 0, 1, 0, 0), ncol = 1)
(coef(PoiReg)[["AlcoholY:CigaretteY"]] + 0 - 0 - 0) - qnorm(0.975) * sqrt(t(a) %*% vcov(PoiReg) %*% a)
(coef(PoiReg)[["AlcoholY:CigaretteY"]] + 0 - 0 - 0) + qnorm(0.975) * sqrt(t(a) %*% vcov(PoiReg) %*% a)


#### Connection between logistic model and loglinear model ####
## Heart disease data again
Heart.Logit <- data.frame(with = Heart$count[1 : 16], without = Heart$count[17 : 32],
                          cholesterol = Heart$cholesterol[1 : 16],
                          bloodpress = Heart$bloodpress[1 : 16])
Logit <- glm(cbind(with, without) ~ cholesterol + bloodpress, data = Heart.Logit, family = binomial())
PoiReg <- glm(count ~ disease + cholesterol + bloodpress + 
                  disease : cholesterol + disease : bloodpress + cholesterol : bloodpress,
              data = Heart, family=poisson())
## Same deviance residuals
deviance(PoiReg); deviance(Logit)
## Same parameter estimates
c(coef(Logit)["(Intercept)"], coef(PoiReg)["diseaseY"] - 0)
c(coef(Logit)["cholesterol200-219"], coef(PoiReg)["diseaseY:cholesterol200-219"] - 0)
## Same log odds
NewData <- data.frame(disease = "Y", # Y = disease
                      cholesterol = "200-219", # X
                      bloodpress = "147-166") # Z
predict(Logit, type = "link", newdata = NewData)
predict(PoiReg, type = "link", newdata = NewData) - 
    predict(PoiReg, type = "link", newdata = data.frame(disease = "N", # Y = disease
                                                        cholesterol = "200-219", # X
                                                        bloodpress = "147-166"))



