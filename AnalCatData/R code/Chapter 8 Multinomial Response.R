## Our data
data(housing, package = "MASS")
## But we need to change the layout 
library(tidyr)
Table <- pivot_wider(data = housing, names_from = Sat, values_from = Freq) 
print(Table, n = 24)

#### Baseline Category Logit Model: By VGAM package ####
library(VGAM)
BCLM <- vglm(cbind(Low, Medium, High) ~ Infl + Type + Cont, data = Table, 
             family = multinomial()) 
# multinomial(parallel = FALSE, refLevel = 3) default is last level in cbind(*, *, ..., *, reference level)
summary(BCLM) # Hauck-Donner effect means that the predicted probability is close to 0 or 1. No effect means that Wald test of beta is plausible.
qchisq(0.95, 34) # Fits the data well.
residuals(BCLM, type = "pearson") # Residuals
confint(BCLM) # Confidence interval of the parameters
predict(BCLM, type = "response") # Fitted probabilities
## Saturated model with perfect fit.
Satu <- vglm(cbind(Low, Medium, High) ~ Infl * Type * Cont, data = Table, 
             family = multinomial())

#### Baseline Category Logit Model: By nnet package ####
library(nnet)
NNET <- multinom(cbind(Low, Medium, High) ~ Infl + Type + Cont, data = Table) # First category as baseline.
NNET
BCLM1 <- vglm(cbind(Low, Medium, High) ~ Infl + Type + Cont, data = Table, 
              family = multinomial(refLevel = 1))
predict(NNET, type = "probs")
cbind(residuals(NNET), residuals(BCLM1, type = "response"))


####  Fit a cumulative link model: By VGAM package ####
POM <- vglm(cbind(Low, Medium, High) ~ Infl + Type + Cont, data = Table, 
            family = cumulative(link = 'logitlink', parallel = TRUE))
summary(POM)
## Test the hypothesis of proportional odds
Alt <- vglm(cbind(Low, Medium, High) ~ Infl + Type + Cont, data = Table, 
            family = cumulative(link = 'logitlink', parallel = FALSE))
deviance(POM) - deviance(Alt)
qchisq(0.95, 40 - 34)


####  Fit a cumulative link model: By MASS package ####
## Be Careful!! Two packages are similar, but the signs are different. Check the help file of polr(). It is alpha_k - eta
library(MASS)
POMpoly <- polr(factor(Sat, levels = c("Low", "Medium", "High")) ~ Infl + Type + Cont, data = housing, weights = Freq,
                method = "logistic")
summary(POMpoly)


#### Test conditional independence by baseline category model ####
Data <- data.frame(Ed = c("H", "H", "H", "C", "C", "C"),
                   Religion = c("F", "M", "L", "F", "M", "L"),
                   Agree = c(6, 8, 11, 4, 21, 22),
                   Neutral = c(2, 3, 5, 2, 3, 4),
                   Disagree = c(10, 9, 6, 11, 5, 1),
                   score = c(1, 2, 3, 1, 2, 3))
library(VGAM)
BCLM <- vglm(cbind(Agree, Neutral, Disagree) ~ Ed + Religion, data = Data, 
             family = multinomial()) 
BCLMind <- vglm(cbind(Agree, Neutral, Disagree) ~ Ed, data = Data, 
                family = multinomial()) 
deviance(BCLMind) - deviance(BCLM)
qchisq(0.95, 4)


#### Test conditional independence by cumulative logits model ####
POM <- vglm(cbind(Agree, Neutral, Disagree) ~ Ed + Religion, data = Data, 
            family = cumulative(link = 'logitlink', parallel = TRUE))
POMind <- vglm(cbind(Agree, Neutral, Disagree) ~ Ed, data = Data, 
               family = cumulative(link = 'logitlink', parallel = TRUE))
deviance(POMind) - deviance(POM)
qchisq(0.95, 2)


#### Testing conditional independence using CMH test ####
Data <- array(data = c(6, 8, 11, 2, 3, 5, 10, 9, 6,
                       4, 21, 22, 2, 3, 4, 11, 5, 1),
              dim = c(3, 3, 2),
              dimnames = list(Religion = c("F", "M", "L"),
                              Opinion = c("Agree", "Neutral", "Disagree"),
                              Education = c("High school", "College")))
mantelhaen.test(Data) 
