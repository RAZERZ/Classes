#### Logit, Probit, cloglog Model ####
## Our data frame
Data <- data.frame(Kill = c(6, 13, 18, 28, 52, 53, 61, 60),
                   NotKill = c(53, 47, 44, 28, 11, 6, 1, 0),
                   dose = c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839))
## Fit models
Logit <- glm(cbind(Kill, NotKill) ~ dose, data = Data, 
             family = binomial(link = "logit"))
Probit <- glm(cbind(Kill, NotKill) ~ dose, data = Data, 
              family = binomial(link = "probit"))
Cloglog <- glm(cbind(Kill, NotKill) ~ dose, data = Data, 
               family = binomial(link = "cloglog"))
## Sample proportions versus predicted proportions
Pred <- data.frame(proportion = c(Data$Kill / (Data$Kill + Data$NotKill),
                                  predict(Logit, type = "response"),
                                  predict(Probit, type = "response"),
                                  predict(Cloglog, type = "response")),
                   model = rep(c("observed", "logit", "probit", "cloglog"), each = 8),
                   dose = rep(Data$dose, times = 4))
library(ggplot2)
ggplot(Pred, aes(x = dose, y = proportion, shape = model, color = model)) + geom_point()
## Compare information criteria
c(AIC(Logit), AIC(Probit), AIC(Cloglog))
c(BIC(Logit), BIC(Probit), BIC(Cloglog))


#### Conditional logit model ####
## Generate Fake data
K <- 500
Y <- NULL
X <- NULL
Z <- NULL
Data <- array(data = 0,
              dim = c(2, 2, K))
betaX <- 0.5
set.seed(12345)
for(i in 1 : K){
    betaZ <- ifelse(i == 1, 0, rnorm(1))
    prob1 <- plogis(-0.5 + betaX + betaZ) # First
    prob2 <- plogis(-0.5 + betaZ) # Second
    NewY <- c(rbinom(n = 1, size = 1, prob = prob1),
              rbinom(n = 1, size = 1, prob = prob2))
    Y <- c(Y,
           NewY)
    X <- c(X, c(1, 0)) 
    Z <- c(Z, c(i, i)) 
    NewTable <- matrix(0, 2, 2)
    if(NewY[1] == 1){
        NewTable[1, 1] <- 1
    } else {
        NewTable[1, 2] <- 1
    }
    if(NewY[2] == 1){
        NewTable[2, 1] <- 1
    } else {
        NewTable[2, 2] <- 1
    }
    Data[, , i] <- NewTable
}
## Conditional maximum likelihood
library(survival)
clogit(Y ~ X + strata(Z))
## Compare to MLE
glm(Y ~ X + factor(Z), family = binomial())
