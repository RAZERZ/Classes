
#### Load data ####
data(Mroz, package = "carData")

#### Fit a logistic GLM ####
Logit <- glm(lfp ~ k5 + k618 + age + wc + hc + lwg + inc, # Model
             family = binomial(link = "logit"), # Distribution assumption
             data = Mroz # Name of data set 
             ) 
# binomial(link = "probit")
# binomial(link = "cloglog")
## Summary of the fitted model
summary(Logit)

## Confidence intervals
confint.default(Logit) # Confidence interval in our slide
confint(Logit) # Profile confidence interval

## Prediction
predict(Logit, newdata = data.frame(k5 = 1, k618 = 0, age = 40, wc = "no", hc = "no", lwg = 1.5, inc = 10.0))
predict(Logit, newdata = data.frame(k5 = 1, k618 = 0, age = 40, wc = "no", hc = "no", lwg = 1.5, inc = 10.0), type = "link")
predict(Logit, newdata = data.frame(k5 = 1, k618 = 0, age = 40, wc = "no", hc = "no", lwg = 1.5, inc = 10.0), type = "response")

## Confidence interval for predicted probability
vcov(Logit) # Covariance matrix of beta hat
## Compute the standard error of predicted linear predictor
predict(Logit, newdata = data.frame(k5 = 1, k618 = 0, age = 40, wc = "no", hc = "no", lwg = 1.5, inc = 10.0), type = "link", se.fit = TRUE)
x0 <- c(1, 0, 40, 0, 0, 1.5, 10)
sum(c(1, x0) * coef(Logit))
sqrt(matrix(c(1, x0), nrow = 1) %*% vcov(Logit) %*% matrix(c(1, x0), ncol = 1))
## Obtain confidence interval
Pred <- predict(Logit, newdata = data.frame(k5 = 1, k618 = 0, age = 40, wc = "no", hc = "no", lwg = 1.5, inc = 10.0), type = "link", se.fit = TRUE)
exp(Pred$fit - qnorm(0.975) * Pred$se.fit) / (1 + exp(Pred$fit - qnorm(0.975) * Pred$se.fit))
exp(Pred$fit + qnorm(0.975) * Pred$se.fit) / (1 + exp(Pred$fit + qnorm(0.975) * Pred$se.fit))

## Grouped versus ungrouped data
Group <- data.frame(success = c(5, 3, 4, 5),
                      failure = c(5, 7, 6, 5),
                      x1 = c(0, 0, 1, 1),
                      x2 = c(0, 1, 0, 1))
Ungroup <- data.frame(y = c(rep(1, 5), rep(0, 5), rep(1, 3), rep(0, 7), 
                          rep(1, 4), rep(0, 6), rep(1, 5), rep(0, 5)),
                    x1 = c(rep(0, 10), rep(0, 10), rep(1, 10), rep(1, 10)),
                    x2 = c(rep(0, 10), rep(1, 10), rep(0, 10), rep(1, 10)))

GroupFit <- glm(cbind(success, failure) ~ x1 + x2, data = Group, family = binomial())
UngroupFit <- glm(y ~ x1 + x2, data = Ungroup, family = binomial())
summary(UngroupFit)
summary(GroupFit)

## Information criterion
AIC(Logit)
BIC(Logit)

#### Randomize quantile residual ####
library(DHARMa)
Res <- simulateResiduals(fittedModel = Logit,
                         n = 1e03, 
                         plot = F)
plot(Res)
plotResiduals(Res, form = Mroz$inc)
plotResiduals(Res, form = Mroz$lwg)

