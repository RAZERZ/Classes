data(Mroz, package = "carData")
## Fit a logistic model
Logit <- glm(lfp ~ lwg + inc, data = Mroz, family = binomial)
## If we have binomial, not bernoulli
glm(cbind(success, failures) ~ x, data, family)
## If we have bernoulli
Index <- which(Mroz$lfp == "yes")
Mroz$lfp01 <- 0
Mroz$lfp01[Index] <- 1
glm(lfp01 ~ lwg + inc, data = Mroz, family = binomial)
## Extract information
summary(Logit)
## Confidence interval for beta
coef(Logit)[3] - qnorm(0.975) * sqrt(vcov(Logit)[3, 3])
coef(Logit)[3] + qnorm(0.975) * sqrt(vcov(Logit)[3, 3])
confint(Logit)
confint.default(Logit)
## Prediction
Newdata <- data.frame(lwg = 0.7654321,
                      inc = 20.213)
predict(Logit, newdata = Newdata)
predict(Logit, newdata = Newdata, type = "response")
predict(Logit, newdata = Newdata, type = "link")
exp(0.03556064) / (1 + exp(0.03556064))
plogis(0.03556064)
## CI to be added.