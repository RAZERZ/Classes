#### Test independence using a logit model in 2-way table #### 
Group <- data.frame(success = c(5, 3, 4),
                    failure = c(5, 7, 6),
                    x = c("0", "1", "2"))
Ungroup <- data.frame(y = c(rep(1, 5), rep(0, 5), rep(1, 3), rep(0, 7), rep(1, 4), rep(0, 6)),
                      x = c(rep("0", 10), rep("1", 10), rep("2", 10)))
GroupFit <- glm(cbind(success, failure) ~ x, data = Group, family = binomial())
predict(GroupFit, newdata = data.frame(x = c("0", "1", "2")), type = "response")
summary(GroupFit)
UngroupFit <- glm(y ~ x, data = Ungroup, family = binomial())
predict(UngroupFit, newdata = data.frame(x = c("0", "1", "2")), type = "response")
summary(UngroupFit)

#### Test independence using a logit model in multiway table #### 
## Our data
Data.GLM <- data.frame(study = c("A", "A", "B", "B", "C", "C", "D", "D"),                        
                       treatment = c(1, 0, 1, 0, 1, 0, 1, 0),                        
                       success = c(11, 10, 16, 22, 14, 7, 2, 1),                        
                       fail = c(25, 27, 4, 10, 5, 12, 14, 16)) 
## Fit the homogeneous association model
Logit <- glm(cbind(success, fail) ~ treatment + study, data = Data.GLM, family = binomial) 
## Wald test for conditional independence
summary(Logit) 
## LRT  for conditional independence
Logit2 <- glm(cbind(success, fail) ~ study, family = binomial, data = Data.GLM) 
Logit2$deviance - Logit$deviance 
Logit2$df.residual - Logit$df.residual
qchisq(0.95, Logit2$df.residual - Logit$df.residual)
## Residual deviance as goodness of fit for conditional independence
summary(Logit2)
qchisq(0.95, 4)


#### Test conditional independence using CMH ####
## Our data, but organized it as a data frame
Data.CMH <- data.frame(study = c(rep("A", 11), rep("A", 10), rep("B", 16), rep("B", 22), 
                                 rep("C", 14), rep("C", 7), rep("D", 2), rep("D", 1),
                                 rep("A", 25), rep("A", 27), rep("B", 4), rep("B", 10), 
                                 rep("C", 5), rep("C", 12), rep("D", 14), rep("D", 16)),
                       treatment = c(rep("T", 11), rep("C", 10), rep("T", 16), rep("C", 22), 
                                     rep("T", 14), rep("C", 7), rep("T", 2), rep("C", 1),
                                     rep("T", 25), rep("C", 27), rep("T", 4), rep("C", 10), 
                                     rep("T", 5), rep("C", 12), rep("T", 14), rep("C", 16)),
                       outcome = c(rep("success", 11), rep("success", 10), rep("success", 16), rep("success", 22), 
                                   rep("success", 14), rep("success", 7), rep("success", 2), rep("success", 1),
                                   rep("fail", 25), rep("fail", 27), rep("fail", 4), rep("fail", 10), 
                                   rep("fail", 5), rep("fail", 12), rep("fail", 14), rep("fail", 16)))
mantelhaen.test(Data.CMH$treatment, Data.CMH$outcome, Data.CMH$study) # P-value and CI produce different conclusions
## This is caused by a continuity correction to the CMH test. 
mantelhaen.test(Data.CMH$treatment, Data.CMH$outcome, Data.CMH$study, correct = FALSE)

## We can also use an array
Data <- array(data = c(11, 10, 25, 27,
                       16, 22, 4, 10,
                       14, 7, 5, 12, 
                       2, 1, 14, 16),
              dim = c(2, 2, 4),
              dimnames = list(treatment = c("drug", "placebo"),
                              response = c("success", "fail"),
                              study = c("A", "B", "C", "D")))
mantelhaen.test(Data) 
mantelhaen.test(Data, correct = FALSE)


#### Test homogeneous association #### 
Logit <- glm(cbind(success, fail) ~ treatment + study, data = Data.GLM, family = binomial) 
summary(Logit) 
