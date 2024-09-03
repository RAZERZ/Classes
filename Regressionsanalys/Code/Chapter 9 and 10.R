data("USairpollution", package = "HSAUR3")
LR <- lm(SO2 ~ manu + popul + wind, data = USairpollution)
AIC(LR)
BIC(LR)
LR2 <- lm(SO2 ~ manu + popul + wind + temp + precip, data = USairpollution)
AIC(LR2)

## Step AIC
library(MASS)
LR <- lm(SO2 ~ manu + popul + wind, data = USairpollution)
stepAIC(LR, direction = "backward", trace = TRUE, 
        scope = list(lower = ~ manu))
LR <- lm(SO2 ~ temp, data = USairpollution)
stepAIC(LR, direction = "forward", trace = TRUE,
        scope = list(upper = ~ temp + manu + popul + wind))
LR <- lm(SO2 ~ manu + popul + wind, data = USairpollution)
stepAIC(LR, direction = "both", trace = TRUE, 
        scope = list(lower = ~ manu,
                     upper = ~ temp + manu + popul + wind))

## Cross validation
library(caret)
Control <- trainControl(method = "cv", number = 10, # K-fold: K = 10
                        returnResamp = "all",
                        savePredictions = TRUE)
Control <- trainControl(method = "LOOCV")
set.seed(123) # Set random seeds
Model1 <- train(SO2 ~ manu + popul + wind, data = USairpollution,
                method = "lm", trControl = Control)
set.seed(123) # Set random seeds
Model2 <- train(SO2 ~ manu + popul + wind + temp + precip, data = USairpollution,
                method = "lm", trControl = Control)
Model1$pred
Model2$pred

## CV on my own
library(caret)
Fold <- createFolds(y = 1 : 41, k = 10) # 10 -fold CV
MSE1 <- MSE2 <- 0
for(k in 1 : 10){
    ## training set
    Train <- USairpollution[-Fold[[k]], ]
    ## validation set
    Test <- USairpollution[Fold[[k]], ]
    ## Fit different models
    Model1 <- lm(SO2 ~ manu + popul + wind, data = Train)
    Model2 <- lm(SO2 ~ manu + popul + wind + temp + precip, data = Train)
    ## Predict 
    MSE1 <- MSE1 + sum((predict(Model1, newdata = Test) - Test$SO2) ^ 2)
    MSE2 <- MSE2 + sum((predict(Model2, newdata = Test) - Test$SO2) ^ 2)
}
sqrt(MSE1)
sqrt(MSE2)
## Re-fit the model using all data
lm(SO2 ~ manu + popul + wind + temp + precip, data = USairpollution)

## Lasso/Ridge
library(glmnet)
Lasso <- cv.glmnet(x = as.matrix(USairpollution[, c("manu", "popul", "wind", "temp")]), 
                y = USairpollution$SO2, 
                family = "gaussian", alpha = 1, 
                nlambda = 100)
Lasso$lambda
Lasso$lambda.min # Best lambda
Lasso$lambda.1se
## Extract coefficients
predict(Lasso, s = 0.03879168, type = "coefficient")
predict(Lasso, s = Lasso$lambda.1se, type = "coefficient")
plot(Lasso)
## Make prediction
predict(Lasso, s = 0.03879168, type = "response", newx = as.matrix(USairpollution[, c("manu", "popul", "wind", "temp")]))
