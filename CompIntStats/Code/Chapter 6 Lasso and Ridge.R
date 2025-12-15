data("USairpollution", package = "HSAUR3")
library(glmnet)
#### With the Lasso penalty
x <- as.matrix(USairpollution[, c("temp", "manu", "popul", "wind", "precip", "predays")])
y <- USairpollution[, "SO2"]
Lasso <- glmnet(x = x, y = y, family = "gaussian", alpha = 1)
#### By default, standardization is TRUE.
Lasso
#### It starts with nothing in the model (large enough lambda) and gradually improves the current estimate.
#### Plot the solution path
#### Numbers above plot are the number of nonzero slopes.
par( mfrow = c(1,2) )
plot( Lasso, label = TRUE )
plot( Lasso, xvar = "lambda", label = TRUE )
#### Extract the coefficient value.
######## The numbers on top are the number of regressors included in the model.
predict( Lasso, type = "coefficients", s = 2 )
#### Lambda=2 abline( v = log(2), lwd = 2, col = 1, lty = 2 )
#### Extract the fitted value predict(Lasso, newx = x, s = 2)
#### Choose tuning parameter by cross validation
#### But it is random split, so you may get different results
Lasso.CV <- cv.glmnet(x, y, family = "gaussian", alpha = 1, nfolds = 10)
plot(Lasso.CV)
Lasso.CV$lambda.min
Lasso.CV$lambda.1se
######## Extract the coefficients
predict(Lasso.CV, type = "coefficients", s = Lasso.CV$lambda.min)
####### If we have a new lambda, same as fitting the whole path again by adding that lambda.
cbind( predict( Lasso.CV, type = "coefficients", s = 2 ),
       predict( Lasso.CV, type = "coefficients", s = 2, exact = TRUE, x = x, y = y ) )
######## The following way of extracting the coefficients is actually not recommended.
Lasso <- glmnet( x, y, family = "gaussian", alpha = 1, lambda = Lasso.CV$lambda.min )
predict( Lasso, type = "coefficients" )
cbind( predict( Lasso, type = "coefficients" ),
       predict( Lasso.CV, type = "coefficients", s = Lasso.CV$lambda.min ))