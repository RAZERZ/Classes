library(glmnet)
library(caret)
library(pROC)
## We only use a specific type of Gaussian kernel
## Y must be 1 and 0
RKHS <- function(x, y, nfolds, sigma2, measure = "class"){
    
    ## Sample size
    n <- nrow(x)
    ## Number of covariates
    p <- ncol(x)
    ## Standardize covariates, whole data set
    mean.x <- colMeans(x)
    sd.x <- apply(x, 2, function(z) sd(z))
    std.x <- apply(x, 2, function(z) scale(z, center = TRUE, scale = TRUE))
    ## Fit the model using all data
    kmat <- matrix(NA, n, n)
    for(i in 1 : n){
        for(j in i : n){
            kmat[i, j] <- exp(-sum((std.x[i, ] - std.x[j, ]) ^ 2) / sigma2) 
            kmat[j, i] <- kmat[i, j] # Kernel matrix is symmetric
        }
    }
    ridge <- glmnet(kmat, y, family = "binomial", alpha = 0)
    lambda <- ridge$lambda
    ## Create folds
    set.seed(12345)
    folds <- caret::createFolds(1 : n, nfolds)
    ## Perform cross validation to choose the tuning parameter of the ridge term
    loss <- matrix(NA, nfolds, length(lambda)) # store sum of squares for model comparison
    for(k in 1 : nfolds){
        
        ## Training set
        train.y <- y[-folds[[k]]]
        train.x <- x[-folds[[k]],]
        n.k <- nrow(train.x)
        ## Standardize the covariates in the training set
        mean.x.train <- colMeans(train.x)
        sd.x.train <- apply(train.x, 2, function(z) sd(z))
        train.x <- apply(train.x, 2, function(z) scale(z, center = TRUE, scale = TRUE))
        ## Create kernel matrix only using the standardized training set
        train.kmat <- matrix(NA, n.k, n.k)
        for(i in 1 : n.k){
            for(j in i : n.k){
                train.kmat[i, j] <- exp(-sum((train.x[i, ] - train.x[j, ]) ^ 2) / sigma2)
                train.kmat[j, i] <- train.kmat[i, j] # Kernel matrix is symmetric
            }
        }
        ## We use glmnet to fit the model
        ridge.CV <- glmnet::glmnet(train.kmat, train.y, family = "binomial", alpha = 0, lambda = lambda)
        
        ## Extract validation set and standardize the validation x
        valid.Y <- y[folds[[k]]]
        valid.X <- x[folds[[k]],]
        for(j in 1 : p){
            valid.X[, j] <- (x[folds[[k]], j] - mean.x.train[j]) / sd.x.train[j]
        }
        ## To make prediction, we need to evaluate the kernel function
        valid.kmat <- matrix(NA, nrow(valid.X), n.k)
        for(i in 1 : nrow(valid.X)){
            for(j in 1 : n.k){
                valid.kmat[i, j] <- exp(-sum((valid.X[i, ] - train.x[j, ]) ^ 2) / sigma2)
            }
        }
        ## Extract predicted probability
        pred.prob <- predict(ridge.CV, newx = valid.kmat, type = "response", s = lambda)
        ## Compute the measure
        if(measure == "L2"){
            diff <- matrix(valid.Y, nrow = nrow(valid.X), ncol = length(lambda), byrow = FALSE) - pred.prob
            loss[k, ] <- -1.0 * colSums(diff ^ 2)
        } else if(measure == "AUC"){
            for(j in 1 : length(lambda)){
                roc <- pROC::roc(response = valid.Y,
                                 predictor = pred.prob[, j], levels = c(0, 1), direction = "<")
                loss[k, j] <- roc$auc
            }
        } else {
            pred.class <- ifelse(pred.prob >= 0.5, "1", "0")
            obs.class <- ifelse(valid.Y == 1, "1", "0")
            for(j in 1 : length(lambda)){
                cm <- caret::confusionMatrix(data = factor(pred.class[, j], levels = c("1", "0")),
                                             reference = factor(valid.Y, levels = c("1", "0")),
                                             mode = "everything", positive = "1")
                if(measure == "F1"){
                    loss[k, j] <- cm$byClass["F1"]
                } else if(measure == "class"){
                    loss[k, j] <- sum(diag(cm$table)) / sum(cm$table)
                }
            }
        }
    }
    ## Choose the optimal tuning parameter
    lambda.opt <- lambda[which.max(colMeans(loss))]
    ## Return result
    list(rkhs = ridge,
         lambda = lambda,
         sigma2 = sigma2,
         lambda.opt = lambda.opt,
         x = x, 
         loss = loss)
    
}
predictprob <- function(object, newx, lambda = NULL){
    ## Standardize data
    mean.x <- colMeans(object$x)
    sd.x <- apply(object$x, 2, function(z) sd(z))
    std.x <- scale(object$x, center = TRUE, scale = TRUE)
    std.newx <- newx
    for(j in 1 : ncol(newx)){
        std.newx[, j] <- (std.newx[, j] - mean.x[j]) / sd.x[j]
    }
    ## Make prediction
    kmatnew <- matrix(NA, nrow(newx), nrow(object$x))
    for(i in 1 : nrow(newx)){
        for(j in 1 : nrow(object$x)){
            kmatnew[i, j] = exp(-0.5 * sum((std.newx[i, ] - std.x[j, ]) ^ 2) / object$sigma2) ;
        }
    }
    if(is.null(lambda) == TRUE){
        lambda <- object$lambda.opt
    } 
    predict(object$rkhs, s = lambda, type = "response", newx = kmatnew)
}
#Y <- rep(1.0, nrow(Data)); Y[Data[, 1] == "N"] <- 0.0
#LR <- RKHS(x = as.matrix(Data[, 2 : 3]), y = Y, nfolds = 10, sigma2 = 1.0, measure = "L2")
#predictprob(object = LR, newx = Data[1 : 5, 2 : 3], lambda = LR$lambda)
