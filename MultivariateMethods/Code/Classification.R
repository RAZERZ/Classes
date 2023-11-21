load("TwoClass.RData")
library(ggplot2)
head(Data)
Data$Label
ggplot(data = Data, aes(V1, V2, color = Label)) + geom_point()

#### Naive Bayes: Gaussian discrminant analysis ####
N <- 500
Z <- rep(0, N)
Z[Data$Label == "Y"] <- 1
## Compute MLE
phi <- sum(Z) / N
Mu1 <- Mu2 <- rep(0, 2)
for(j in 1 : 500){
  Xj <- c(Data[j, "V1"], Data[j, "V2"])
  Mu1 <- Mu1 + Z[j] * Xj
  Mu2 <- Mu2 + (1 - Z[j]) * Xj
}
Mu1 <- Mu1 / sum(Z)
Mu2 <- Mu2 / sum(1 - Z)
Sig <- matrix(0, 2, 2)
for(j in 1 : N){
  Xj <- c(Data[j, "V1"], Data[j, "V2"])
  Sig <- Sig + Z[j] * matrix(Xj - Mu1, ncol = 1) %*% matrix(Xj - Mu1, nrow = 1) +
    (1 - Z[j]) * matrix(Xj - Mu2, ncol = 1) %*% matrix(Xj - Mu2, nrow = 1)
    
}
Sig <- Sig / N
## Compute posterior probability
x0 <- c(-1, 2)
p1 <- phi * mvtnorm::dmvnorm(x = x0, mean = Mu1, sigma = Sig)
p2 <- (1 - phi) * mvtnorm::dmvnorm(x = x0, mean = Mu2, sigma = Sig)
p1 / (p1 + p2) # Posterior prob
p2 / (p1 + p2)
## Our decision boundary
V1grid <- seq(-3, 5, length.out = 50)
V2grid <- seq(-6, 5, length.out = 50)
Vgrid <- expand.grid(V1grid, V2grid)
PredGrid <- rep(0, nrow(Vgrid))
for(i in 1 : nrow(Vgrid)){
  x0 <- Vgrid[i, ]
  p1 <- phi * mvtnorm::dmvnorm(x = x0, mean = Mu1, sigma = Sig)
  p2 <- (1 - phi) * mvtnorm::dmvnorm(x = x0, mean = Mu2, sigma = Sig)
  if(p1 > p2){
    PredGrid[i] <- 1
  } else {
    PredGrid[i] <- 0
  }
}
plot(Vgrid, col = PredGrid, pch = 16)
## Nicer way of decision boundary
Decision.NB <- rep(0, nrow(Vgrid))
for(i in 1 : nrow(Vgrid)){
  x0 <- Vgrid[i, ]
  p1 <- phi * mvtnorm::dmvnorm(x = x0, mean = Mu1, sigma = Sig)
  p2 <- (1 - phi) * mvtnorm::dmvnorm(x = x0, mean = Mu2, sigma = Sig)
  Decision.NB[i] <- p1 / p2 - 1
}
plot(Data[, "V1"], Data[, "V2"], col = Z + 1, pch = 16)
contour(x = V1grid, y = V2grid, z = matrix(Decision.NB, 50, 50),
        level = 0, add = TRUE, col = "green", lwd = 2)

#### Fisher's LDA ####
library(MASS)
LDA <- lda(x = Data[, c("V1", "V2")], 
           grouping = Data$Label)
x0DF <- data.frame(V1 = -1, 
                   V2 = 2)
predict(LDA, newdata = x0DF)
## Identical to our Naive Bayes
LDA <- lda(x = Data[, c("V1", "V2")], 
           grouping = Data$Label, 
           method = "mle")
predict(LDA, newdata = x0DF)$posterior
p1 / (p1 + p2)
## Identical to LDA in our slides
LDA <- lda(x = Data[, c("V1", "V2")], 
           grouping = Data$Label, 
           method = "mle", prior = c(0.5, 0.5))

#### QDA ####
QDA <- qda(x = Data[, c("V1", "V2")], 
           grouping = Data$Label)
predict(QDA, newdata = x0DF)
## Decision boundary
Decision <- predict(QDA, newdata = cbind(Vgrid[, 1], Vgrid[, 2]))$posterior
plot(Data[, "V1"], Data[, "V2"], col = Z + 1, pch = 16)
contour(x = V1grid, y = V2grid, z = matrix(Decision, 50, 50),
        level = 0.5, add = TRUE, col = "blue", lwd = 2)

#### Logistic regression approach ####
Data$Y <- Z
Logit <- glm(Y ~ V1 + V2, data = Data, family = binomial())
## Predict new observation
x0df <- data.frame(V1 = -1,
                   V2 = 2)
predict(Logit, newdata = x0df, type ="response") # probability
predict(Logit, newdata = x0df, type ="link") # default
## Decision
colnames(Vgrid) <- c("V1", "V2")
Decision <- predict(Logit, newdata = Vgrid, type = "link")
plot(Data[, "V1"], Data[, "V2"], col = Z + 1, pch = 16)
contour(x = V1grid, y = V2grid, z = matrix(Decision, 50, 50),
        level = 0, add = TRUE, col = "blue", lwd = 2)
## Nonlinear terms
Data$V1sq <- Data$V1 * Data$V1
Data$V2sq <- Data$V2 * Data$V2
Data$V1V2 <- Data$V1 * Data$V2
Data$V1cube <- Data$V1 * Data$V1 * Data$V1
Logitnonl <- glm(Y ~ V1 + V2 + V1sq + V2sq + V1V2 + V1cube, 
             data = Data, family = binomial())
Vgridnon <- cbind(Vgrid, Vgrid[, 1] * Vgrid[, 1],
                  Vgrid[, 2] * Vgrid[, 2],
                  Vgrid[, 1] * Vgrid[, 2],
                  Vgrid[, 1] ^ 3)
colnames(Vgridnon) <- c("V1", "V2", "V1sq", "V2sq", "V1V2", "V1cube")
Decision.Non <- predict(Logitnonl, newdata = Vgridnon, type = "link")
contour(x = V1grid, y = V2grid, z = matrix(Decision.Non, 50, 50),
        level = 0, add = TRUE, col = "magenta", lwd = 2)
