data(HolzingerSwineford1939, package = "lavaan")
Data <- HolzingerSwineford1939[, paste0("x", 1 : 9)]

#### Canonical correlation analysis ####
library(CCA)
X1 <- Data[, 1 : 3] # p <- 3
X2 <- Data[, 4 : 9] # q <- 6
scale(Data, center = TRUE, scale = TRUE) # Ev. col. has 0 mean and 1 variance. 
cca <- cc(X = X1, Y = X2) # covariance scale. Demean data (zero column mean)
cca$cor # Canonical correlations
cca$xcoef # Coef for linear combinations
cca$ycoef
cca$scores$corr.X.xscores
cca$scores$xscores # Values of linear combination U
cca$scores$yscores # Values of linear combination V (3 columns)
lm(cca$scores$yscores ~ cca$scores$xscores)

#### PLS Regression ####
library(pls)
PLS <- plsr(cbind(x1, x2, x3) ~ x4 + x5 + x6 + x7 + x8 + x9,
            ncomp = 6, # maximum circles 
            data = Data, 
            validation = "CV")
summary(PLS)
coef(PLS, ncomp = 3, intercept = TRUE)
plot(RMSEP(PLS)) # Your choice, the lowest one. 

PLS3 <- plsr(cbind(x1, x2, x3) ~ x4 + x5 + x6 + x7 + x8 + x9,
            ncomp = 3, # maximum circles 
            data = Data)
coef(PLS3)
## Predict
x0 <- matrix(rnorm(6), 1, 6)
colnames(x0) <- paste0("x", 4 : 9)
predict(PLS, ncomp = 3, newdata = x0)
