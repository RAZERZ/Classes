data(decathlon2, package = "factoextra")
Data <- decathlon2[1 : 23, 1 : 10]
S <- cov(Data) # Sample covariance matrix

#### Eigen decomposition: princomp() does ####
ed <- eigen(S)
ed$values # eigenvalues (largest -> smallest)
ed$vectors # eigenvector (e1, e2, ...., ep)
## Variance explained
ed$values / sum(ed$values)

#### Use built-in function(s) for PCA ####
pca <- prcomp(Data, scale = TRUE, # variance to be 1 or not
              center = TRUE)
## Variance of principal components
pca$sdev ^ 2
eigen(cor(Data))$values # cor(Data) compute the correlation matrix
## Percentage of variance explained
(pca$sdev ^ 2) / sum(pca$sdev ^ 2)
cumsum(pca$sdev ^ 2) / sum(pca$sdev ^ 2)
## (e1, e2, ..): loading matrix
pca$rotation
## Component scores
predict(pca)

#### Another function
pca2 <- princomp(Data, cor = TRUE) # Correlation matrix or cov matrix
pca2$sdev

#### Visulization ####
library(factoextra)
fviz_eig(pca)
fviz_eig(pca, choice = "eigenvalue")

## Plot component score
fviz_pca_ind(pca)

## Plot variables
fviz_pca_var(pca)
