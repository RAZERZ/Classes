#### 4 point data ####
Data <- matrix(c(0, 1, 
                 1, 2, 
                 2, 4,
                 3, 2), 4, 2, byrow = TRUE)
dist(Data, method = "euclidean")
## single linkage
Single <- hclust(dist(Data), method = "single")
plot(Single)
## Complete linkage
Complete <- hclust(dist(Data), method = "complete")
plot(Complete)
# method = "average"
# method = "ward.D2"

#### Kmeans ####
load("C:/Users/shaji948/Box/Teaching/Math Department/Multivariate analysis 1MS003/2023HT/Block IV/Chapter 11/TwoClass.RData")
Data
plot(Data[, "V1"], Data[, "V2"], col = ifelse(Data$Label == "Y", 1, 2), pch = 15)
K1 <- kmeans(x = scale(Data[, c("V1", "V2")]), centers = 2)
K1$cluster
plot(Data[, "V1"], Data[, "V2"], col = K1$cluster, pch = 15)
kmeans(x = Data[, c("V1", "V2")], centers = 4)$centers
kmeans(x = Data[, c("V1", "V2")], centers = 4)$centers
## Increase the number of random trials: nstart
K4 <- kmeans(x = scale(Data[, c("V1", "V2")]), centers = 4, nstart = 100)
plot(Data[, "V1"], Data[, "V2"], col = K4$cluster, pch = 15)

#### Determine K ####
Candidate <- 2 : 12
## CH index
K <- list()
CHval <- rep(0, 5)
for(k in Candidate){
    K[[k - 1]] <- kmeans(x = scale(Data[, c("V1", "V2")]), 
                         centers = k, nstart = 100)
    CHval[k - 1] <- (K[[k - 1]]$betweenss / (k - 1)) / (K[[k - 1]]$tot.withinss / (500 - k))
}
plot(Candidate, CHval) # Maximum value
which.max(CHval)
## GAP
library(factoextra)
library(cluster)
gap_stat <- cluster::clusGap(x = Data[, c("V1", "V2")],
                             kmeans,
                             K.max = 12,
                             B = 100, # Number of replications. For illustration
                             iter.max = 10 # Should be large
                             )
fviz_gap_stat(gap_stat)
## Silhouette statistic
fviz_nbclust(Data[, c("V1", "V2")], kmeans, method = "silhouette")

#### kernel kmeans ####
## Try several times
library(kernlab)
KerKmeans <- kkmeans(scale(Data[, c("V1", "V2")]),
                     centers = 2,
                     kernel = "rbfdot", # Gaussian kernel 
                     kpar = "automatic") # kpar = list(sigma = 1)
plot(Data[, "V1"], Data[, "V2"], col = KerKmeans@.Data, pch = 15)


#### Gaussian mixture analysis ####
library(mclust)
GMA <- Mclust(data = Data[, c("V1", "V2")], G = 2 : 4,
              modelNames = "VVV" # What assumptions you have for covariance matrix
                  )
GMA$classification
GMA$parameters
GMA$z # Posterior probability
GMA$BIC # -1 * BIC
