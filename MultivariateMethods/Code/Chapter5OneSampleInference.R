#### Hotelling's T2 ####
## Test mu0 = (0, 0)
n <- 1000
p <- 2
Mu0 <- c(0, 0)
Xbar <- colMeans(Normal)
S <- cov(Normal) # 1 / (n - 1)
T2 <- n * matrix(Xbar - Mu0, nrow = 1) %*% solve(S) %*% matrix(Xbar - Mu0, ncol = 1)
T2
(n - 1) * p / (n - p) * qf(p = 0.95, df1 = p, df2 = n - p) # critical value
library(DescTools)
HotellingsT2Test(x = Normal, mu = Mu0, test = "f")
(n - p) * T2 / ((n - 1) * p)

#### Wilks lambda ####
## Compute the denominator
S0 <- matrix(0, 2, 2)
for(j in 1 : n){
    S0 <- S0 + matrix(Normal[j, ] - Mu0, ncol = 1) %*% 
        matrix(Normal[j, ] - Mu0, nrow = 1)
}
Lambda <- (det((n - 1) * S) / det(S0)) ^ (n / 2)
WilksLambda <- det((n - 1) * S) / det(S0)
(1 + T2 / (n - 1)) ^ (-1)

#### Confidence region ####
Mu1Loop <- seq(-0.2, 0.2, length.out = 100)
Mu2Loop <- seq(0.2, 0.7, length.out = 100)
plot(NULL, xlim = range(Mu1Loop), ylim = range(Mu2Loop), xlab = "Mu1", ylab = "Mu2")
for(i in 1 : 100){
    for(j in 1 : 100){
        Mu1 <- Mu1Loop[i]
        Mu2 <- Mu2Loop[j]
        Mu0 <- c(Mu1, Mu2)
        ## Compute T2
        T2 <- n * matrix(Xbar - Mu0, nrow = 1) %*% solve(S) %*% matrix(Xbar - Mu0, ncol = 1)
        ## Critical value
        FCV <- (n - 1) * p / (n - p) * qf(p = 0.95, df1 = p, df2 = n - p)
        ## Cover or not 
        if(T2 <= FCV){
            points(Mu1, Mu2, pch = 15)
        }
    }
    
}
## individual t confidence interval
abline(v = Xbar[1] - sqrt(S[1, 1] / n) * qt(0.975, n - 1), col = 2, lwd = 2)
abline(v = Xbar[1] + sqrt(S[1, 1] / n) * qt(0.975, n - 1), col = 2, lwd = 2)
abline(h = Xbar[2] - sqrt(S[2, 2] / n) * qt(0.975, n - 1), col = 2, lwd = 2)
abline(h = Xbar[2] + sqrt(S[2, 2] / n) * qt(0.975, n - 1), col = 2, lwd = 2)
## Simultaneous T2 confidence interval
abline(v = Xbar[1] - sqrt(S[1, 1] / n) * sqrt(FCV), col = 3, lwd = 2)
abline(v = Xbar[1] + sqrt(S[1, 1] / n) * sqrt(FCV), col = 3, lwd = 2)
abline(h = Xbar[2] - sqrt(S[2, 2] / n) * sqrt(FCV), col = 3, lwd = 2)
abline(h = Xbar[2] + sqrt(S[2, 2] / n) * sqrt(FCV), col = 3, lwd = 2)
## Bonferonni correction
abline(v = Xbar[1] - sqrt(S[1, 1] / n) * qt(1 - 0.05 / (2 * 2), n - 1), col = 4, lwd = 2)
abline(v = Xbar[1] + sqrt(S[1, 1] / n) * qt(1 - 0.05 / (2 * 2), n - 1), col = 4, lwd = 2)
abline(h = Xbar[2] - sqrt(S[2, 2] / n) * qt(1 - 0.05 / (2 * 2), n - 1), col = 4, lwd = 2)
abline(h = Xbar[2] + sqrt(S[2, 2] / n) * qt(1 - 0.05 / (2 * 2), n - 1), col = 4, lwd = 2)
