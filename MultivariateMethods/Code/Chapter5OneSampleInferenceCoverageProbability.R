#### This code shows that the coverage probability of the individual 
#### confidence intervals are lower than the nominal level
#### We want to construct confidence intervals for all elements of mu.
#### Simulation is used here.
Alpha <- 0.05
N <- 50
p <- 5
Sigma <- matrix(0.4, p, p)
diag(Sigma) <- 1
Replication <- 1e04 # Number of replications in the simulation
CP <- matrix(NA, Replication, 3) # To store all coverage
colnames(CP) <- c("individual t interval", 
                  "Bonferroni",
                  "T2 interval")
for(r in 1 : Replication){
    
    set.seed(12345 + 10 * r) # Set random seeds
    X <- mvtnorm::rmvnorm(n = N, mean = rep(0, p), sigma = Sigma)
    Xbar <- colMeans(X)
    S <- cov(X)
    ## t confidence interval for each mu  
    Tcv <- qt(1 - Alpha / 2, df = N - 1)
    Low.T <- Xbar - Tcv * sqrt(diag(S) / N)
    Upp.T <- Xbar + Tcv * sqrt(diag(S) / N)
    CP[r, "individual t interval"] <- prod((Low.T <= 0) * (0 <= Upp.T))
    ## Bonferroni
    Tcv <- qt(1 - Alpha / (2 * p), df = N - 1)
    Low.T <- Xbar - Tcv * sqrt(diag(S) / N)
    Upp.T <- Xbar + Tcv * sqrt(diag(S) / N)
    CP[r, "Bonferroni"] <- prod((Low.T <= 0) * (0 <= Upp.T))
    ## T2 interval
    Fcv <- sqrt(p * (N - 1) * qf(p = 1 - Alpha, df1 = p, df2 = N - p) / (N - p))
    Low.F <- Xbar - Fcv * sqrt(diag(S) / N)
    Upp.F <- Xbar + Fcv * sqrt(diag(S) / N)
    CP[r, "T2 interval"] <- prod((Low.F <= 0) * (0 <= Upp.F))
}
colMeans(CP) ## Relative frequency as coverage probability
