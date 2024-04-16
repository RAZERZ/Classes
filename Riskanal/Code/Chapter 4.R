## Empirical distribution function
plot(ecdf(earth))

## Parametric estimation
## Assume Exponential distribution
## MLE = sample mean
Thetastar <- mean(earth) # Estimate theta
## MLE by numerical methods
library(fitdistrplus)
Exponential <- fitdist(data = earth, distr = "exp", # Fit an exponential distribution 
                       method = "mle" # Using maximum likelihood
                       ) # Estimate 1 / theta
1 / 0.002287232
## Use Probability papers
plot(Exponential)
## Assume normal distribution
Normal <- fitdist(data = earth, distr = "norm", method = "mle")
plot(Normal)

## Asymptotic 95% confidence interval for theta
Appro.Var <- (sum(earth) ^ 2) / (62 ^ 3)
1 / 0.002287232 - qnorm(1 - 0.05 / 2) * sqrt(Appro.Var)
1 / 0.002287232 + qnorm(1 - 0.05 / 2) * sqrt(Appro.Var)

## Asymptotic 95% confidence interval for g(theta) by Delta method
der.g <- 1500 / (Thetastar ^ 2) * exp(-1500 / Thetastar)
Approx.Var.g <- (der.g ^ 2) * Appro.Var
exp(-1500 / Thetastar) - qnorm(1 - 0.05 / 2) * sqrt(Approx.Var.g)
exp(-1500 / Thetastar) + qnorm(1 - 0.05 / 2) * sqrt(Approx.Var.g)

## Parametric Bootstrap
## Step 1: 
B <- 1000 # a lot bigger
## Step 2: usual MLE step
Thetastar <- mean(earth)
## Step 3
Error <- rep(NA, B)
for(b in 1 : B){
    ## Generate fake data: bootstrap sample
    x.bootstrap <- rexp(n = 62, rate = 0.002287232)
    ## Estimate theta using bootstrap sample
    theta.boot <- mean(x.bootstrap)
    ## g(theta)
    Error[b] <- exp(-1500 / Thetastar) - exp(-1500 / theta.boot)
}
## Visulize
hist(Error)
## CI
exp(-1500 / Thetastar) - quantile(Error, prob = 0.025)
exp(-1500 / Thetastar) + quantile(Error, prob = 0.975)
