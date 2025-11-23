# Example of Rao-Blackwellization with Gibbs sampler
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
from scipy.stats import expon
from scipy.stats import gamma

# We first generate some fake data as observed data
np.random.seed(12345)
Mu = 1.5 
Lambda = 0.3 
Data = norm.rvs(size = 50, loc = Mu, scale = 1.0 / np.sqrt(Lambda))

## Function to perform Gibbs sampling
def Gibbs(n, initial, data, mu0, lam0, b0):
    ## n: the length of the Markov chain, including burn in period
    ## initial: the initial state for (mu, lambda)
    ## data: observed data
    ## mu0, lam0, b0: hyperparameters that we need to specify
    ## First, obtain summary statistics from the data
    nobs = len(data)
    s = sum(data)
    s2 = sum(data ** 2)
    ## Second, write the loop for Gibbs sampling
    mu = np.zeros(n + 1)
    mu[0] = initial[0]
    lamb = np.zeros(n + 1)
    lamb[0] = initial[1]
    for t in range(1, n + 1):
        ## Draw mu conditional on lambda: a normal distribution
        mean_norm = (s / lam0 + mu0 / lamb[t - 1]) / (nobs / lam0 + 1 / lamb[t - 1])
        sd_norm = 1 / np.sqrt(lamb[t - 1] * lam0 * (nobs / lam0 + 1 / lamb[t - 1]))
        mu[t] = norm.rvs(loc = mean_norm, scale = sd_norm)           
        ## Draw lamb conditional on mu: a gamma distribution
        bn = b0 + 0.5 * (s2 - 2.0 * mu[t] * s + nobs * mu[t] ** 2)
        lamb[t] = gamma.rvs(a = 0.5 * nobs + 1.0, scale = 1 / bn)
       
    return mu, lamb

## Perform Gibbs sampling
np.random.seed(54321)
Mu, Lambda = Gibbs(n = 20000, initial = np.array([0, 1]), data = Data, mu0 = 1, lam0 = 1, b0 = 1)
## Discard burn-in period
Mu = Mu[10001 : ]
Lambda = Lambda[10001 : ]

## We implement Rao-Blackwell
#### Naive MC     
np.mean(Lambda)     
#### Rao-Blackwell approach
bn = (1 + 0.5 * np.sum(Data ** 2) - np.sum(Data) * Mu + 0.5 * len(Data) * (Mu ** 2))
np.mean((1 + len(Data) / 2) / bn) 

#### Investigate the variance reduction
Rep = 100 
Naive = np.zeros(Rep)
RB = np.zeros(Rep)
for r in range(Rep) :
    ## Run Gibbs and discard burn-in period
    Mu, Lambda = Gibbs(n = 2000, initial = np.array([0, 1]), data = Data, mu0 = 1, lam0 = 1, b0 = 1)
    Mu = Mu[1001 : ]
    Lambda = Lambda[1001 : ]
    ## Naive MC     
    Naive[r] = np.mean(Lambda)   
    ## Rao-Blackwell    
    bn = (1 + 0.5 * np.sum(Data ** 2) - np.sum(Data) * Mu + 0.5 * len(Data) * (Mu ** 2))
    RB[r] = np.mean((1 + len(Data) / 2) / bn) 

np.mean(Naive)
np.mean(RB)
np.var(Naive) 
np.var(RB)
