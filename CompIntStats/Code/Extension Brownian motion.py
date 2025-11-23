# Simulate option price
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import expon
from scipy.stats import norm
from scipy.stats import truncnorm

## Define the function to compute stock price
def Price(n, S0, r, sigma, Time, K):
    # n: Number of iterations
    # S0: S(0)
    # r: interest rate
    # sigma: square root of diffusion coefficient
    # Time: time T
    # K: Strike price
    C = np.zeros(n)
    for i in range(n): 
        ## Simulate N(0,1)
        z = norm.rvs(size = 1, loc = 0, scale = 1)
        ## Simulate S(T)
        ST = S0 * np.exp((r - 0.5 * sigma ** 2) * Time + sigma * np.sqrt(Time) * z)
        ## Compute payoff
        C[i] = max(ST - K, 0) 
    
    ## Return payoff
    return C / np.exp(r * Time)

Payoff = Price(n = 10000, S0 = 1, r = 0.05, sigma = 0.1, Time = 1, K = 1.1)
np.mean(Payoff)
np.mean(Payoff == 0)

# Variance Reduction by Importance Sampling 
def PriceIS(n, S0, r, sigma, Time, K):
    # n: Number of iterations
    # S0: S(0)
    # r: interest rate
    # sigma: square root of diffusion coefficient
    # Time: time T
    # K: Strike price
    C = np.zeros(n)
    mean = np.log(S0) + (r - 0.5 * sigma ** 2) * Time
    sd = np.sqrt(Time) * sigma
    for i in range(n): 
        ## Simulate from shifted Exponential
        y = expon.rvs(size = 1, scale = 0.1) + np.log(K)
        ## Compute payoff
        C[i] = (np.exp(y) - K) * norm.pdf(y, loc = mean, scale = sd) / expon.pdf(y - np.log(K), scale = 0.1)  
    
    ## Return payoff
    return C / np.exp(r * Time)

Payoff_IS = PriceIS(n = 10000, S0 = 1, r = 0.05, sigma = 0.1, Time = 1, K = 1.1)
np.mean(Payoff_IS)
np.std(Payoff, ddof = 1)
np.std(Payoff_IS, ddof = 1)

# Variance Reduction by Rao-Blackwell
def PriceRaoBlackwell(n, S0, r, sigma, Time, K):
    # n: Number of iterations
    # S0: S(0)
    # r: interest rate
    # sigma: square root of diffusion coefficient
    # Time: time T
    # K: Strike price
    C = np.zeros(n)
    mean = np.log(S0) + (r - 0.5 * sigma ** 2) * Time
    sd = np.sqrt(Time) * sigma
    for i in range(n): 
        ## Simulate from truncated normal
        a = (np.log(K) - mean) / sd
        y = truncnorm.rvs(size = 1, a = a, b = np.inf, loc = mean, scale = sd)
        ## Compute payoff
        C[i] = np.exp(y) - K
    
    ## Return payoff
    return (1 - norm.cdf(np.log(K), loc = mean, scale = sd)) * C / np.exp(r * Time)

Payoff_RB = PriceRaoBlackwell(n = 10000, S0 = 1, r = 0.05, sigma = 0.1, Time = 1, K = 1.1)
np.mean(Payoff_RB)
np.std(Payoff_RB, ddof = 1)

# Simulate a standard Brownian motion 
## Stepsize (time difference)
h = 0.001 
## Simulate independent increments
z = norm.rvs(size = 1000, loc = 0, scale = np.sqrt(h))
## Simulated trajectory
SBM = np.cumsum(np.append(0, z)) 
plt.plot(SBM)
plt.show()

