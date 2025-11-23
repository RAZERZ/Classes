# Metropolis-Hastings Algorithm
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

## For illustration purpose, our proposal distribution is Y | X[t] is N(X[t]/2, sd^2)
## Kernel of the log density that we want to sample random numbers from
def logtarget(x):
    return -x ** 2 + np.log(2 + np.sin(5 * x) + np.sin(2 * x)) 

## Function to conduct M-H sampling
def MetropolisHastings(n, x0, sd):   
    # n is the length of the Markov chain, including the burn in period
    # x0 is the initial state
    # sd is the standard deviation in the proposal distribution
    chain = np.zeros(n + 1)
    chain[0] = x0
    for t in range(1, n + 1):
        # propose a candidate by N(x, sd)
        y = chain[t - 1] + sd * np.random.randn()
        # Calculate the numerator
        log_num = logtarget(y) + norm.logpdf(chain[t - 1], loc = y, scale = sd)
        # Calculate the denominator
        log_den = logtarget(chain[t - 1]) + norm.logpdf(y, loc = chain[t - 1], scale = sd)
        # calculate the ratio
        r = np.exp(log_num - log_den)
        # Generate U(0, 1)
        u = np.random.uniform(0, 1)
        # Update
        if u <= r:
            chain[t] = y
        else:
            chain[t] = chain[t - 1]
            
    return chain

# Perform Metropolis sampling
np.random.seed(12345)
MetrHast = MetropolisHastings(n = 10000, x0 = 10, sd = 1) 
plt.plot(MetrHast)
plt.xlabel("Iteration")
plt.ylabel("x")
plt.show()
# Compare the histogram with the density
MetrHast_NoBurnIn = MetrHast[1001:]
plt.hist(MetrHast_NoBurnIn, bins = 30, density = True)
x = np.linspace(-2, 2, 1000)
plt.plot(x, np.exp(logtarget(x)) / 3.544908)
plt.show()

