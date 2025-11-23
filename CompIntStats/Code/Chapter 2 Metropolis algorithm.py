# Metropolis Algorithm
import numpy as np
import matplotlib.pyplot as plt
# The kernel of log target density to simulate random numbers from
def logtarget(x):
    return -x ** 2 + np.log(2 + np.sin(5 * x) + np.sin(2 * x)) 
# Functions to perform Metropolis sampling
def Metropolis(n, x0, sd):
    # n is the length of the Markov chain, including burn in
    # x0 is the initial state
    # Initiate the Markov chain
    chain = np.zeros(n + 1)
    chain[0] = x0
    for t in range(1, n + 1):
        # propose a candidate by N(x, sd)
        y = chain[t - 1] + sd * np.random.randn()
        # calculate the ratio
        r = np.exp(logtarget(y) - logtarget(chain[t - 1]))
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
Metr = Metropolis(n = 10000, x0 = 10, sd = 1) 
plt.plot(Metr)
plt.xlabel("Iteration")
plt.ylabel("x")
plt.show()
# Compare the histogram with the density
Metr_NoBurnIn = Metr[1001:]
plt.hist(Metr_NoBurnIn, bins = 30, density = True)
x = np.linspace(-2, 2, 1000)
plt.plot(x, np.exp(logtarget(x)) / 3.544908)
plt.show()
