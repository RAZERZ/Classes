import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import beta

# Rejection sampling
# Example: Generate random numbers from Beta(2.5, 3.2)
a = 2.5
b = 3.2
mode = (a - 1) / (a + b - 2)
M = beta.pdf(mode, a, b)

# We generate Beta(2.5, 3.2) random numbers where g() is Uniform(0, 1)
Beta = np.full(10000, -1.0) # Initiate a vector of -1
np.random.seed(12345)
for i in range(len(Beta)): 
    ## In practice, a while loop
    ## Generate x from g()
    x = np.random.uniform(0, 1)
    ## Compute the ratio
    r = beta.pdf(x, a, b) / (M * 1)
    ## Generate another uniform[0,1]
    u = np.random.uniform(0, 1)
    if u < r:
        Beta[i] = x
    # else nothing happens
    
# Empirical acceptance percentage
np.mean(Beta >= 0)

# Compre the histogram with the density fucntion
Beta = Beta[Beta >= 0]
plt.hist(Beta, bins = 20, density = True)
x = np.linspace(0, 1, 1000)
betadensity = beta.pdf(x, a, b)
plt.plot(x, betadensity)
plt.show()
