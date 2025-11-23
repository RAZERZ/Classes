# Gibbs sampler (Bivariate normal)
import numpy as np
import matplotlib.pyplot as plt

## Define function for Gibbs sampling
def Gibbs(x0, n, mu1, mu2, sigma1, sigma2, rho):
    ## x0: initial values
    ## n: length of Markov chain
    ## mu1, mu2: expected values of the bivariate normal
    ## sigma1, sigma2, rho: parameters in the covariance matrix
    x1 = np.zeros(n + 1)
    x2 = np.zeros(n + 1)
    x1[0] = x0[0]
    x2[0] = x0[1]
    ## Gibbs sampling
    for i in range(n):
        # Sample x1 given x2[i]
        mean1 = mu1 + rho * sigma1 * (x2[i] - mu2) / sigma2
        sd1 = sigma1 * np.sqrt(1 - rho ** 2)
        x1[i + 1] = np.random.normal(size = 1, loc = mean1, scale = sd1)
        
        # Sample x2 given x1[i+1]
        mean2 = mu2 + rho * sigma2 * (x1[i + 1] - mu1) / sigma1
        sd2 = sigma2 * np.sqrt(1 - rho**2)
        x2[i + 1] = np.random.normal(size = 1, loc = mean2, scale = sd2)
    
    return x1, x2

# Run the Gibbs sampler
x1, x2 = Gibbs(x0 = (0, 0), n = 1000, mu1 = -1, mu2 = 2, sigma1 = 1, sigma2 = 2, rho = 0.5)

# Plot
fig, axs = plt.subplots(2, 1, figsize=(8, 6))
axs[0].plot(x1)
axs[0].set_ylabel("x1")
axs[1].plot(x2)
axs[1].set_ylabel("x2")
plt.show()
## Bivariate plot
plt.plot(x1[0 : 20], x2[0 : 20])
plt.show()

