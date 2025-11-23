# Nonparametric bootstrap fails to work for uniform(0, theta) 
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import expon

# Simulate Uniform(0, 1) of size N
N = 1000
X = np.random.uniform(0, 1, size = N)
# Find its max value
Xmax = max(X) 
# Bootstrap: We draw with replacement M out of N
M = N 
B = 10000
Bstat = np.zeros(B)
for r in range(B):
    # Sample with replacement
    Bx = np.random.choice(X, size = M, replace = True)
    # Calculate the statistic
    Bstat[r] = M * (Xmax - max(Bx)) / Xmax
    
# A peak at Xmax, corresponding to a zero statistic value
plt.hist(Bstat, bins = 30)
plt.show()


# If we choose a small M, bootstrap looks better
M = int(np.sqrt(N))
Bstat = np.zeros(B)
for r in range(B):
    # Sample with replacement
    Bx = np.random.choice(X, size = M, replace = True)
    # Calculate the statistic
    Bstat[r] = M * (Xmax - max(Bx)) / Xmax
# A peak at Xmax, corresponding to a zero statistic value
# But it still follows the trend.
plt.hist(Bstat, bins = 30, density = True)
x = np.linspace(0, 8, 1000)
limitdensity = expon.pdf(x, scale = 1)
plt.plot(x, limitdensity)
plt.show()
