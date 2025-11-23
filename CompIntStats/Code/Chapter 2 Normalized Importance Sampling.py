# Normalized Importance Sampling
import numpy as np
from scipy.stats import expon
## Example: integrate log(1 + x) * c * x * exp(-4 * x) from 0 to infinity  
## We know c * x * exp(-4 * x) is a probability density, but we do not know the value of c
# Simulate random number from importance distribution
np.random.seed(12345)
x = expon.rvs(size = 10000, scale = 1.0) 
# Compute importance weight w = ptilde(x) / g(x)
w = x * np.exp(-4 * x) / expon.pdf(x, scale = 1)
# Approximated integral value
sum(w * np.log(1 + x)) / sum(w)
