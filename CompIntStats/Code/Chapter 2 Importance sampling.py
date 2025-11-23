# Importance Sampling 
import numpy as np
from scipy.stats import expon
from scipy.stats import norm
## Example 1: integrate log(1 + x) * x * exp(-4 * x) from 0 to infinity 
N = 10000
## Simulate random number from importance distribution
np.random.seed(12345)
x = expon.rvs(size = N, scale = 1)
## Compute the value of f(x) / g(x)
fgx = np.log(1 + x) * x * np.exp(-4 * x) / expon.pdf(x, scale = 1)
## Approximated integral value
np.mean(fgx)
## Approximated 95% confidence interval
sig_g = np.std(fgx, ddof = 1)
quantile = norm.ppf(0.975, loc = 0, scale = 1)
np.mean(fgx) - quantile * sig_g / np.sqrt(N)
np.mean(fgx) + quantile * sig_g / np.sqrt(N)

## Example 2: Approximate P(X > 2) 
N = 10000
## Simulate random number from importance distribution
np.random.seed(12345)
x = norm.rvs(size = N, loc = 0, scale = 10)
## Compute the value of f(x) / g(x)
fgx = 1 / (np.pi * (1 + x ** 2)) * (x > 2) / norm.pdf(x, loc = 0, scale = 10)
## Approximated integral value
np.mean(fgx)

