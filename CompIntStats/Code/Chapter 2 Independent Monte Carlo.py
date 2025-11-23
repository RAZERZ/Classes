# Independent Monte Carlo 
import numpy as np
from scipy.stats import expon
from scipy.stats import norm
## Example 1: integrate log(x) * exp(-2 * x) from 0 to infinity 
N = 10000
## Simulate independent random number
np.random.seed(12345)
x = expon.rvs(size = N, scale = 0.5) # same as rate = 2
## Compute the value of h(x)
hx = 0.5 * np.log(x)
## Approximated integral value
np.mean(hx)
## Approximated 95% confidence interval
quantile = norm.ppf(0.975, loc = 0, scale = 1)
np.mean(hx) - quantile * np.std(hx, ddof = 1) / np.sqrt(N)
np.mean(hx) + quantile * np.std(hx, ddof = 1) / np.sqrt(N)

# Example 2: integrate log(x) * exp(-2 * x) from 0 to 0.5 
N = 10000
## Simulate independent random number
np.random.seed(12345)
x = expon.rvs(size = N, scale = 0.5) # same as rate = 2
## Compute the value of h(x)
hx = 0.5 * np.log(x) * (x < 0.5)
## Approximated integral value
np.mean(hx)
## Approximated 95% confidence itnerval
quantile = norm.ppf(0.975, loc = 0, scale = 1)
np.mean(hx) - quantile * np.std(hx, ddof = 1) / np.sqrt(N)
np.mean(hx) + quantile * np.std(hx, ddof = 1) / np.sqrt(N)
