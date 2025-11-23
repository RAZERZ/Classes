# Halton sequence
import numpy as np
from scipy.stats import qmc
sampler_halton = qmc.Halton(d = 2, scramble = False) # d dimension

## Generate 10 samples but we need to delete the first one
sample = sampler_halton.random(n = 10)
print(sample)
## If we let scramble = True, then we get
sampler_halton = qmc.Halton(d = 2, scramble = True)
sample = sampler_halton.random(n = 10)
print(sample)

# Sobol sequence
sampler_Sobol = qmc.Sobol(d = 2, scramble = False)
## Generate 2 ^ m samples, but we need to delete the first one.
sample = sampler_Sobol.random_base2(m = 3)  # 2^3 = 8 samples
print(sample)

# Approximate integral by change of variables in integral
Halton = sampler_halton.random(n = 10000)
np.mean(np.log(-np.log(1 - Halton)) * (1 - Halton))
## Approximate integral by factorization
x = -np.log(1 - Halton) / 2.0
0.5 * np.mean(np.log(x))


