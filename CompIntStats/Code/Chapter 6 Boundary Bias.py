# Boundary Bias 
import numpy as np
import matplotlib.pyplot as plt
## Generate fake data
np.random.seed(123456)
x = np.linspace(0, 4 * np.pi, num = 1000)
mu = np.sin(x)
error = np.random.normal(size = 1000, loc = 0, scale = 0.1)
y = mu + error
plt.scatter(x, y)
plt.show()

# Fit kernel regression
from statsmodels.nonparametric.kernel_regression import KernelReg
## Nadaraya-Watson estimator
NW = KernelReg(endog = y, exog = x, var_type = 'c', reg_type = "lc", bw = [0.3]) 
Fitted, _ = NW.fit(x)
plt.plot(x, mu)
plt.plot(x, Fitted, color = "red")
## Local linear regression of same bandwidth
LLR = KernelReg(endog = y, exog = x, var_type = 'c', reg_type = "ll", bw = [0.3]) 
Fitted, _ = LLR.fit(x)
plt.plot(x, Fitted, color = "orange")
plt.show()

