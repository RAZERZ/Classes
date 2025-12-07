# Nadaraya-Watson Estimator
import numpy as np
import matplotlib.pyplot as plt

## Generate fake data
np.random.seed(123456)
x = np.linspace(0, 4 * np.pi, num = 200)
mu = np.sin(x)
error = np.random.normal(size = 200, loc = 0, scale = 0.1)
y = mu + error
plt.scatter(x, y)
plt.show()

## Regression using Nadaraya-Watson estimator
from statsmodels.nonparametric.kernel_regression import KernelReg
#### 'c' = continuous variables, lc = local constant 
NW = KernelReg(endog = y, exog = x, var_type = 'c', reg_type = "lc") 
## In-sample fitted value
Fitted, _ = NW.fit(x)
plt.plot(x, mu)
plt.plot(x, Fitted, color = "red")
plt.show()
## Out-of-Sample prediction
Pred, _ = NW.fit([0.1, 0.3, 0.5])
## Effect of bandwidth
NW = KernelReg(endog = y, exog = x, var_type = 'c', reg_type = "lc", bw = [0.5]) 
Fitted, _ = NW.fit(x)
plt.plot(x, mu)
plt.plot(x, Fitted, color = "red")
NW = KernelReg(endog = y, exog = x, var_type = 'c', reg_type = "lc", bw = [5]) 
Fitted, _ = NW.fit(x)
plt.plot(x, Fitted, color = "orange")
plt.show()
