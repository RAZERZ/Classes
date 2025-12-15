# Local Linear Regression
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
#### 'c' = continuous variables, ll = local linear 
LLR = KernelReg(endog = y, exog = x, var_type = 'c', reg_type = "ll") 
## In-sample fitted value
Fitted, _ = LLR.fit(x)
plt.scatter(x, y)
plt.plot(x, mu)
plt.plot(x, Fitted, color = "red")
plt.show()
## Out-of-Sample prediction
Pred, _ = LLR.fit([0.1, 0.3, 0.5])
## Effect of bandwidth
LLR = KernelReg(endog = y, exog = x, var_type = 'c', reg_type = "ll", bw = [0.5]) 
Fitted, _ = LLR.fit(x)
plt.plot(x, mu)
plt.plot(x, Fitted, color = "red")
LLR = KernelReg(endog = y, exog = x, var_type = 'c', reg_type = "ll", bw = [5]) 
Fitted, _ = LLR.fit(x)
plt.plot(x, Fitted, color = "orange")
plt.show()
