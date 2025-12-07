# Gaussian mixture
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm
from scipy.stats import binom
## Define observed log-likelihood
def obs_log_like(y, mean, var, prop):
    ## y: observed data vector
    ## mean, var, prop: initial values of group mean, variance and proportion
    n = len(y)
    K = len(mean)
    likelihood = np.zeros([n, K]) # dimension = n * K
    for k in range(K):
        likelihood[:, k] = norm.pdf(y, loc = mean[k], scale = np.sqrt(var[k])) * prop[k]
    
    obs_likelihood = np.sum(likelihood, axis = 1) # Column-wise sum
    return np.sum(np.log(obs_likelihood))

## Define the function to perform EM algorithm for univariate Gaussian mixture
def GaussMixture(y, K, mu, var, prop, max_iter = 1000, tol = 1e-6):
    ## y: observed data vector
    ## K: number of components in the mixture
    ## mu, var, prop: initial values of group mean, variance and proportion
    ## max_iter: maximum number of iterations
    ## tol: tolerance for convergence
    
    ## Number of data points
    n = len(y) 
    ## Observed log-likelihood
    obsloglike = []
    obsloglike.append(obs_log_like(y = y, mean = mu, var = var, prop = prop))
    ## EM algorithm
    mu_trace = [mu.copy()]
    var_trace = [var.copy()]
    prop_trace = [prop.copy()]
    for iter in range(max_iter):
        
        ## E-Step: Calculate posterior probabilities (responsibilities)
        resp = np.zeros((n, K))
        for i in range(n):
            resp_i = np.zeros(K)
            for k in range(K):
                resp_i[k] = norm.pdf(y[i], loc = mu_trace[iter][k], scale = np.sqrt(var_trace[iter][k])) * prop_trace[iter][k]
            
            resp[i, ] = resp_i / np.sum(resp_i)
        
        ## M-Step: Update parameters
        #### Update proportion
        mean_resp = np.mean(resp, axis = 0) # Column average
        prop_trace.append(mean_resp.copy())
        #### Update means
        updt_mu = np.zeros(K)
        for k in range(K):
            updt_mu[k] = np.sum(resp[:, k] * y) / np.sum(resp[:, k])
        mu_trace.append(updt_mu)
        #### Update variance
        updt_var = np.zeros(K)
        for k in range(K):
            updt_var[k] = np.sum(resp[:, k] * (y - mu_trace[iter + 1][k]) ** 2) / np.sum(resp[:, k])
        var_trace.append(updt_var)
        
        ## Compute new log-likelihood
        updt_obsloglike = obs_log_like(y = y, mean = mu_trace[iter + 1], var = var_trace[iter + 1], prop = prop_trace[iter + 1])
        obsloglike.append(updt_obsloglike)
        
        ## Check for convergence
        increase = abs(obsloglike[iter+1] - obsloglike[iter])
        if increase < tol:
            break
        
    return {
        'means': mu_trace[iter + 1],
        'vars': var_trace[iter + 1],
        'prop': prop_trace[iter + 1],
        'obs_loglike': obsloglike[iter + 1],
        'iter': iter + 1,
        'tol': increase,
        'resp': resp,
        'trace': {
            'means': mu_trace,
            'vars': var_trace,
            'prop': prop_trace,
            'obs_loglike': obsloglike
        }
    }

# Generate fake Gaussian mixture data
## Target density
def Density(y) :
    return 0.4 * norm.pdf(y, loc = -2, scale = 1) + 0.6 * norm.pdf(y, loc = 1, scale = 1)
yvalues = np.linspace(-4, 4, num = 1000)
plt.plot(yvalues, Density(yvalues))
plt.show()
## Generate fake data
np.random.seed(12345)
Bern = binom.rvs(size = 1000, n = 1, p = 0.6) ## Decide group
Y = np.zeros(1000)
for i in range(1000):
    ## Generate Y
    if Bern[i] == 0 :
        Y[i] = norm.rvs(size = 1, loc = -2, scale = 1)
    else :
        Y[i]= norm.rvs(size = 1, loc = 1, scale = 1)

## Fit Gaussian Mixture
Fit = GaussMixture(y = Y, K = 2, mu = np.array([-1, 1]), var = np.array([1, 1]), prop = np.array([0.5, 0.5]), max_iter = 100, tol = 1e-5)
Fit["iter"]
Fit["tol"]
#### Estimate parameter values
Fit["means"]
Fit["vars"]
Fit["prop"]
#### Responsibility matrix
Fit["resp"]
#### Plot the trajectory
plt.plot(Fit['trace']['obs_loglike'])
plt.show()
#### Plot estimated density function from the fitted mixture
def est_density(y):
    return (Fit['prop'][0] * norm.pdf(y, loc = Fit['means'][0], scale = np.sqrt(Fit['vars'][0])) +
            Fit['prop'][1] * norm.pdf(y, loc = Fit['means'][1], scale = np.sqrt(Fit['vars'][1])))

plt.plot(yvalues, Density(yvalues), label = "True density")
plt.plot(yvalues, est_density(yvalues), label = 'Estimated Density', color = 'red')
plt.legend()
plt.show()

# We can also use Gaussian mixture models to approximate other distributions. 
from scipy.stats import gamma
## Generate random numbers
np.random.seed(12345)
Y = np.log(gamma.rvs(size = 1000, a = 2, scale = 1 / 2))
def Density(x):
    return gamma.pdf(x = np.exp(x), a = 2, scale = 1 / 2) * np.exp(x)
plt.plot(yvalues, Density(yvalues), label = "True density")
## Fit Gaussian distribution
plt.plot(yvalues, norm.pdf(yvalues, loc = np.mean(Y), scale = np.std(Y, ddof = 1)), color = 'red', label = "Gaussian")
## Fit two components Gaussian mixture (Not converged at 100 iterations!)
Fit2 = GaussMixture(y = Y, K = 2, mu = np.array([-1, 1]), var = np.array([1, 1]), prop = np.array([0.5, 0.5]), max_iter = 100, tol = 1e-5)
Fit2["tol"]
plt.plot(yvalues, 
         Fit2['prop'][0] * norm.pdf(yvalues, loc = Fit2['means'][0], scale = np.sqrt(Fit2['vars'][0])) +
         Fit2['prop'][1] * norm.pdf(yvalues, loc = Fit2['means'][1], scale = np.sqrt(Fit2['vars'][1])),
         label = '2-Component Mixture', color = 'green')
plt.legend()
plt.show()

