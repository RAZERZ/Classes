# Bootstrap
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import expon

## Load data
# earthquake = pd.read_csv("C:/Users/shaji948/Box/Teaching/Math Department/Computer intensive statistics and applications 1MS049/Python/Earthquake.csv")
earthquake = pd.read_csv("/home/rami/UU/Classes/CompIntStats/Code/Earthquake.csv")
earth = earthquake["x"].to_numpy()
#MLE Of focus quantity

## Estimate the parametric model (Exponential distribution)
Thetastar = np.mean(earth)
np.exp(-1500/Thetastar)
## Obtain median
Median = np.median(earth)
## Bootstrap
# B = 1000 # a lot bigger
# Error = np.zeros(B)
# Parametric = False
# for b in range(B):
    # ## Generate fake data: bootstrap sample
    # if Parametric == True:
        # ## Parametric bootstrap
        # x_bootstrap = expon.rvs(size = 62, scale = Thetastar)
    # else:
        # ## Nonparametric bootstrap
        # x_bootstrap = np.random.choice(earth, size = 62, replace = True)

    # ## Estimate median using bootstrap sample
    # median_bootstrap = np.median(x_bootstrap)
    # Error[b] = median_bootstrap - Median

B = 1000 # a lot bigger
Error = np.zeros(B)
Parametric = True 
Percent = np.zeros(B)
Theta_boot = np.zeros(B)
for b in range(B):
    ## Generate fake data: bootstrap sample
    if Parametric == True:
        ## Parametric bootstrap
        x_bootstrap = expon.rvs(size = 62, scale = Thetastar)
    else:
        ## Nonparametric bootstrap
        x_bootstrap = np.random.choice(earth, size = 62, replace = True)

    ## Estimate median using bootstrap sample
    # median_bootstrap = np.median(x_bootstrap)
    # Error[b] = median_bootstrap - Median
    Theta_boot[b]= np.mean(x_bootstrap)
    Percent[b] = np.exp(-1500/Theta_boot[b])

## Visulize
plt.hist(Percent, bins = 30, density = True)
plt.show()
#Compute percentile conf interval
np.quantile(Percent, [0.025,0.975])
