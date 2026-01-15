import pandas as pd
import numpy as np
import sklearn as sk
import matplotlib.pyplot as plt
import Utils
import warnings
warnings.filterwarnings('ignore')

f = lambda x: np.pow(x,0.2)*np.pow((1-x), 1.3)

def problem1_rejection(n_samples=1):

    results = np.zeros(n_samples)
    count = 0
    while count < n_samples:
        #2.9282686083410767
        x = np.random.uniform(0,1)
        r = f(x)/(0.55488)
        U = np.random.uniform(0,1)
        
        if U <= r:
            results[count] = x
            count = count + 1

    return results

problem1_samples = problem1_rejection(100000)

def problem2():
    return np.mean(np.sin(10*problem1_samples))

# plt.hist(problem1_samples)
# plt.show()

x = np.random.uniform(0,1,100000)
# print(1/np.mean(f(x)))

# plt.plot(x, 3.1*f(x))

# plt.show()
