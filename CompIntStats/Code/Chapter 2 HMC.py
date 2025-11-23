# Hamiltonian MC
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

# Kernel of the log density that we want to sample random numbers from
def logtarget(x):
    return -x ** 2 + np.log(2 + np.sin(5 * x) + np.sin(2 * x)) 

# Gradient of the log of target (gradient of log(pi(x)) in our slide)
def grad_logpi(x):
    return -2 * x + (5 * np.cos(5 * x) + 2 * np.cos(2 * x)) / (2 + np.sin(5 * x) + np.sin(2 * x))

# HMC sampler for our example
def HMC(n, initial, m, epsilon, L):  
    # n: the length of the Markov chain, including burn in     
    # initial: the initial state     
    # m: variance of momentum
    # epsilon: step size
    # L: number of steps
    chain = np.zeros(1 + n)
    for t in range(1, n + 1):        
        # Draw initial momentum from normal as an example        
        phi0 = norm.rvs(loc = 0, scale = np.sqrt(m))         
        #-------------------------------------#         
        # Leapfrog to update x        
        x = chain[t - 1]         
        # Current Hamiltonian   
        H = -logtarget(x) - norm.logpdf(phi0, loc = 0, scale = np.sqrt(m))        
        # Make a half step for momentum at the beginning         
        phi = phi0 + 0.5 * epsilon * grad_logpi(x)
        # Alternate full steps for position and momentum         
        for i in range(0, L): 
            # Make a full step for the position             
            x = x + epsilon / m * phi             
            # Make a full step for the momentum, except at end of trajectory             
            if i != (L - 1):                
                phi = phi + epsilon * grad_logpi(x)          
                         
        # Make a half step for momentum at the end.         
        phi = phi + 0.5 * epsilon * grad_logpi(x)   
        #-------------------------------------#         
        # Negate momentum at end of trajectory to make the proposal symmetric         
        phistar = -phi         
        # Proposed Hamiltonian        
        Hstar = -logtarget(x) - norm.logpdf(phistar, loc = 0, scale = np.sqrt(m))        
        # Metropolis ratio         
        ratio = np.exp(H - Hstar)         
        # Accept or reject the state at end of trajectory, returning either         
        # the position at the end of the trajectory or the initial position  
        u = np.random.uniform(0, 1)
        if u < ratio:             
            chain[t] = x       
        else:           
            chain[t] = chain[t - 1]          
        
    return chain
  
# Perform HMC sampling
np.random.seed(54321)
HMCdraw = HMC(n = 20000, initial = 0, m = 1, epsilon = 0.4, L = 20)
plt.plot(HMCdraw)
plt.xlabel("Iteration")
plt.ylabel("x")
plt.show()
# Compare the histogram with the density
HMCdraw_NoBurnIn = HMCdraw[10001:]
plt.hist(HMCdraw_NoBurnIn, bins = 30, density = True)
x = np.linspace(-2, 2, 1000)
plt.plot(x, np.exp(logtarget(x)) / 3.544908)
plt.show()
np.mean(HMCdraw_NoBurnIn)
