#!/usr/bin/env python3
import numpy as np
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt

def fun(t,u):
    GM = 1
    r = np.sqrt(u[0]**2+u[2]**2)
    return np.array([u[1],
                     -GM*u[0]/r**3,
                     u[3],
                     -GM*u[2]/r**3])

tspan = (0, 2*np.pi) # en period
ecc = 0
u0 = np.array([1-ecc, 0, 0, np.sqrt((1+ecc)/(1-ecc))])
SOL = solve_ivp(fun, tspan, u0, max_step=0.01)

plt.plot(SOL.t, SOL.y[2,:])
plt.grid()
plt.axis('equal')
plt.xlabel('x')
plt.ylabel('y')
plt.show()
