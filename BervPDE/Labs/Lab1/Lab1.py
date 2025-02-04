import numpy as np
import scipy.sparse.linalg as spsplg
import scipy.linalg as splg
import operators as ops
import matplotlib.pyplot as plt
import time
import rungekutta4 as rk4

######################################################################################
##                                                                                  ##
##  Lab "Introduction to Finite Difference Methods", part 1, for course             ##
##  "Scientific computing for PDEs" at Uppsala University.                          ##
##                                                                                  ##
##  Author: Gustav Eriksson                                                         ##
##  Date:   2022-08-31                                                              ##
##  Updated by Vidar Stiernstrom, January 2023.                                     ##
##  Based on Matlab code written by Ken Mattsson in June 2022.                      ##
##                                                                                  ##
##  Solves the first order wave equation u_t + c u_x = 0 with periodic boundary     ##
##  conditions using summation-by-parts finite differences. Illustrates dispersion  ##
##  errors for different orders of accuracy.                                        ##
##                                                                                  ##
##  The code has been tested on the following versions:                             ##
##  - Python     3.9.9                                                              ##
##  - Numpy      1.25.2                                                             ##
##  - Scipy      1.11.2                                                             ##
##  - Matplotlib 3.7.2                                                              ##
##                                                                                  ##
######################################################################################

# Initial data
def f(x):
    return np.exp(-((x - 0.5)/0.05)**2)

def run_simulation(mx=100, order=2, show_animation=True):
    """Solves the advection equation using finite differences
    and Runge-Kutta 4.
    
    Method parameters: 
    mx:     Number of grid points, integer > 15.
    order:  Order of accuracy, 2, 4, 6, 8, 10 or 12
    """

    # Model parameters
    c = 1 # wave speed
    T = 3 # end time
    xl = 0 # left boundary
    xr = 1 # right boundary
    L = xr - xl # domain length

    # Space discretization
    hx = (xr - xl)/mx
    xvec = np.linspace(xl, xr-hx, mx) # periodic, u(xl) = u(xr)
    _, _, D1 = ops.periodic_expl(mx, hx, order)

    # Define right-hand-side function
    def rhs(u):
        return -c*D1@u

    # Time discretization
    ht_try = 0.1*hx/c
    mt = int(np.ceil(T/ht_try) + 1) # round up so that (mt-1)*ht = T
    tvec, ht = np.linspace(0, T, mt, retstep=True)

    # Initialize time variable and solution vector
    t = 0
    u = f(xvec)

    # Initialize plot for animation
    if show_animation:
        fig, ax = plt.subplots()
        [line] = ax.plot(xvec, u, label='Approximation')
        ax.set_xlim([xl, xr-hx])
        ax.set_ylim([-1, 1.2])
        title = plt.title(f't = {0:.2f}')
        plt.draw()
        plt.pause(1)

    # Loop over all time steps
    for tidx in range(mt-1):

        # Take one step with the fourth order Runge-Kutta method.
        u, t = rk4.step(rhs, u, t, ht)

        # Update plot every 50th time step
        if tidx % 50 == 0 and show_animation: 
            line.set_ydata(u)
            title.set_text(f't = {t:.2f}')
            plt.draw()
            plt.pause(1e-8)

    # Close figure window
    if show_animation:
        plt.close()

    return u, T, xvec, hx, L, c

def exact_solution(t, xvec, L, c):
    T1 = L/c  # Time for one lap
    t_eff = (t/T1 - np.floor(t/T1))*T1  # "Effective" time, using periodicity
    u_exact = f(xvec - c*t_eff)
    return u_exact

def l2_norm(vec, h):
    return np.sqrt(h)*np.sqrt(np.sum(vec**2))

def compute_error(u, u_exact, hx):
    """Compute discrete l2 error"""
    error_vec = u - u_exact
    relative_l2_error = l2_norm(error_vec, hx)/l2_norm(u_exact, hx)
    return relative_l2_error

def plot_final_solution(u, u_exact, xvec, T):
    fig, ax = plt.subplots()
    ax.plot(xvec, u, label='Approximation')
    plt.plot(xvec, u_exact, 'r--', label='Exact')
    ax.set_xlim([xvec[0], xvec[-1]])
    ax.set_ylim([-1, 1.2])
    plt.title(f't = {T:.2f}')
    plt.legend()
    plt.show()

def main():
    m = 200   # Number of grid points, integer > 15.
    order = 2  # Order of accuracy. 2, 4, 6, 8, 10, or 12.
    u, T, xvec, hx, L, c = run_simulation(m, order)
    u_exact = exact_solution(T, xvec, L, c)
    error = compute_error(u, u_exact, hx)
    print(f'L2-error: {error:.2e}')
    plot_final_solution(u, u_exact, xvec, T)

if __name__ == '__main__':
    main()    
