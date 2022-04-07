import numpy as np
import matplotlib.pyplot as plt

func = lambda x: np.exp(x)-10*np.cos(x)
fprim = lambda x: np.exp(x)+10*np.sin(x)

def bisection(func, a, b, tol):
    # sign_fa = np.sign(func(a))
    # sign_fb = np.sign(func(b))

    x = (a+b)/2
    itercounter = 0
    sign = np.sign(func(a)) == np.sign(func(b))

    assert(not sign)

    while((b-a)/2) > tol:
        
        itercounter += 1
        localSign = np.sign(func(x))

        if sign == localSign:
            a = x
        else:
            b = x

        x = (a+b)/2
    return x, itercounter


def newton(func, fprim, x0, tol):
    maxiter = 100
    itercounter = 0
    x = x0
    dx = 2*tol

    while(np.abs(dx) > tol):
        itercounter +=1
        dx = -func(x)/fprim(x)
        x = x+dx
        assert(itercounter < maxiter)

    return x, itercounter

tol = 0.5e-8
x_bisecion,niter_bisection = bisection(func, 1, 1.5, tol)
x_newton, niter_newton = newton(func, fprim, 1.25, tol)
print(x_bisecion, niter_bisection)
print("-----------")
print(x_newton, niter_newton)

