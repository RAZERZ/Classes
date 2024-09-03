library(NISTnls)
data(Chwirut1)
plot(y ~ x, Chwirut1)
## Newton Gauss approach
NLS <- nls(y ~ exp(-a * x) / (b + c * x), data = Chwirut1, 
           start = list(a = 0.03, b = 0.01, c = 0.02) # specify initial guess
           )
summary(NLS)

## Newton Raphson algorithm
RSS <- function(theta) {
    a <- theta[1]
    b <- theta[2]
    c <- theta[3]
    sum((Chwirut1$y - exp(-a * Chwirut1$x) / (b + c * Chwirut1$x)) ^ 2)
}
optim(par = c(0.03, 0.01, 0.02), # initial guess,
      fn = RSS, method = "BFGS")

load("Nonlinear regression.RData")
## Generalized additive model
library(mgcv)
plot(y ~ x, data = Data)
Model <- gam(y ~ s(x), data = Data, family = gaussian)
summary(Model)
plot(Model)
## default setting
s(x, k = 10, bs = "tp")
## Check the default setting
gam.check(Model)
Model <- gam(y ~ s(x, bs = "tp", k = 5), data = Data, family = gaussian)
plot(Model)
## Extract predicted value
predict(Model)
predict(Model, newdata = data.frame(x = 10))

## Kernel regression (Univariate x)
KS <- ksmooth(Data$x, Data$y, kernel = "normal")
plot(y ~ x, data = Data)
lines(KS, col = 2, lwd = 1.5)

## Local polynomial regression
library(ggplot2)
ggplot(Data, aes(x = x, y = y)) + geom_point() + 
    geom_smooth(method = "loess")
Loc <- loess(y ~ x, data = Data, degree = 2, span = 0.1) # span has the same function has bandwidth
predict(Loc)
plot(y ~ x, data = Data)
points(Data$x, predict(Loc), col = 2)

## Gaussian process
library(kernlab)
GPR <- gausspr(y ~ x, data = Data, type = "regression", 
               kpar = list(sigma = 10), 
               kernel = "rbf") # Gaussian kernel
plot(y ~ x, data = Data)
points(Data$x, predict(GPR, Data), col = 2)

## 2D data
load("Nonlinear regression (2D).RData")
Model <- gam(y ~ s(x1) + s(x2), data = Data, family = gaussian)
Model <- gam(y ~ te(x1, x2, k = c(20, 20)), data = Data, family = gaussian)
## Illustrate the fitted surface
N <- 50
x1 <- seq(-5, 5, length = N)
x2 <- seq(-5, 5, length = N)
Newdata <- expand.grid(x1, x2)
names(Newdata) <- c("x1", "x2")
Pred <- predict(Model, newdata = Newdata)
Pred <- matrix(Pred, N, N)
library(plotly)
plot_ly(x = ~x1, y = ~x2, z = ~Pred) %>% add_surface()
