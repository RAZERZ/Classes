#### Generate fake data
set.seed(123456)
x <- seq(0, 4 * pi, length.out = 200)
y <- sin(x) + rnorm(200, 0, 0.3)
plot(x, y, cex.lab = 1, cex.axis = 1, pch = 16)

#### Fit GAM  ####
library(mgcv)
Data <- data.frame(x = x, y = y)
Gam.Fit <- mgcv::gam(y ~ s(x, k = 50), data = Data, family = gaussian)
summary(Gam.Fit)
plot(Gam.Fit, shade = TRUE, seWithMean = TRUE) # Plot the smooth function
## Whether the basis function dimension is enough
s(x)
s(x, bs = "tp", k = 10)
# Default
gam.check(Gam.Fit)
?smooth.terms
## In sample prediction
predict(Gam.Fit)
## Out-of-sample prediction
predict(Gam.Fit, newdata = data.frame(x = 0.19))
## Choosing tuning parameter
Gam.Fit <- mgcv::gam(y ~ s(x), data = Data, family = gaussian, method = "GCV.Cp")
Gam.Fit <- mgcv::gam(y ~ s(x), data = Data, family = gaussian, method = "REML")
## Residual analysis
library(DHARMa)
SimulateRes <- simulateResiduals(Gam.Fit, n = 1000, plot = FALSE)
plot(SimulateRes)

#### 2D GAM ####
load("C:/Users/shaji948/Box/Teaching/Math Department/Computer intensive statistics and applications 1MS049/R/Nonlinear regression (2D).RData")
library(plotly)
plot_ly(Data, x = ~x1, y = ~x2, z = ~y, type = "scatter3d", mode = "markers", marker = list(size = 2))
m1 <- gam(y ~ s(x1, x2, k = 100), data = Data) # or te(x1, x2, k = ?)
plot(m1, se = FALSE)
x1new <- seq(-6, 6, length.out = 30)
x2new <- seq(-6, 6, length.out = 30)
xnew <- expand.grid(x1new, x2new)
colnames(xnew) <- c("x1", "x2")
predy <- predict(m1, newdata = xnew)
plot_ly(x = x1new, y = x2new, z = matrix(predy, length(x1new), length(x2new), byrow = TRUE)) %>% add_surface()

m2 <- gam(y ~ ti(x1) + ti(x2) + ti(x1, x2), data = Data) # ti() only models the interaction
plot(m2, se = FALSE)


