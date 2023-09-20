X11()
plot.new()
#### 3D density ####
library(ggplot2)
library(plotly)
x1 <- seq(-3, 3, by = 0.05)
x2 <- seq(-3, 3, by = 0.05)
density <- matrix(NA, 121, 121)
Rho <- 0
for(i in 1 : 121){
    for(j in 1 : 121){
        density[i, j] <- mvtnorm::dmvnorm(c(x1[i], x2[j]), 
                                          mean = c(0, 0),
                                          sigma = matrix(c(1, Rho,
                                                           Rho, 1), 2, 2, byrow = TRUE))
    }
}
plotly::plot_ly(x = x1, y = x2, z = density) %>% add_surface()
locator(1)

#### Evaluate normality ####
## Visual check
hist(x1)
locator(1)
hist(x2)
locator(1)
plot(density(x1))
locator(1)
plot(density(x2))
locator(1)
library(car)
qqPlot(x1)
locator(1)
qqPlot(x2)
locator(1)
## Univariate test?
library(moments)
skewness(x1); skewness(x2) ## Normal: 0
locator(1)
kurtosis(x1); kurtosis(x2) ## Normal: 3
locator(1)
shapiro.test(x1) # H0: Normal
shapiro.test(x2)
library(nortest)
ad.test(x1)
ad.test(x2)
ks.test(x1, "pnorm") # Another alternative
## Multivariate test
library(MVN)
BiV <- cbind(x1 = x1,
             x2 = x2)
mvn(BiV, univariatePlot = "histogram")
locator(1)
mvn(BiV, univariatePlot = "qqplot")
locator(1)
mvn(BiV, mvnTest = "royston")
locator(1)
mvn(BiV, mvnTest = "mardia")
locator(1)
