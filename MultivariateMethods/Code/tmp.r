#### 3D density

library(gpplot2)
library(plotly)

x1 <- seq(-3,3,by = 0.05) 
x2 <- seq(-3,3,by = 0.05) 
density <- matrix(NA,121,121)
Rho <- 0.6

for(i in 1:121) {
  for(j in 1: 121) {
    density[i,j] <- mvtnorm:dmvnorm(c(x1[i], x2[j]), mean = c(0,0), sigma = matrix(c(1, Rho, Rho, 1), 2, 2, byrow = TRUE))
  }
}

plotly::plot_ly(x=x1, y = x2, z = density) %>% add_surface()
