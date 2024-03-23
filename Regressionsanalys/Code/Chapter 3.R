data(penguins, package = "palmerpenguins")
head(penguins)
## Visulization
library(GGally)
ggpairs(data = penguins)

## Multiple linear regression
LR <- lm(bill_length_mm ~ flipper_length_mm + body_mass_g, data = penguins)
## Extract fitted object
summary(LR) 
## Residuals ( y - x betahat)
resid(LR)
residuals(LR)
## In sample fitted value
fitted(LR)
predict(LR)
## Out-of-sample prediction
predict(LR, newdata = data.frame(flipper_length_mm = 100,
                                 body_mass_g = 4000))
## Coefficients
coef(LR)
-3.4366939 + 0.2218655 * 100 + 0.0006622 * 4000
