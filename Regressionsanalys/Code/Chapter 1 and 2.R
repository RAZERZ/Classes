## Simple linear regression (one y and one x)
data(penguins, package = "palmerpenguins")
head(penguins)
## Visulization
## M1: 
plot(bill_length_mm ~ bill_depth_mm, data = penguins) # y ~ x
plot(penguins$bill_depth_mm, penguins$bill_length_mm) # x, y
## M2: if we have a data frame
library(ggplot2)
ggplot(data = penguins, aes(bill_depth_mm, bill_length_mm)) +
    geom_point()
ggplot(data = penguins, aes(flipper_length_mm, bill_length_mm)) +
    geom_point()

## Simple linear regression
LR <- lm(y ~ x, data = name) # OLS estimation
LR <- lm(bill_length_mm ~ flipper_length_mm, data = penguins)
summary(LR) # Extract information
