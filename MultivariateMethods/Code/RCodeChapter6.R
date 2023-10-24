#### Paired t test based on T2 ####
p <- 3
D <- Data[, 1 : 3] - Data[, 4 : 6]
library(DescTools)
HotellingsT2Test(x = D, mu = c(0, 0, 0), test = "f")
library(MVTests)
Mpaired(T1 = Data[, 1 : 3], T2 = Data[, 4 : 6])

#### Two sample T2 test ####
library(MVTests)
TwoSamplesHT2(data = rbind(as.matrix(DF1),
                           as.matrix(DF2)),
              group = c(rep(1, 200),
                        rep(2, 300)),
              alpha = 0.05,
              Homogenity = TRUE) # Same covariance matrix if TRUE

#### One way MANOVA ####
data(iris)
unique(iris$Species)
## ANOVA (p = 1)
ANOVA <- aov(Sepal.Length ~ Species, data = iris)
summary(ANOVA)
## MANOVA (p = 4)
MANOVA <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species,
                 data = iris)
summary(MANOVA, test = "Wilks")
summary(MANOVA)

#### Two way MANOVA ####
MANOVA2 <- manova(cbind(V1, V2) ~ F1 * F2,
                  data = Data)
summary(MANOVA2)
summary(MANOVA2, test = "Wilks")
