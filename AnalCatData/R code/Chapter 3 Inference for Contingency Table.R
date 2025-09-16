#### Inference for odds ratio ####
x <- matrix(c(985, 273, 726, 352), 2, 2)
oddsratio(x, log = TRUE) # log(odds ratio) 
loddsratio(x) # also log(odds ratio)
## Confidence interval
confint(loddsratio(x), level = 0.95) # For log(theta)
confint(loddsratio(x), level = 0.95, log = FALSE) # For theta
confint(oddsratio(x, log = TRUE), level = 0.95, log = FALSE) # For theta

#### Test independence by Pearson chi-square #### 
x <- matrix(c(28, 18, 656, 658), 2, 2)
chisq.test(x, correct = TRUE) # With continuity correction |O - E| - 0.5
chisq.test(x, correct = FALSE)


#### Fisher exact test for 2 * 2 table ####
x <- matrix(c(3, 1, 1, 3), 2, 2)
fisher.test(x)


#### Gamma coefficient ####
x <- matrix(c(34, 80, 29, 53, 174, 75, 88, 304, 172), 3, 3)
library(DescTools)
GoodmanKruskalGamma(x = x, conf.level = 0.95)


#### Mann-Whitney test ####
Data <- rbind(matrix(c(1, 1), 34, 2, byrow = TRUE),
              matrix(c(1, 2), 53, 2, byrow = TRUE),
              matrix(c(1, 3), 88, 2, byrow = TRUE),
              matrix(c(2, 1), 80, 2, byrow = TRUE),
              matrix(c(2, 2), 174, 2, byrow = TRUE),
              matrix(c(2, 3), 304, 2, byrow = TRUE))
colnames(Data) <- c("Age", "Satisfaction")
Data <- data.frame(Data)
wilcox.test(Satisfaction ~ Age, data = Data, alternative = "two.sided")


#### Kruskall-Wallis Test
Data <- rbind(matrix(c(1, 1), 34, 2, byrow = TRUE),
              matrix(c(1, 2), 53, 2, byrow = TRUE),
              matrix(c(1, 3), 88, 2, byrow = TRUE),
              matrix(c(2, 1), 80, 2, byrow = TRUE),
              matrix(c(2, 2), 174, 2, byrow = TRUE),
              matrix(c(2, 3), 304, 2, byrow = TRUE),
              matrix(c(3, 1), 29, 2, byrow = TRUE),
              matrix(c(3, 2), 75, 2, byrow = TRUE),
              matrix(c(3, 3), 172, 2, byrow = TRUE))
colnames(Data) <- c("Age", "Satisfaction")
Data <- data.frame(Data)
wilcox.test(Satisfaction ~ Age, data = Data, alternative = "two.sided")
kruskal.test(Satisfaction ~ Age, data = Data)
