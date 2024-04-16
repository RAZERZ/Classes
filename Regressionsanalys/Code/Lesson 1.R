## 2.1
library(alr4)
data(Htwt)
## Scatter plot 
library(ggplot2)
ggplot(Htwt, aes(x = wt, y = ht)) + geom_point() +
    geom_smooth(method = "lm")
plot(ht ~ wt, data = Htwt)
LR <- lm(ht ~ wt, data = Htwt)
summary(LR)
plot(ht ~ wt, data = Htwt)
abline(a = coef(LR)[1], b = coef(LR)[2])
confint(LR)
vcov(LR)

## 2.2
data(UBSprices)
ggplot(UBSprices, aes(x = rice2003, y = rice2009)) + geom_point() +
    geom_smooth(method = "lm")
ggplot(UBSprices, aes(x = rice2003, y = rice2009)) + geom_point() +
    geom_smooth()
plot(log(rice2009) ~ log(rice2003), data = UBSprices)

library(GGally)
ggpairs(data = UBSprices)

## 2.17
data(snake)
LR <- lm(Y ~ X - 1, data = snake) # -1 removes the intercept
summary(LR)
model.matrix(LR)
sqrt(sum(resid(LR) ^ 2) / (17 - 1)) # sigma_hat
confint(LR)
sum(resid(LR))
cor(resid(LR), fitted(LR))

## 3.5
lm(y ~ x1, data = Data)
lm(y ~ x2, data = Data)
lm(y ~ x3, data = Data)
lm(y ~ x1 + x2 + x3, data = Data)
X <- as.matrix(Data[, c("x1", "x2", "x3")])
t(X) %*% X
lm(y ~ ., data = Data) # if lots of x
