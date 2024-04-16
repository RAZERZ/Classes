data(chickwts)
chickwts
LR <- lm(weight ~ feed, data = chickwts)
summary(LR)
X <- model.matrix(LR)

## Quantitative data
chickwts2 <- chickwts
chickwts2$feed <- as.numeric(chickwts2$feed)
lm(weight ~ feed, data = chickwts2)
lm(weight ~ factor(feed), data = chickwts2)
# Or declare factors before fitting lm()
chickwts2$feed <- factor(chickwts2$feed)
lm(weight ~ feed, data = chickwts2)
## Change the reference level
lm(weight ~ relevel(feed, ref = "6"), data = chickwts2)

## Test one linear combination
## horsebean - linseed = 0
a <- matrix(c(0, 1, -1, 0, 0, 0), ncol = 1)
Num <- t(a) %*% coef(LR) / sqrt(t(a) %*% solve(t(X) %*% X) %*% a)
Den <- sqrt(sum(resid(LR) ^ 2) / (71 - 6))
Num / Den # t statistic
qt(0.95, 71 - 6)

## Subsetting
subset(UN11, UN11$group == "other")

lm(lifeExpF ~ log(ppgdp) * group, data = UN11)
lm(lifeExpF ~ group * log(ppgdp), data = UN11)
