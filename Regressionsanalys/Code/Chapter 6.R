## F test
data(penguins, package = "palmerpenguins")
penguins <- na.omit(penguins) # Remove NA values
## Example 1
LR <- lm(bill_length_mm ~ bill_depth_mm + flipper_length_mm + body_mass_g,
         data = penguins)
summary(LR)
## Example 2
LR0 <- lm(bill_length_mm ~ bill_depth_mm,
         data = penguins)
summary(LR0)
(-4.273) ^ 2
## Example 3: Your own F statistic
RSS1 <- sum(resid(LR) ^ 2)
RSS0 <- sum(resid(LR0) ^ 2)
Num <- (RSS0 - RSS1) / 2
Den <- RSS1 / (333 - 4)
Num / Den # F value
qf(0.95, 2, 333 - 4) # Critical value

## ANOVA
aggregate(flipper_length_mm ~ species, data = penguins, FUN = "mean")
AOV <- aov(flipper_length_mm ~ species, data = penguins)
summary(AOV)
LR <- lm(flipper_length_mm ~ species, data = penguins)
summary(LR)
## Correct multiple testing
TukeyHSD(AOV) # Tukey's HSD
p.value <- c(1.25e-08, 2e-16, 0.03, 0.06)
p.adjust(p.value, method = "holm") # bonferroni, hochberg

## Two way anova
AOV <- aov(flipper_length_mm ~ species + sex, data = penguins)
summary(AOV)
AOV <- aov(flipper_length_mm ~ species * sex, data = penguins)
summary(AOV)
## Type I ANOVA
AOV1 <- aov(flipper_length_mm ~ species + sex, data = penguins)
summary(AOV1)
AOV2 <- aov(flipper_length_mm ~ sex + species, data = penguins)
summary(AOV2)
## Type II ANOVA
library(car)
LR1 <- lm(flipper_length_mm ~ species + sex, data = penguins)
Anova(LR1)
LR2 <- lm(flipper_length_mm ~ sex + species, data = penguins)
Anova(LR2)
