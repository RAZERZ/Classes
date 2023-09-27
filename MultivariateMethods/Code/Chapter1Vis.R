load("Chapter1Vis.RData")
library(tcltk)
X11()
hist(Paper$Density)
locator(1)
plot(Paper$Density, Paper[["Machine direction"]])
locator(1)

## Scatter plot
library(ggplot2)
library(GGally)
ggpairs(Paper)
locator(1)

## Violin plots
ggplot(data = Paper, aes(x = Group, y = `Machine direction`)) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1)

locator(1)
