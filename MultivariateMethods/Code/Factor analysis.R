data(HolzingerSwineford1939, package = "lavaan")
Data <- HolzingerSwineford1939[, paste0("x", 1 : 9)]
## 
library(psych)
fa(Data, nfactors = 1, rotate = "none", fm = "pa") # Principal axis
fa(Data, nfactors = 1, rotate = "none", fm = "ml") # Maximum likelihood
## Parallel analysis for factor analysis
fa.parallel(Data)
fa(Data, nfactors = 3, rotate = "none", fm = "ml")
fa(cov(Data), nfactors = 3, rotate = "none", fm = "ml")
fa(cor(Data), nfactors = 3, rotate = "none", fm = "ml")
## ML with varimax rotation
fa(Data, nfactors = 3, rotate = "varimax", fm = "ml")
## ML with geomin rotation
fa(Data, nfactors = 3, rotate = "geominQ", fm = "ml")
## Factor scores
FA <- fa(Data, nfactors = 3, rotate = "geominQ", fm = "ml", 
         scores = "Bartlett")
FA$scores
FA <- fa(Data, nfactors = 3, rotate = "varimax", fm = "ml",
         scores = "regression")
FA$scores
FA <- fa(Data, nfactors = 3, rotate = "varimax", fm = "ml",
         scores = "Anderson") # For orthogonal rotation
FA$scores
FA$scores
FA <- fa(Data, nfactors = 3, rotate = "geominQ", fm = "ml",
         scores = "Anderson") # Still a bug
FA$scores
cov(FA$scores)
## An alternative: builtin function for FA
factanal(Data, factors = 3, rotation = "varimax")
