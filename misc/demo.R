### A demo of what you can do with the package
library(MFA)
result <- mfa(wines, sets)
result

PlotFactorScores(result, group = wines$country, label = wines$ID)
PlotPartialFactorScores(result, 1, group = wines$country, label = wines$ID)
PlotLoadings(result, label = colnames(wines[, unlist(sets)]))

RV_table(wines, sets)
Lg_table(wines, sets)

BootstrapRatios(result, times = 10000)
