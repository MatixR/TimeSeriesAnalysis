foldername <- "/Users/swatimital/GitHub/TimeSeriesAnalysis/"
source(paste(foldername, "TimeSeriesData.r", sep=""))
source(paste(foldername, "StochasticVarFitting.r", sep=""))
source(paste(foldername, "GarchFitting.r", sep=""))

end <- format(Sys.Date(), "%Y-%m-%d")
start <- format(Sys.Date() - 30*365, "%Y-%m-%d")

ret.dataset <- getSPYReturns(start, end, 0.7)
plot(ret.dataset$train)

sv.fit <- fitStochasticVarianceModel(ret.dataset$train)
stochastic.vol <- getStochasticVol(sv.fit$fkf, ret.dataset$train)

garch.fit <- fitGarch11Model(ret.dataset$train)
garch.vol <- getGarchVol(garch.fit, ret.dataset$train)

plot(stochastic.vol)
plot(garch.vol)

cor(stochastic.vol, garch.vol)