library(quantmod)
library(qmao)
library(devtools)
library(Quandl)
library(ggplot2)
library(lmtest)
library(bizdays)

foldername <- "/Users/swatimital/Documents/Oxford MSc Mathematics/Module 7/Assignment/VIX Strategy/"

productOfXts <- function(xts.1, xts.2)
{
  if (ncol(xts.1) != ncol(xts.2)) stop("XTS Objects must have the same columns")
  if (nrow(xts.1) < nrow(xts.2))
  {
    missing.dates <- index(xts.2)[which(!(index(xts.2) %in% index(xts.1)))]
    missing.xts.1 <- xts(matrix(NA,nrow=length(missing.dates), ncol=ncol(xts.1)), order.by=missing.dates)
    merged.xts.1 <- c(xts.1, missing.xts.1)
    
    xts.1.data <- coredata(merged.xts.1)
    xts.2.data <- coredata(xts.2)
    product.data <- xts(rowSums(xts.1.data * xts.2.data,na.rm=TRUE), order.by=index(xts.2))
    product.data.trimmed <- product.data[product.data!=0]
  }
  return(product.data.trimmed)
}

getUnadjustedFrontVIXFutures <- function(fromYear, toYear, front.n)
{
  VIX.symbols <- createVIXSymbols(fromYear, toYear)

  #https://www.quantstart.com/articles/Continuous-Futures-Contracts-for-Backtesting-Purposes
  VIX.front.future <- c()
  for (sym in VIX.symbols)
  {
    VIX.sym <- eval(as.name(sym))
    n <- length(index(VIX.sym))
    VIX.close <- VIX.sym[,4]
    VIX.front.future <- rbind(VIX.front.future, na.omit(VIX.close[n-front.n:n]))
  }
  
  colnames(VIX.front.future) <- "Close"
  VIX.front <- VIX.front.future[!duplicated(index(VIX.front.future))]
  return (VIX.front)
}

getVIXFuturesXTSObject <- function(fromYear, toYear)
{
  VIX.symbols <- createVIXSymbols(fromYear, toYear)
  VIX.xts = eval(as.name(VIX.symbols[1]))[,4]
  for (sym in VIX.symbols[-1])
  {
    VIX.sym <- eval(as.name(sym))
    VIX.xts <- merge(VIX.xts, VIX.sym[,4])
  }
  return (VIX.xts)
}

startDateOfVIXFuture <- function(startYear)
{
  VIX.symbols <- createVIXSymbols(startYear, startYear)
  start.date <- lapply(VIX.symbols, function(x) { min(index(eval(as.name(x)))) })
  return (as.Date(min(unlist(start.date))))
}

regressOnLaggedVIXBasis <- function(VIX.object, VIX.basis)
{
  VIX.object.delta <- diff(VIX.object)[-1]
  VIX.basis <- VIX.basis[-(nrow(VIX.basis))]
  output <- lm (VIX.object.delta ~ VIX.basis)
  dwOutput <- dwtest(VIX.object.delta ~ VIX.basis)
  
  plot(as.vector(VIX.object.delta[,1]), as.vector(VIX.basis$Close), xlab='VIX Basis', ylab='VIX Object Delta', col='blue')
  abline(output, lwd=2)
  
  return (list(regression = output, dwtest = dwOutput))
}

regressOnSingleTS <- function(y.ts, x.ts)
{
  print(cor(y.ts, x.ts))
  output <- lm(y.ts ~ x.ts)
  plot(as.vector(y.ts[,1]), as.vector(x.ts[,1]), xlab='Independent Variable', ylab='Y vals', col='green')
  abline(output, lwd=2)
  return (output)
}

computeTimeSeriesReturn <- function(ts)
{
  ts.delta <- as.vector(diff(ts[,1])[-1])
  ts.rtn <- 100*(ts.delta/as.vector(ts[-nrow(ts)]))
  return (xts(ts.rtn, order.by=index(ts)[-1]))
}

computeTimeSeriesLogReturn <- function(ts)
{
  return (xts(diff(log(ts)), order.by=index(ts)[-1]))
}

##############################
# Download data
# The quandl VX1 data has very high values for 2005 and 2006 VIX futures
VIX.front <- Quandl("CHRIS/CBOE_VX1")
plot(VIX.front$`Trade Date`, VIX.front$Close, type='l', col='blue')
fromYear <- 2006
toYear <- 2015
# Get VIX Futures prices
getSymbols("VX",Months=1:12,Years=fromYear:toYear,src='cfe')
#Get VIX data
getSymbols("^VIX", from="2005-01-01", to="2016-03-01")
# Get front mini-S&P Future from Quandl
SP.front.future <- Quandl("CHRIS/CME_ES1")
plot(SP.front.future$`Date`, SP.front.future$Last, type='l', col='blue')
#############################

#Get VIX front future
VIX.front.future <- getUnadjustedFrontVIXFutures(fromYear, toYear, front.n=10)
VIX.xts <- getVIXFuturesXTSObject(fromYear, toYear)

#Compute continuous VIX futures
start.date <- startDateOfVIXFuture(fromYear)
VIX.expiries <- expiryDatesVIXFuturesPair(fromYear, toYear)
rollover.days <- 20
rollover.weights <- futuresRollOverWeights(start.date, VIX.expiries, rollover.days)
VIX.adjusted.futures <- productOfXts(VIX.xts, rollover.weights)
plot(VIX.adjusted.futures)
VIX.front.future <- VIX.adjusted.futures

VIX.adjusetd.BBG <- read.csv(paste(foldername, "VIX-adjusted.csv",sep=""), sep=',')
VIX.bbg.dates <- as.Date(VIX.adjusetd.BBG$Date,format="%d/%m/%y")
VIX.bbg.prices <- VIX.adjusetd.BBG$PX_LAST

VIX.front.future <- xts(VIX.bbg.prices, order.by=VIX.bbg.dates)
# Filter by VIX futures dates
VIX.subset <- VIX$VIX.Close[index(VIX.front.future)]
VIX.front.future.subset <- VIX.front.future[match(index(VIX.subset), index(VIX.front.future))]

# Compute the VIX Futures Basis
VIX.basis <- xts(as.vector(VIX.front.future.subset$Close)-as.vector(VIX.subset$VIX.Close), order.by = index(VIX.front.future.subset))
colnames(VIX.basis) <- c('Close')

# Plot VIX and roll
plot.xts(x = VIX.basis, ylim=c(-50,100), xlab='Time')
abline(h=0, untf=FALSE, col='black')
lines(x = VIX.subset, col = "blue")
legend(x = 'topleft', legend = c("VIX", "Basis"), lty = 1, col = c("Blue", "Black"))

# Regress on VIX basis
outputVIXonVIXBasis <- regressOnLaggedVIXBasis(VIX.subset, VIX.basis)
outputVIXFutureonVIXBasis <- regressOnLaggedVIXBasis(VIX.front.future.subset, VIX.basis)
summary(outputVIXonVIXBasis$regression)
summary(outputVIXFutureonVIXBasis$regression)

# S&P versus VIX Futures regression
SP.xts <- xts(SP.front.future$Last, order.by=SP.front.future$Date)
SP.rtn <- computeTimeSeriesReturn(SP.xts)

SP.rtn.filtered <- SP.rtn[index(VIX.front.future)]
VIX.future.filtered <- VIX.front.future[match(index(SP.rtn.filtered), index(VIX.front.future))]
VIX.delta.filtered <- diff(VIX.future.filtered)
head(SP.rtn.filtered); head(VIX.delta.filtered)

outputVIXFutureOnSPRET <- regressOnSingleTS(VIX.delta.filtered[-1], SP.rtn.filtered[-1])
summary(outputVIXFutureOnSPRET)


common.dates <- as.Date(Reduce(intersect, list(index(VX_G07), index(VX_H07), index(VX_F07))))
plot(VX_F07$VX_F07.Close[common.dates], ylim=c(10, 18), xlab='Dates', ylab='Close VIX Futures', main='Close Prices')
lines(VX_G07$VX_G07.Close[common.dates], col='blue')
lines(VX_H07$VX_H07.Close[common.dates], col='red')
legend(x = 'topleft', legend = c("Jan07", "Feb07", "March07"), lty = 1, col = c("Black", "Blue", "Red"))
