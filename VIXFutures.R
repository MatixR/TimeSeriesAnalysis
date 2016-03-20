library(bizdays)

createVIXSymbols <- function(fromYear, toYear)
{
  fromYearYY <- substring(fromYear,3,4)
  toYearYY <- substring(toYear,3,4)
  
  VIX.years <- formatC(fromYearYY:toYearYY, width=2, flag="0")
  VIX.months <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
  VIX.symbols <- unlist(lapply(VIX.years, function(y) { paste("VX_", VIX.months, y, sep="") }))
  
  return (VIX.symbols)
}


expiryDatesVIXFuturesPair <- function(fromYear, toYear)
{
  VIX.symbols <- createVIXSymbols(fromYear, toYear)
  VIX.expiry <- c()
  for (sym in VIX.symbols)
  {
    VIX.sym <- eval(as.name(sym))
    VIX.expiry <- c(VIX.expiry, tail(index(VIX.sym),1))
  }
  
  expiry.dates <- data.frame(Symbols=VIX.symbols, Date=VIX.expiry)
  return (expiry.dates)
}

futuresRollOverWeights <- function(start.date, vix.expiries, rollover.days)
{
  expiry.dates <- vix.expiries$Date
  contracts <- vix.expiries$Symbols
  biz.dates <- bizseq(from=start.date, to=as.Date(expiry.dates[length(expiry.dates)]))
  roll.weights <- xts(matrix(0, nrow=length(biz.dates), ncol=length(contracts)), order.by=biz.dates)
  colnames(roll.weights) <- contracts
  decay.weights <- seq(0,1,by=1/rollover.days)
  
  prev.date <- start.date
  for (row in 1:(length(expiry.dates)-1))
  {
    item.expiry <- as.Date(expiry.dates[row])
    item.sym <- as.character(contracts[row])
    item.next.sym <- as.character(contracts[row+1])
    
    item.dates <- bizseq(prev.date,item.expiry)
    item.roll.dates <- tail(item.dates, rollover.days + 1)
    roll.weights[item.dates,item.sym] <- 1
    roll.weights[item.roll.dates,item.sym] <- t(rep(1,rollover.days + 1) - decay.weights)
    roll.weights[item.roll.dates,item.next.sym] <- t(decay.weights)  
    prev.date <- offset(item.expiry,1)
  }
  
  item.expiry <- as.Date(expiry.dates[length(expiry.dates)])
  item.sym <- as.character(contracts[length(expiry.dates)])
  item.dates <- bizseq(prev.date,item.expiry)
  roll.weights[item.dates,item.sym] <- 1
  
  return (roll.weights)
}
