library('FKF')

stochasticVar <- function(mu, gamma, phi, sigma)
{
  Tt <- matrix(phi)
  Zt <- matrix(1)
  ct <- matrix(-1.27+mu)
  dt <- matrix(gamma)
  GGt <- matrix(pi*pi*0.5)
  Ht <- matrix(sqrt(exp(sigma)))
  HHt <- Ht %*% t(Ht)
  a0 <- c(0.199)
  P0 <- matrix((exp(sigma)^2)/(1-(phi^2)), nrow = 1, ncol = 1)
  return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt,HHt = HHt))
}

## The objective function passed to 􏰀optim􏰀
objectiveSV <- function(theta, yt) {
  sp <- stochasticVar(theta["mu"], theta["gamma"], theta["phi"], theta["sigma"])
  ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
             Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
  return(-ans$logLik)
}

fitStochasticVarianceModel <- function(returns)
{
  yt <- log(returns^2)
  yt <- apply(yt, 1, function(x) ifelse(is.finite(x), x, NA))
  ytm <- matrix(as.vector(yt),nrow=1)
  
  theta <- c(mu = 20, gamma = 0.5, phi = 0.9, sigma = 0.1)
  fit <- optim(theta, objectiveSV, yt = ytm, hessian = TRUE)
  fit$par
  
  ## Confidence intervals
  rbind(fit$par - qnorm(0.975) * sqrt(diag(solve(fit$hessian))), fit$par + qnorm(0.975) * sqrt(diag(solve(fit$hessian))))
  
  ## Filter the series with estimated parameter values
  sp <- stochasticVar(fit$par["mu"], fit$par["gamma"], fit$par["phi"], fit$par["sigma"])
  ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt, Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = ytm)
  
  return(list(fkf = ans, coeff=fit))
}

getStochasticVol <- function(fit, rets)
{
  daily.sigma <- exp(fit$at) 
  daily.sigma <- daily.sigma[-1]
  return( xts(sqrt(252) * sqrt(daily.sigma), order.by=index(rets)) )
}
