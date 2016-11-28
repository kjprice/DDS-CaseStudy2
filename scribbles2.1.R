SNPreturns = log(lag(SNPdata)) - log(SNPdata)

getVol = function(d, logrets) {
  var = 0
  lam = 0
  varlist = c()
  for (r in logrets) {
    lam = lam*(1 - 1/d) + 1
    var = (1 - 1/lam)*var + (1/lam) * r^2
    varlist = c(varlist, var)
  }
  
  sqrt(varlist)
}


volest = getVol(10, SNPreturns)
volest2 = getVol(30, SNPreturns)
volest3 = getVol(100, SNPreturns)


plot(volest, type="l")
lines(volest2, type="l", col='red')
lines(volest3, type="l", col='green')


## Problem 3

