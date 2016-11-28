## problem 2

library(tseries)
SNPdata = get.hist.quote('gwph', quote='Close')
summary(SNPdata)

SNPreturns = log()