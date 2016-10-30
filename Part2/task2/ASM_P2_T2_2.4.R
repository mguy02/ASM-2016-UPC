
source('GSJS.R')
source('Rice.R')

load("boston.Rdata")
x <- boston.c["LSTAT"]
y <- boston.c["RM"]

(estimation <- sqrt(GSJS.estimator(x,y)))

#comparing with package sm estimation

library(sm)

fit.loess <- loess(RM~LSTAT, data.frame(x,y))
fit.sm.regression <- sm.regression(x, y)

fit.loess$s
fit.sm.regression$sigma
estimation

(estimation <- sqrt(Rice.estimator(x,y)))
