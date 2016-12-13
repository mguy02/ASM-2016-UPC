#ASM Part 2 Task 3 : Bandwidth choice

set.seed(1970)

library("sm")
library("KernSmooth")

source("CVfunctions.R")


load("boston.Rdata")

x <- boston.c$LSTAT
y <- boston.c$RM
N <- length(y)

(h.v <- exp( seq(from=log(.5), to = log(15), length=12) ))

methods <- c("h.select", "dpill", "Leave-one-out CV", "5 folds CV", "10 folds CV", "Generalized CV")

bandwidth <- data.frame(method=methods, best.h=NA)

best.h <- c(h.select(x,y, method="cv"), dpill(x,y))

looCV <- PMSE.CV(x,y, h.v)
fold5CV <- PMSE.kCV(x,y, h.v, k=5)
fold10CV <- PMSE.kCV(x,y, h.v, k=10)
GCV <- PMSE.GCV(x,y, h.v)


best.h[3] <- find.best.h(h.v, looCV)
best.h[4] <- find.best.h(h.v, fold5CV)
best.h[5] <- find.best.h(h.v, fold10CV)
best.h[6] <- find.best.h(h.v, GCV)
bandwidth$best.h = best.h


{
h.log = log(h.v)
  
plot(h.log, looCV, ylim=c(0.25,0.35), main="Plots of bandwidth estimators", ylab="h.error(h)")
lines(h.log, looCV)
lines(h.log, fold10CV, col="blue")
lines(h.log, fold5CV, col="red")
lines(h.log, GCV, col="green", lwd=2)
legend(x="topleft",c("5-fold CV","10-fold CV","GCV","LOOCV"),lty=c(1,1,1,1),lwd=c(2.5,2.5),col=c("red","blue","green","black"))
}