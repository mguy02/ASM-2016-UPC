# locpolreg.R Local polynomial regression for estimateing the 
#             regression function or its r-th derivative
#            
#           Changes in this version:
#              * Epanechnikov kernel is replaced by normal kernel
#              * Re-scaled Epanechnikov kernel is added as an option
#              * Data are not sorted at the beginning of the function
#
# Input: 
#      x,y  Observed data (two (n,1) vectors)
#      h    Smoothing parameter 
#      p    degree of the local polynomial to be fitted (default: 1)
#      r    order of the derivative to be estimate (Default: 0, the function)
#      tg   grid of values t where the estimated regression function 
#           is evaluated (default: x)
#      type.kernel "normal"  (Gaussian, default), 
#                  "epan"    (Epanechnikov) or 
#                  "rs.epan" (re-scaled Epanechnikov)
#
# Output:  An object with two elements, 
#      mtg  Estimated values of the r-th derivative of the regression function at points in vector tg
#      S    The S smoothing matrix
#
# Uso:
#      result <- locpolreg(x,y,h,p,r,tg)
# 
locpolreg <- function(x,y,h=(max(x)-min(x))/5,p=1,r=0,tg=NULL,type.kernel="normal",
                      nosubplot=F,doing.plot=T, ...){
    if (is.null(tg)){tg<-x}                  
#   if (sum(diff(tg)<0)>0) {
      aux <- sort(tg,index.return=T)
      sorted.tg <- tg[aux$ix]
      sorted.tg.ix <- aux$ix
#   } else {
#      sorted.tg <- tg
#      sorted.tg.ix <- 1:length(tg)   
#   }
      
   n <- length(x);
   m <- length(tg);
   mtgr <- seq(1,m)*0;
   S <- matrix(0,nrow=m,ncol=n)

   factr <- max(1,prod(1:r))

   for (i in seq(1,m)){
      aux <- kernel((x-tg[i])/h,type=type.kernel);
      Ih <- (aux>0);
      n <- sum(Ih);     
      xh <- x[Ih]-tg[i];
      Dp <- matrix(1,nrow=n,ncol=p+1);
      if (p>0){for (j in 1:p) Dp[,j+1] <- xh^j}
      Wx <- kernel(xh/h,type=type.kernel)/h;
      Wm <- Wx%*%ones(1,p+1);
      Dpp <- Wm*Dp;
      Si <- solve(t(Dp)%*%Dpp)%*%t(Dpp);
      beta <- Si%*%y[Ih];
      mtgr[i] <- factr*beta[r+1];
      S[i,Ih] <- Si[r+1,]
   }
  
   if (doing.plot){
      if (r==0){
         if (nosubplot) par(mfrow=c(1,1))
         plot(x,y,col="grey",...)
         lines(sorted.tg,mtgr[sorted.tg.ix],col=1,lwd=2)
      } 
      else{
         par(mfrow=c(2,1))
         aux <- locpolreg(x,y,h,p,0,tg,nosubplot=F,type.kernel,...)
         plot(sorted.tg,mtgr[sorted.tg.ix],ty="l",col=1,lwd=2)
         abline(h=0)
      }
   }
return(list(mtgr=mtgr,S=S))
}

epan <- function(x){pmax(.75*(x+1)*(1-x))}
kernel <- function(x,type=c("normal","epan","rs.epan")){
   switch(type[1],
          epan = pmax(.75*(x+1)*(1-x),0),
          rs.epan = pmax(.75*(x/sqrt(5)+1)*(1-x/sqrt(5))/sqrt(5),0),
          dnorm(x))
}
ones <- function(n,m){matrix(1,nrow=n,ncol=m)}
