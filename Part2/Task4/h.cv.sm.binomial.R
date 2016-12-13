# h.cv.sm.binomial.R
#
# Bandwidth choice in the local logistic regression 
# by leave-one-out cross-validation.
# Function "sm.binomial", from library "sm", is used.
#
# Pedro Delicado

prob.missclas.CV <- function(x,y,h){
  n <- length(x)
  pred <- sapply(1:n, 
      function(i,x,y,h){
         sm.binomial(x=x[-i],y=y[-i],h=h,eval.points=x[i],display="none")$estimate
      },   x,y,h)
  return(sum(abs(pred-y)>.5)/n)
}

loglik.CV <- function(x,y,h){
  n <- length(x)
  pred <- sapply(1:n, 
      function(i,x,y,h){
         sm.binomial(x=x[-i],y=y[-i],h=h,eval.points=x[i],display="none")$estimate
      },   x,y,h)
  return(-sum( y*log(pred/(1-pred)) + log(1-pred) ))
}

# method can be equal to 'loglik.CV' (default) or 'prob.missclas.CV'
h.cv.sm.binomial <- function(x,y,rg.h=NULL,l.h=10,method=loglik.CV){
   cv.h <- numeric(l.h)
   if (is.null(rg.h)){
      hh <- c(h.select(x,y,method="cv"), h.select(x,y,method="aicc"))#,hcv(x,y))
      rg.h <- range(hh)
   }
   i <- 0
   gr.h <- exp( seq(log(rg.h[1]/1.1), log(rg.h[2]*1.1), l=l.h))
   for (h in gr.h){
      i <- i+1
      cv.h[i] <- method(x,y,h)
   }
   return(list(h=gr.h,cv.h=cv.h, h.cv = gr.h[which.min(cv.h)]))
}