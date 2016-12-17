# h.cv.sm.poisson.R
#
# December 4, 2016 - Advanced Statistical Modeling
# 
# Bandwidth choice in the local Poisson regression 
# by leave-one-out cross-validation.
# Function "sm.poisson", from library "sm", is used.
#
# From the h.cv.sm.binomial.R of Pedro Delicado
# 
# Martin Guy and Hannes Leskela


loglik.CV <- function(x,y,h){
  n <- length(x)
  pred <- sapply(1:n, 
      function(i,x,y,h){
         sm.poisson(x=x[-i],y=y[-i],h=h,eval.points=x[i],display="none")$estimate
      },   x,y,h)
  return(sum( y*log(pred) - n*pred ))
}

# method can be equal to 'loglik.CV' (default) or 'prob.missclas.CV'
h.cv.sm.poisson <- function(x,y,rg.h=NULL,l.h=10,method=loglik.CV){
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

