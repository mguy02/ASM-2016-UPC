#Input: Vectors x and y.
#Output: The estimation of σ 2 using
#Gasser, Sroka, and Jennen-Steinmetz (1986) method.

GSJS.estimator <- function(x,y)
{
  data <- cbind(x,y)
  if (is.unsorted(x, na.rm = F, strictly = F))
  {
    data <- data[order(x),]
  }
  
  x <- data[,1]
  y <- data[,2]
  
  a <- c()
  b <- c()
  epsilon <- c()
  n <- length(y)
  
  for (i in 2:(n-1)) 
  { 
    if (x[i-1] == x[i] && x[i] == x[i+1])
    {
      x.u <- min(x[x > x[i]])
      x.l <- max(x[x < x[i]])
      
      a[i] <- (x.u-x[i])/(x.u-x.l)
      b[i] <- (x[i]-x.l)/(x.u-x.l)
    }
    else
    {
      a[i] <- (x[i+1]-x[i])/(x[i+1]-x[i-1])
      b[i] <- (x[i]-x[i-1])/(x[i+1]-x[i-1])
      
    }
    epsilon[i] <- a[i]*y[i-1] + b[i]*y[i+1] - y[i]
  }  
  
  s <- 0
  
  for (i in 2:(n-1))
  {
    #print(epsilon[i]**2)
    #print((a[i]**2 + b[i]**2 + 1))
    s <- s + epsilon[i]**2 / (a[i]**2 + b[i]**2 + 1)
  }
  
  s <- s * 1/(n-2)
  
  s
  
  
}