# Alternative residal variance estimation
# Martin Guy & Hannes Leskelä
#
# Proposal of Rice (1984)
# If one assume a model 

Rice.estimator <- function(x,y)
{
  data <- cbind(x,y)
  if (is.unsorted(x, na.rm = F, strictly = F))
  {
    data <- data[order(x),]
  }

  
  #x <- data[,1]
  y <- data[,2]
  n <- length(y)
  sum_y <- 0
  for (i in 2:n) {
    sum_y = sum_y+ (y[i]-y[i-1])^2
  }
  
  sigma_squared <- 1/(2*(n-1)) * sum_y
  
  sigma_squared
}

