#CVfunctions.R
#Cross validations functions for bandwith

set.seed(1970)
source('locpolreg.R')

#perform the i-th fold CV
CV.i.fold <- function(i, h, k, data)
{
  N <- length(data$x)
  
  folds <- cut( seq(1,N), breaks= k, labels=FALSE)
  
  training_part <- data[folds!=i,] # for building the model (training)
  validation_part <- data[folds==i,] # for prediction (validation)
  
  
  x_train <- training_part[,1]
  y_train <- training_part[,2]
  
  x_validation <- validation_part[,1]
  y_validation <- validation_part[,2]
  
  m <- loess(y ~ x, training_part, span=h, control=loess.control(surface="direct"))
  
  y_pred <- predict(m, x_validation)
  
  nv <- length(x_validation)
  
  PMSE <- 0
  
  for(j in 1:nv)
  {
    PMSE <- PMSE + (y_validation[j] - y_pred[j])^2
  }
  PMSE/nv
}

PMSE.kCV <- function (x,y, h.v, k)
{
  data <- data.frame(x,y)
  N <- length(data$x)
  
  s.h <- rep(0, length(h.v))
  
  for(j in 1:length(h.v))
  {
    h.error <- c()
    for(i in 1:k)
    {
      h.error[i] <- CV.i.fold(i,h.v[j],k=k, data)
    }
    s.h[j] <- mean(h.error)
  }
  s.h
}

#Leave one out cross validation
PMSE.CV <- function(x,y, h.v)
{
  s.h <- rep(0, length(h.v))
  
  for (i in 1:length(h.v))
  {
    h <- h.v[i]
    m <- locpolreg(x=x, y=y, h=h, doing.plot = F)
    
    S <- m$S
    
    s.h[i] <- sum( ((y - m$mtgr)/(1 - diag(S)))^2 ) / length(y)
  }
  
  s.h
}

PMSE.GCV <- function(x,y, h.v)
{
  s.h <- rep(0, length(h.v))
  
  for (i in 1:length(h.v))
  {
    h <- h.v[i]
    m <- locpolreg(x=x, y=y, h=h, doing.plot = F)
    
    S <- m$S
    
    traceS <- sum(diag(S))
    
    n <- length(y)
    
    avg.sii <- rep(traceS/n, n)
    
    s.h[i] <- sum( ((y - m$mtgr)/(1 - avg.sii))^2 ) / n
  }
  
  s.h
}

find.best.h <- function(h.v, h.results)
{
  return(h.v[which(h.results==min(h.results))])
}
