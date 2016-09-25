
##################################
##         exercice 4+5         ##
##################################

samplesize <- 50
sampleset <- matrix(0, 100, samplesize)
samplemeans <- matrix(0, 100, 1)
samplevariances <- matrix(0,100, 1)

mu <- 12
standardDeviation <- sqrt(3)

for(i in 1:100)
{
  sampleset[i,] <- rnorm(samplesize, mu, standardDeviation)  
  samplemeans[i,] <- mean(sampleset[i,])
  samplevariances[i,] <- var(sampleset[i,])
}

range <- 1.96*standardDeviation/sqrt(samplesize)
sample_mean <- mean(samplemeans)
sample_variance <- mean(samplevariances)


sum(samplemeans >= sample_mean-range & samplemeans <= sample_mean+range)

alpha <- 0.05

upper <- (samplesize-1)*sample_variance

chi.1ma <- qchisq(1-alpha/2, samplesize-1) #chisquare 1-alpha/2, n-1
chi.a <- qchisq(alpha/2, samplesize-1) #chisquare alpha/2, n-1

sum(upper/chi.1ma <= samplevariances  & samplevariances <= upper/chi.a)
