
##################################
##         exercice 3           ##
##################################

samplesize <- 10
mean <- 30
standardDeviation <- sqrt(1.5)

#we first create our sample set
sampleset <- matrix(0, 1000, samplesize)


S1_value <- matrix(0, 1000,1)
S2_value <- matrix(0, 1000, 1)

for(i in 1:1000)
{
  sampleset[i,] <-  rnorm(samplesize, mean, standardDeviation)
  
  sum <- 0
  sample_mean = mean(sampleset[i,])
  for (j in 1:samplesize)
  {
    sum <- sum + (sampleset[i,j] - sample_mean)**2
  }
  
  
  S1_value[i,] <- sum/(samplesize-1)
  S2_value[i,] <- sum/(samplesize)
}

#printing
mean(S1_value)
hist(S1_value)

mean(S2_value)
hist(S2_value)


