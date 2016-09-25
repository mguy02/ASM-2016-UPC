
##################################
##         exercice 2           ##
##################################

samplesize <- 15


#we first create our sample set
sampleset <- matrix(0, 1000, samplesize)
#Now for each sample (rows) we compute the sample mean 
means_value = matrix(0, 1000,1)

for(i in 1:1000)
{
  sampleset[i,] <-  rpois(samplesize, 6)
  means_value[i] <- mean(sampleset[i,])
}

#printing
hist(means_value)


samplesize <- 50

sampleset <- matrix(0, 1000, samplesize)

for(i in 1:1000)
{
  sampleset[i,] <-  rpois(samplesize, 6)
  means_value[i] <- mean(sampleset[i,])
}


#printing
hist(means_value)

#Again, we can see the mean is 6, which is lambda

