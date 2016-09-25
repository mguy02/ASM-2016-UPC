
##################################
##         exercice 1           ##
##################################

############
# first part
samplesize <- 15


#we first create our sample set
sampleset <- matrix(0, 1000, samplesize)
#Now for each sample (rows) we compute the sample mean 
means_value = matrix(0, 1000,1)

for(i in 1:1000)
{
  sampleset[i,] <-  rbinom(samplesize, 10, .35)
  means_value[i] <- mean(sampleset[i,])
}

#printing
hist(means_value)

############
#Second part
samplesize <- 50

sampleset <- matrix(0, 1000, samplesize)

for(i in 1:1000)
{
  sampleset[i,] <-  rbinom(samplesize, 10, .35)
  means_value[i] <- mean(sampleset[i,])
}


#printing
hist(means_value)

#Ctrl shift C

# We observe that in both histograms, the means value (which is a random variable) are shaped
# like a Normal/Gaussian distribution. It is centered around 3.5.
# The Central Limit Theorem says that if we have X, a RV, and take a sample x1, ..., xn from
# it, then the mean sample follows approximately a Normal distribution.

