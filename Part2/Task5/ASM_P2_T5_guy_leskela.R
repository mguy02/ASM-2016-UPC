
set.seed(12)
library("sm")

hirsut<-read.table(file="hirsutism.dat",head=T, sep="\t")

hirsut<- na.omit(hirsut)

head(hirsut)

attach(hirsut)

hirsut.0 <- hirsut[hirsut$Treatment == 0,]
hirsut.1 <- hirsut[hirsut$Treatment == 1,]
hirsut.2 <- hirsut[hirsut$Treatment == 2,]
hirsut.3 <- hirsut[hirsut$Treatment == 3,]

# 1. In the following regression models test the no eect null hypothesis
# (model="no effect" in sm.regression):

  # weight as a function of height
  lm1.weight.height <- sm.regression(height, weight, model = "no effect")

  # SysPres as a function of height
  lm1.syspres.height <- sm.regression(height, SysPres, model = "no effect")
  
  # SysPres as a function of a weight
  lm1.syspres.weight <- sm.regression(weight, SysPres, model = "no effect")
  
  # FGm0 as a function of weight
  lm1.FGm0.weight <- sm.regression(weight, FGm0, model = "no effect")
  
  # SysPres as a function of DiaPres
  lm1.syspres.diapres <- sm.regression(DiaPres, SysPres, model = "no effect")
  

hirsut.0and2 <- rbind(hirsut.0, hirsut.2)

#Create a binary variable (Tr02) with value 0 for patients with
# treatment 0, and 1 for patients with treatment 2.

Tr02 <- hirsut.0and2$Treatment / 2

 
# Test the null hypothesis stating that the logistic model is appropriate
# in the regression:
#   Tr02 as a function of FGm12.

lm2.tr02.fgm12 <- sm.regression(hirsut.0and2$FGm12, Tr02, model = "no effect")

#So it is not significant


#Repeat the last point using now treatments 2 and 3.

hirsut.2and3 <- rbind(hirsut.2, hirsut.3)

#Create a binary variable (Tr02) with value 0 for patients with
# treatment 0, and 1 for patients with treatment 2.

Tr23 <- hirsut.2and3$Treatment-2

# Test the null hypothesis stating that the logistic model is appropriate
# in the regression:
#   Tr23 as a function of FGm12.

lm3.tr23.fgm12 <- sm.regression(hirsut.2and3$FGm12, Tr23, model = "no effect")

#So it is more significant


# For this point use only patients with treatments 0 or 2. In the following
# regressions test whether the regression functions are equal in both
# groups.

# weight as a function of height
lm4.weight.height.0 <- sm.regression(hirsut.0$height, hirsut.0$weight, model = "no effect")
lm4.weight.height.2 <- sm.regression(hirsut.2$height, hirsut.2$weight, model = "no effect")

# SysPres as a function of height
lm4.syspres.height.0 <- sm.regression(hirsut.0$SysPres, hirsut.0$weight, model = "no effect")
lm4.syspres.height.2 <- sm.regression(hirsut.2$SysPres, hirsut.2$weight, model = "no effect")


##Repeat the last point using now treatments 2 and 3.

# weight as a function of height
lm5.weight.height.2 <- lm4.weight.height.2
lm5.weight.height.3 <- sm.regression(hirsut.3$height, hirsut.3$weight, model = "no effect")

# SysPres as a function of height
lm5.syspres.height.2 <- lm4.syspres.height.2
lm5.syspres.height.3 <- sm.regression(hirsut.3$SysPres, hirsut.3$weight, model = "no effect")


# In the following regressions test whether the regression functions are
# equal in the 4 groups defined by variable Treatment.
# weight as a function of height
# SysPres as a function of height

#Done above.