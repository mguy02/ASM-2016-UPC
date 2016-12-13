# We will be working with the file countries.txt containing information on
# development indicators measured in 132 countries 
# (Source: World Bank,1992).
# 
# We will focus on the following variables:
#   life.exp        Life expectancy at birth.
#   agricul         % of agriculture contribution to the GDP (Gross Domestic Product).
#   inf.mort        Infant mortality rate: The annual number of deaths of infants
#                   under one year of age per 1,000 live births in the same year.
#   life.exp.f      Life expectancy at birth for females.
#   life.exp.m      Life expectancy at birth for males.
#   le.fm           Difference life.exp.f minus life.exp.m.
# 

library(sm)
library(KernSmooth)


# Read data from countries.txt:

countries<-read.table(file="countries.txt",head=T,row.names=2,dec=",")


head(countries)
summary(countries)

attach(countries)



###
## Section 4.3, point 5. When fitting the local Poisson models, use your script to select the bandwidth.

#Variable le.fm always takes non-negative values, except for one country.

le.fm.0 <- pmax(0,le.fm)

# plot its histogram:
hist(le.fm.0,br=40)

source("h.cv.sm.poisson.R")


###### Use sm.poisson to fit the following local Poisson regression models:
### le.fm.0 as a function of inf.mort

  # Local Poisson regression

h1.CV.prob <- h.cv.sm.poisson(inf.mort, le.fm.0, method=prob.missclas.CV)
(h1 <- h1.CV.prob$h.cv)

m1.poisson <- sm.poisson(inf.mort, le.fm.0, h = h1, col="black")


  # using a standard nonparametric regression fit

m1.std <- sm.regression(inf.mort, le.fm.0, add=T, col="red", lwd=2)


  # using a Poisson Generalized Linear Model

countries.ordered = countries[order(inf.mort),]
head(countries.ordered)

le.fm.0.ordered <- pmax(0,countries.ordered$le.fm)

m1.glm.poisson <- glm(le.fm.0.ordered ~ inf.mort, family= poisson, data=countries.ordered)

lines(m1.glm.poisson$fitted.values, col="blue", lwd=2)

#######
### le.fm.0 as a function of life.exp

  # Local Poisson regression

h2.CV.prob <- h.cv.sm.poisson(life.exp, le.fm.0, method=prob.missclas.CV)
h2 <- h2.CV.prob$h.cv
m2.poisson <- sm.poisson(life.exp, le.fm.0, h = h2, col="black", lwd=2)


  # using a standard nonparametric regression fit

m2.std <- sm.regression(life.exp, le.fm.0, add=T, col="red", lwd=2)


  # using a Poisson Generalized Linear Model

countries.ordered = countries[order(life.exp),]
head(countries.ordered)

le.fm.0.ordered <- pmax(0,countries.ordered$le.fm)

m2.glm.poisson <- glm(le.fm.0.ordered ~ life.exp, family= poisson, data=countries.ordered)

lines(m2.glm.poisson$fitted.values, col="blue", lwd=2)
