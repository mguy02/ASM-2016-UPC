
set.seed(12)
library("sm")

hirsut<-read.table(file="hirsutism.dat",head=T, sep="\t")

head(hirsut)

attach(hirsut)

hirsut.0 <- hirsut[Treatment == 0,]
hirsut.1 <- hirsut[Treatment == 1,]
hirsut.2 <- hirsut[Treatment == 2,]
hirsut.3 <- hirsut[Treatment == 3,]


#7. Comparing the regression function
# FGm12 as a function of FGm0
# in the 4 groups defined by Treatment.
# In sm.ancova, use both h1 and h2 obtained by cross-validation and
# AICc criteria, respectively (use argument method="cv" or method="aicc"
#   in h.select).
# In function sig.trace use hvec = seq(min(h1,h2)/3,3*max(h1,h2),length=20).


h1 <- h.select(x = FGm0, y = FGm12, method = "cv")
h2 <- h.select(x = FGm0, y = FGm12, method = "aicc")
hvec = seq(min(h1,h2)/3,3*max(h1,h2), length=20)

# using the h1 bandwidth
lm7.fgm12.fgm0.h1 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h1, method = "cv", group = Treatment, model = "equal", xlab="FGm0", ylab="FGm12")
# using the h2 bandwidth
lm7.fgm12.fgm0.h2 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h2, method = "cv", group = Treatment, model = "equal", xlab="FGm0", ylab="FGm12")

# 8. Test whether the regression function
# FGm12 as a function of FGm0
# can be considered equal or parallel in the two subpopulations defined
# according to Treatment==0 or not.
# Use the indications given in the last point for choosing the bandwidth.

group0 <- hirsut$Treatment == 0
# name is as follows:
# assignment.dependent.independent.group.model.bandwidth
lm8.fgm12.fgm0.0.equal.h1 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h1, method = "cv", group = group0, model = "equal", xlab="FGm0", ylab="FGm12")
lm8.fgm12.fgm0.0.parallel.h1 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h1, method = "cv", group = group0, model = "parallel", xlab="FGm0", ylab="FGm12")
lm8.fgm12.fgm0.0.equal.h2 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h2, method = "cv", group = group0, model = "equal", xlab="FGm0", ylab="FGm12")
lm8.fgm12.fgm0.0.parallel.h2 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h2, method = "cv", group = group0, model = "parallel", xlab="FGm0", ylab="FGm12")

group123 <- hirsut$Treatment!= 0
lm8.fgm12.fgm0.123.equal.h1 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h1, method = "cv", group = group123, model = "equal", xlab="FGm0", ylab="FGm12")
lm8.fgm12.fgm0.123.equal.h1 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h1, method = "cv", group = group123, model = "parallel", xlab="FGm0", ylab="FGm12")
lm8.fgm12.fgm0.123.equal.h2 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h2, method = "cv", group = group123, model = "equal", xlab="FGm0", ylab="FGm12")
lm8.fgm12.fgm0.123.equal.h2 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h2, method = "cv", group = group123, model = "parallel", xlab="FGm0", ylab="FGm12")

# 9. For this point use only patients with treatments 1 or 3. Test whether
# the regression function
# FGm12 as a function of FGm0
# can be considered equal or parallel in the two subpopulations defined
# by Treatment.
# Use the indications given above for choosing the bandwidth.

group13 <- hirsut$Treatment == 1 | hirsut$Treatment == 3
lm9.fgm12.fgm0.equal.h1 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h1, method = "cv", group = group13, model = "equal", xlab="FGm0", ylab="FGm12")
lm9.fgm12.fgm0.parallel.h1 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h1, method = "cv", group = group13, model = "parallel", xlab="FGm0", ylab="FGm12")

lm9.fgm12.fgm0.equal.h2 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h2, method = "cv", group = group123, model = "equal", xlab="FGm0", ylab="FGm12")
lm9.fgm12.fgm0.parallel.h2 <- sm.ancova(hirsut$FGm0, hirsut$FGm12, h = h2, method = "cv", group = group123, model = "parallel", xlab="FGm0", ylab="FGm12")

# 10. For this point use only patients with treatments 1, 2 or 3. Test the
# linearity of the regression function
# FGm12 as a function of FGm0.
# Use the indications given above for choosing the bandwidth.

hirsut.23 <- rbind(hirsut.2, hirsut.3)
hirsut.123 <- rbind(hirsut.1, hirsut.23)

h1.123 <- h.select(x = hirsut.123$FGm0, y = hirsut.123$FGm12, method = "cv")
h2.123 <- h.select(x = hirsut.123$FGm0, y = hirsut.123$FGm12, method = "aicc")

lm10.fgm12.fgm0.linear.h1.123 <- sm.regression(hirsut.123$FGm0, hirsut.123$FGm12, h=h1.123, display="se",model="linear")
lm10.fgm12.fgm0.linear.h1.123 <- sm.regression(hirsut.123$FGm0, hirsut.123$FGm12, h=h2.123, display="se",model="linear")
