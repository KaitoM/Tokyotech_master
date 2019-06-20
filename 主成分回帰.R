#install.packages("ISLR")
#install.packages("pls")

library(ISLR)
library(pls)

pcr.res.cv <- pcr(var_1 ~ ., data=data, validation="CV")

summary(pcr.res.cv)

plot(MSEP(pcr.res.cv), legendpos="topright")

selectNcomp(pcr.res.cv, method="onesigma", plot=T)

MSEP(pcr.res.cv, ncomp=5)
MSEP(pcr.res.cv, ncomp=1)


plsr.res.cv <- plsr(var_1 ~ ., data=data, validation="CV")

summary(plsr.res.cv)

plot(MSEP(plsr.res.cv), legendpos="topright")

selectNcomp(plsr.res.cv, method="onesigma", plot=T)

MSEP(plsr.res.cv, ncomp=5)
MSEP(plsr.res.cv, ncomp=1)
