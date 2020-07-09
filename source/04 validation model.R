## Step 4 : Modele validation 
# Load packages
library(momentuHMM)

# load data 
mod <- readRDS("./output/modDC2434.rds")
modnull <- readRDS("./output/modnull2_34.rds")
 
# For validation of the model we observe the distribution of pseudo residual and de ACF (we can also compare graphics coming from the null model).
# plot of pseudo-residuals 
plotPR(mod, lag.max = 10000)
plotPR(mod.null2, lag.max=10000)
# The main difference is for ACF. 

# If for some individuals you get a error message, you can try this. 
##
prmod <- pseudoRes(mod)
inf <- which(prmod$ACRes=="Inf")
prmod2 <- prmod$ACRes[-c(inf)]#this need to be tested, or add the numbers by hand. 

par(mfrow=c(1,3))
plot(prmod$ACRes, type="h", ylab= "pseudo-residu")# 1st graph
qqnorm(prmod$ACRes, ylim=c(-2, 8))# 2nd graph
abline(0, 1, lwd = 2, col="red")
acf(prmod2, lag.max = 10000, main="ACF", na.action = na.pass)#3rd graph
par(mfrow=c(1,1))

