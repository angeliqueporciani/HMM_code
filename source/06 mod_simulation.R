##Model simulation (other steps for model validation) ----

# load packages
library(momentuHMM)

# load the best model selected in step 03. 

mod <- readRDS("./output/modDC2423.rds")

# simulation tools from momenthumm 

# simulation of data from the mod selected in step3
simdata1 <- simData(model=mod_DC, obsPerAnimal =9141)
plot(mod_DC$data$Time,simdata1$AC, type="h", ylim=c(0,75))

# to see the difference between model null and with cyclical activity. 
simdata2 <- simData(mod=mod.null2, obsPerAnimal = 9141)

plot(mod$data$Time,simdata2$AC, type="h", ylim=c(0,75))

# we can compare with graph the simulated data to the real ones. 
par0sim <- getPar0(mod)
formula <- ~ cosinor(Hour, period = 24)
DM <- list(AC=list(lambda=~Day))

modsim1 <- fitHMM(data = simdata1, nbStates = 4, dist= list(AC="pois"), Par0 = par0sim$Par, 
                  delta0 =par0sim$delta, formula=formula, DM=DM, retryFits = 5)
prsim1 <- pseudoRes(modsim1)

qqnorm(prsim1$ACRes, main="Poisson HMM: Q-Q Plot of Pseudo Residuals (simulated1)")
abline(a=0, b=1, lty=3)
abline(h=seq(-2, 2, 1), lty=3)
abline(v=seq(-2, 2, 1), lty=3)

## 10 simulations of data coming from the best model selected. 
## This allow us to add a tool for validation model. Indeed, if the data simulated from the best model are close to the real one, we could assume that the model are able to describe our data. 
## Increasing the number of simulation give an overview of the variability in simulation data. 

listsim <- list()
for (i in 1:10){
  listsim[[i]] <- simData(model=mod, obsPerAnimal =9141)
}

par(mfrow=c(2,5))
for (i in 1:10){
  plot(mod$data$Time,listsim[[i]]$AC, type="h", ylim=c(0,60), ylab="simulated activity count", xlab="Time",main = paste0("sim data ID",unique(mod$data$ID)))
  par(new=TRUE)
  plot(mod$data$Time, mod$data$AC, type="h", col="red", ylim=c(0,60), ylab = "", xlab = "")
}
par(mfrow=c(1,1))

###################
listprsim <- list()
resmod <- list()
for (i in 1:10){
  resmod[[i]] <- fitHMM(data = listsim[[i]], nbStates = 4, dist= list(AC="pois"), 
                Par0 = par0sim$Par, delta0 =par0sim$delta, formula=formula, DM=DM)
  listprsim[[i]] <- pseudoRes(resmod[[i]])
}

#for (i in 1:10){
 # par(new=TRUE)
  #qqnorm(listprsim[[i]]$ACRes, main="Poisson HMM: Q-Q Plot of Pseudo Residuals (simulated)",
   #      ylim=c(-2, 4), pch="+")
#}

par(mfrow=c(2,5))
for (i in 1:10){
  plot(mod$data$Time,resmod[[i]]$data$AC, type="h", ylim=c(0,100), ylab="simulated activity count", xlab="Time",main = paste0("sim data ID",unique(mod$data$ID)))
  par(new=TRUE)
  plot(mod$data$Time, mod$data$AC, type="h", col="red", ylim=c(0,100), ylab = "", xlab = "")
}
par(mfrow=c(1,1))

plot(mod$data$Time, mod$data$AC, type="h", col="red", ylim=c(0,100))
summary(mod$data$AC)

#################################################################################
## Vérification des résidus sur simulations des données à partir d'un autre package (hiddenMarkov)
#   Example Using Poisson Distribution for pseudo residuals----- 
require(HiddenMarkov)
##utilisation des valeurs du modèle A (plus facile pour créer la matrice dans un premier temps comme il n'y pas de covariables à impliquer)
Pi <- matrix(c(0.83887657,0.09704158,0.007124469,0.05695737,
                  0.11623148, 0.40601937, 0.065624652, 0.41212450,
                  0.01888856, 0.07767211, 0.592146508, 0.31129282,
                 0.06224522, 0.30504855, 0.148799599, 0.48390663),
             byrow=TRUE, nrow=4)

x <- dthmm(NULL, Pi, c(0.7, 0.1,0.1,0.1), "pois",
           list(lambda=c(0.02858405, 4.91315, 15.10341, 33.47211)), discrete=TRUE)

n <- 2000
x <- simulate(x, nsim=n, seed=5)

y <- residuals(x) 

w <- hist(y, main="Poisson HMM: Pseudo Residuals")
z <- seq(-3, 3, 0.01)
points(z, dnorm(z)*n*(w$breaks[2]-w$breaks[1]), col="red", type="l")
box()

##comparaison des 3 résidus obtenus = avec les 2 packages et le "vrai" modèle A. 
par(mfrow=c(1,3))
qqnorm(y, main="Poisson HMM: Q-Q Plot of Pseudo Residuals 
       (simulated data)")
abline(a=0, b=1, lty=3)
abline(h=seq(-2, 2, 1), lty=3)
abline(v=seq(-2, 2, 1), lty=3)

qqnorm(pr1$ACRes, main="Poisson HMM: Q-Q Plot of Pseudo Residuals 
       (real model)")
abline(a=0, b=1, lty=3)
abline(h=seq(-2, 2, 1), lty=3)
abline(v=seq(-2, 2, 1), lty=3)

qqnorm(prsim1$ACRes, main="Poisson HMM: Q-Q Plot of Pseudo Residuals 
       (simulated data2)")
abline(a=0, b=1, lty=3)
abline(h=seq(-2, 2, 1), lty=3)
abline(v=seq(-2, 2, 1), lty=3)

