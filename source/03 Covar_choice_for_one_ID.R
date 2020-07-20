## 2. Model construction : choice of covariables. 
# this step allow to choose which covar need to be included in the model. For that, we need to adjust lot of model with different covar. 
# The best will be done that with a loop of something and stock AIC of each model and the model with the lower AIC. 

# Working with one mosquito 

# load packages
library(momentuHMM)

# load data
#data <- readRDS("./output/data_momentuHMM.rds")
ID <- readRDS("./output/ID23/ID23.rds")
ID$Hour12 <- rep(1:12, length.out=length(ID[,1]))
ID$Hour8 <- rep(1:8, length.out=length(ID[,1]))

# Model null (more simple)

nbStates <- 4
Par0_3s <-c(0.0001, 4, 15)
Par0_4s <-c(0.0001, 5, 15, 35)# Change to adapt to individual data
Par0_5s <-c(0.0001, 5, 15, 30, 40)
del0 <- rep(1.e-100,4)##initial parameter for Markov Chain
Par0=list(AC=Par0_4s)
prior <- function(par){sum(dnorm(par,0,10,log=TRUE))}

mod.null <- fitHMM(data = ID, nbStates = nbStates, dist= list(AC="pois"), 
                   Par0 = Par0, delta0 =del0, prior=prior)

# double check 

Par_02 <-getPar0(model=mod.null)
mod.null2 <- fitHMM(data = ID, nbStates = nbStates, dist= list(AC="pois"), 
                   Par0 = Par_02$Par, delta0 =Par_02$delta, beta0=Par_02$beta, prior=prior)

saveRDS(mod.null2, paste0("./output/modnull2_",unique(ID$ID),".rds"))

# Model with lambda~Day

DM <- list(AC=list(lambda=~Day))
Par_0B <-getPar0(model=mod.null2, DM=DM)

mod_lambda<- fitHMM(data =ID, nbStates = nbStates, 
                 dist= list(AC="pois"), Par0 =Par_0B$Par,  
                 beta0=Par_0B$beta, DM=DM, delta0 =Par_0B$delta, prior=prior) 

saveRDS(mod_lambda, paste0("./output/modlambda_",unique(ID$ID),".rds"))

# Model cosinor 24h
formula <- ~ cosinor(Hour, period = 24)
Par_0B <-getPar0(model=mod.null2, formula=formula)

mod_cos<- fitHMM(data =ID, nbStates =nbStates, 
                 dist= list(AC="pois"), Par0 =Par_0B$Par,  
                 beta0=Par_0B$beta, formula=formula, delta0 =Par_0B$delta, prior=prior) 

saveRDS(mod_cos, paste0("./output/modcos24_",unique(ID$ID),".rds"))

# Model cosinor 12h
formula2 <- ~ cosinor(Hour12, period = 12)
Par_0B <-getPar0(model=mod.null2, formula=formula2)

mod_cos12<- fitHMM(data =ID, nbStates =nbStates, 
                 dist= list(AC="pois"), Par0 =Par_0B$Par,  
                 beta0=Par_0B$beta,formula=formula2, delta0 =Par_0B$delta, prior=prior) 

mod_cos12

saveRDS(mod_cos12, paste0("./output/modcos12_",unique(ID$ID),".rds"))

# Model cos 8h
formula3 <- ~ cosinor(Hour8, period = 8)
Par_0B <-getPar0(model=mod.null2, formula=formula3)

mod_cos8<- fitHMM(data =ID, nbStates =nbStates, 
                   dist= list(AC="pois"), Par0 =Par_0B$Par,  
                   beta0=Par_0B$beta,formula=formula3, delta0 =Par_0B$delta, prior=prior) 

saveRDS(mod_cos8, paste0("./output/modcos8_",unique(ID$ID),".rds"))

# Model cos+lambda
DM <- list(AC=list(lambda=~Day))
formula <- ~ cosinor(Hour, period = 24)

Par_0B <-getPar0(model=mod_cos, formula=formula, DM=DM)

mod_DC<- fitHMM(data =ID, nbStates = nbStates, 
               dist= list(AC="pois"), Par0 =Par_0B$Par,  
               beta0=Par_0B$beta, formula=formula, DM=DM, delta0 =Par_0B$delta, prior=prior) 

saveRDS(mod_DC, paste0("./output/modDC24",unique(ID$ID),".rds"))

# Model cos+lambda12
DM <- list(AC=list(lambda=~Day))
formula <- ~ cosinor(Hour12, period = 12)

Par_0B <-getPar0(model=mod_cos, formula=formula, DM=DM)

mod_DC12<- fitHMM(data =ID, nbStates = nbStates, 
                dist= list(AC="pois"), Par0 =Par_0B$Par,  
                beta0=Par_0B$beta, formula=formula, DM=DM, delta0 =Par_0B$delta, prior=prior)

saveRDS(mod_DC12, paste0("./output/modDC12",unique(ID$ID),".rds"))


# Model cos+lambda8

DM <- list(AC=list(lambda=~Day))
formula <- ~ cosinor(Hour8, period = 8)

Par_0B <-getPar0(model=mod_cos, formula=formula, DM=DM)

mod_DC8<- fitHMM(data =ID, nbStates = nbStates, 
                dist= list(AC="pois"), Par0 =Par_0B$Par,  
                beta0=Par_0B$beta, formula=formula, DM=DM, delta0 =Par_0B$delta, prior=prior) 

saveRDS(mod_DC8, paste0("./output/modDC8",unique(ID$ID),".rds"))

# mod lambda~day + gamma~cosinor+day

DM <- list(AC=list(lambda=~Day))
formula <- ~ cosinor(Hour, period = 24) + Day

Par_0B <-getPar0(model=mod_cos, formula=formula, DM=DM)

mod_2DC<- fitHMM(data =ID, nbStates = nbStates, 
                dist= list(AC="pois"), Par0 =Par_0B$Par,  
                beta0=Par_0B$beta, formula=formula, DM=DM, delta0 =Par_0B$delta, prior=prior) 

mod_2DC

saveRDS(mod_2DC, paste0("./output/mod_2_DC24",unique(ID$ID),".rds"))

# mod lambda~1 + gamma~cosinor+day

DM <- list(AC=list(lambda=~1))
formula <- ~ cosinor(Hour, period = 24) + Day

Par_0B <-getPar0(model=mod_cos, formula=formula, DM=DM)

mod_2DCb<- fitHMM(data =ID, nbStates = nbStates, 
                 dist= list(AC="pois"), Par0 =Par_0B$Par,  
                 beta0=Par_0B$beta, formula=formula, DM=DM, delta0 =Par_0B$delta, prior=prior) 

mod_2DCb
saveRDS(mod_2DC, paste0("./output/mod_2_DC24",unique(ID$ID),".rds"))



# AIC 
AIC1 <- AIC(mod.null2, mod_lambda, mod_cos, mod_cos12, mod_cos8, mod_DC, mod_DC12, mod_DC8,
            mod_2DC, mod_2DCb)
saveRDS(AIC1, paste0("./output/AICmod_",unique(ID$ID),".rds"))

n <- AIC1$Model[which.min(AIC1$AIC)]
n# nom du modèle qu'on reprendra pour la suite 
AIC(n)
# We take the model with the lowest AIC to adjust different harmonics. 

# harmonics

formula4 <- ~ cosinor(Hour, period = 24) + state2(cosinor(Hour12, period=12))+state2(cosinor(Hour8, period=8))+state3(cosinor(Hour12, period=12))+state3(cosinor(Hour8, period=8))#
formula5 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour12, period=12))+state2(cosinor(Hour8, period=8))+state3(cosinor(Hour12, period=12))#
formula6 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour12, period=12))+state2(cosinor(Hour8, period=8))+state3(cosinor(Hour8, period=8))#
formula7 <- ~ cosinor(Hour, period = 24)+ state3(cosinor(Hour12, period=12))+state3(cosinor(Hour8, period=8))+state2(cosinor(Hour12, period=12))#
formula8 <- ~ cosinor(Hour, period = 24)+ state3(cosinor(Hour12, period=12))+state3(cosinor(Hour8, period=8))+state2(cosinor(Hour8, period=8))#

formula9 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour12, period=12))+state2(cosinor(Hour8, period=8))#
formula10 <- ~ cosinor(Hour, period = 24)+ state3(cosinor(Hour12, period=12))+state3(cosinor(Hour8, period=8))#

formula11 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour12, period=12))+state3(cosinor(Hour12, period=12))#
formula12 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour12, period=12))+state3(cosinor(Hour8, period=8))#
formula13 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour8, period=8))+state3(cosinor(Hour12, period=12))#
formula14 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour8, period=8))+state3(cosinor(Hour8, period=8))#

formula15 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour12, period=12))#
formula16 <- ~ cosinor(Hour, period = 24)+ state2(cosinor(Hour8, period=8))#
formula17 <- ~ cosinor(Hour, period = 24)+ state3(cosinor(Hour12, period=12))#
formula18 <- ~ cosinor(Hour, period = 24)+ state3(cosinor(Hour8, period=8))#
formula19 <-  ~ state2(cosinor(Hour, period=24))+state3(cosinor(Hour8, period=24))+state4(cosinor(Hour, period=24))

## model with each formula. Add option for save each model (Trymod in different list of result than AIC)

# list with all formulas
# With the model choosen in the first part, test the different harmonics contained in formula_list.

formula_list <- list(formula4, formula5, formula6, formula7, formula8, 
                  formula9, formula10, formula11, formula12, formula13, 
                  formula14, formula15, formula16, formula17, formula18, formula19)


DM <- list(AC=list(lambda=~Day))# a changer selon si c'est le modèle cos ou DC qui a donné le plus petit AIC dans les étapes précedentes. 

mod_harmonic_list <- list()
AIC2 <- matrix(0, length(formula_list),2)
foreach (i = 1:length(formula_list)) %dorng%
{
  Par_0B <-getPar0(model=mod_DC, formula=formula_list[[i]], DM=DM)
  trymod<-tryCatch(fitHMM(data = ID, 
                           nbStates = 3,
                           dist= list(AC="pois"), 
                           Par0 = Par_0B$Par,  
                           beta0=Par_0B$beta, DM=DM, formula=formula_list[[i]], delta0 =Par_0B$delta, prior=prior),
                    error=function(e) e)
  if(inherits(trymod,"error")){
    mod_harmonic_list[[i]] <- "error"
    AIC2[i,] <- "error"
  } else 
    mod_harmonic_list[[i]] <- trymod
    AIC2[i,1] <- AIC(trymod)
  AIC2[i,2] <- i
}
stopImplicitCluster()


saveRDS(mod_harmonic_list, paste0("./output/mod_harmonic_list_ID",unique(ID$ID),".rds"))
saveRDS(AIC2, paste0("./output/AIC2_ID",unique(ID$ID),".rds"))
which.min(AIC2[,1])


AIC4 <- data.frame(cbind(AIC2[,2], AIC2[,1]))
colnames(AIC4) <- c("Model", "AIC")
AICtot <- rbind(AIC4, AIC1)
which.min(AICtot[,2])
AICtot
saveRDS(AICtot, paste0("./output/AICtot_ID",unique(ID$ID),".rds"))


# Clean .GlobalEnv
## idées pour les parametres initiaux : choisir le max de la distri et decrire les autres paramètres à partir de ça. 

