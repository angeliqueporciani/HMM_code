## 1. Model construction : State number choice for one mosquito
# For the first part of the script we work with one individual
# Working with the number 1 

# load packages

library(xts)
library(momentuHMM)
library(doRNG)
library(doParallel)
library(setRNG)

###############################################################################################
## Automatisation for number of state choice for all individuals. 
###############################################################################################

# 1 load data
data.ind<- readRDS("./Data/data.ind.rds")
 
# 2 Loop for all individuals. 

# AIC_nbchoice <- function(data.ind) { This function need a list of momentuHMM data (one for each individuals) created by script 01. 

AIC_list <- vector("list", length(data.ind))
AIC_DF <- vector("list", length(data.ind))

for (j in 1:length(data.ind)){
  
  ID <- data.ind[[j]]
# initial parameteres specific of each individual. 
  Par0_2s <-c(0.0001, quantile(ID$AC[ID$AC!=0], 0.75))
  Par0_3s <-c(0.0001, quantile(ID$AC[ID$AC!=0], 0.25), quantile(ID$AC[ID$AC!=0], 0.75))
  Par0_4s <-c(0.0001, quantile(ID$AC[ID$AC!=0], 0.25), quantile(ID$AC[ID$AC!=0], 0.75), quantile(ID$AC[ID$AC!=0], 0.75)+5)
  Par0_5s <-c(0.0001, quantile(ID$AC[ID$AC!=0], 0.25), quantile(ID$AC[ID$AC!=0], 0.75), quantile(ID$AC[ID$AC!=0], 0.75)+5, quantile(ID$AC[ID$AC!=0], 0.75)+10)
  Par_list <- list(NULL, Par0_2s, Par0_3s, Par0_4s, Par0_5s)
        
  AIC <- matrix(0, 5,2)
    foreach (i = 2:5) %dorng%
      {
         trymodA<-tryCatch(fitHMM(data = data.ind[[j]], 
                           nbStates = i,
                           dist= list(AC="pois"), 
                           delta0= rep(1.e-100,i),
                           Par0 = list(AC=Par_list[[i]]), retryFits = 3), # retryFits for test a little variation of initial parameter on model fit.  
                    error=function(e) e)
        if(inherits(trymodA,"error")){
       AIC[i,] <- "error"
          } else 
            AIC[i,1] <- AIC(trymodA)
            AIC[i,2] <- i
      }
  AIC_list[[j]] <- AIC
  attributes(AIC_list[[j]]) <- data.ind[[j]][1,c(1,5,6,7)]
  AIC_DF[[j]] <- cbind.data.frame(attributes(AIC_list[[j]]), AIC=AIC_list[[j]][1:5], States=AIC_list[[j]][6:10]) 
  #return(AIC_DF)
}
#}

stopImplicitCluster()
  
# regrouper tout les AIC de tous les ID dans un meme DF
AIC_DF_tot <- do.call(rbind, AIC_DF)

# travailler sur ce gros DF pour savoir si ya une relation entre nombre d'état et groupe. 

# 2.3 Save AIC for each ind 
saveRDS(AIC_list, "AIC_allID.rds")
saveRDS(AIC_DF_tot, "AIC_DF.rds")


## We need to attach metadata to AIC_list element in order to discriminate between each group. 
## We also need to find the number of state that give the lower AIC for each individuals. Maybe with sapply or lapply or tapply... 

# clean Global.Env
rm(trymodA)
rm(Par_list)





