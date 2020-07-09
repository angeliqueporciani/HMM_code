## 1. Model construction : State number choice for one mosquito
# For the first part of the script we work with one individual
# Working with the number 1 

# load packages

library(xts)
library(momentuHMM)
library(doRNG)
library(doParallel)
library(setRNG)

# load data

data <- readRDS("./Projet Insemination/output/data_momentuHMM.rds")
data$Day <- droplevels(data$Day)

# Choose ID to work with. 

# 1st method : if we already know with which individual we want to work.
ID <- data[data$ID=="113",] 

# 2nd method : random ID choice following his caracteristics 

ID_sample <- function(data, food, status,Lstatus){
x <- sample(unique(data$ID[data$Food==food & data$Status==status & data$Light_status==Lstatus]),1)
ID <- data[data$ID==x,]
return(ID)
}

ID <- ID_sample(data, "Blood", "Inseminated", "LD")
plot(ID$AC, type="h", ask=FALSE, main = paste0("ID",unique(ID$ID)))#please check the brut data before the following parts. 
#If the individual selected has a lower activity, or seems to be dead before the end of recording, change. 

# save the ID data for following steps 
saveRDS(ID, paste0("./output/ID", unique(ID$ID),".rds"))

# Model construction from 2 to 5 states

### initials parameters: these need to be choosen carrefully for the stability of the model. 
# We start with minimal value and almost max of count data. 

summary(ID$AC)# allows choose the initial parameters
## Try several values, the model is very sensitive to these initial values. 
Par0_2s <-c(0.0001, 15)
Par0_3s <-c(0.0001, 4, 15)
Par0_4s <-c(0.0001, 4, 15, 20)
Par0_5s <-c(0.0001, 5, 15, 20, 25)
Par_list <- list(NULL, Par0_2s, Par0_3s, Par0_4s, Par0_5s)

#foreach (i = Par_list) %dorng%
#for (i in Par_list)
AIC <- matrix(0, 5,2)
AICmodlist <- list()
foreach (i = 2:5) %dorng%
{
  trymodA<-tryCatch(fitHMM(data = ID, 
                           nbStates = i,
                           dist= list(AC="pois"), 
                           delta0= rep(1.e-100,i),
                           Par0 = list(AC=Par_list[[i]]), retryFits = 5), # retryFits to discuss, maybe useful if we work for different individuals with different initial parameters. 
                    error=function(e) e)
  if(inherits(trymodA,"error")){
    AIC[i,] <- "error"
    AICmodlist[[i]] <- "error"
    } else 
    AIC[i,1] <- AIC(trymodA)
    AIC[i,2] <- i
    AICmodlist[[i]] <- trymodA
}
stopImplicitCluster()

saveRDS(AIC,paste0("./output/AICID",unique(ID$ID),".rds"))
saveRDS(AICmodlist,paste0("./output/AICmodID",unique(ID$ID),".rds"))

# which model gets the lower AIC = best model. This model will be used in the next script for covar choice. 
which.min(AIC[,1][AIC[,1]>0])#give the place of the lowest AIC among AIC>0. 

AIC
AICmodlist[[3]]
AICmodlist[[4]]
AICmodlist[[5]]

# sometimes, HMM with five states has a lower AIC, but 5 states had no real biological interpretation as the difference between states is a lamba of 0.003 and 0.77 for instance. 


## This script could be, and should be, done for multiple individuals of the same physiological and food status. 
# Indeed, the number of state detected could be linked to the physiological states or food. 

###############################################################################################
## 2. Automatisation of number of state choice for individuals of different conditions. 
###############################################################################################
# 2.1 load data
data.ind<- readRDS("./output/data.ind.rds")
 
# 2.2 Loop for all individuals of a subset (adapted from the previous loop): stock of AIC for each individuals in a list ? 
Par0_2s <-c(0.0001, 35)
Par0_3s <-c(0.0001, 35, 50)
Par0_4s <-c(0.0001, 3, 50, 70)
Par0_5s <-c(0.0001, 3, 35, 50, 70)

Par_list <- list(NULL, Par0_2s, Par0_3s, Par0_4s, Par0_5s)
delta0= rep(1.e-100,4)
AIC_list <- vector("list", length(data.ind))

for (j in 1:length(data.ind))
  {      
  AIC <- matrix(0, 5,2)
    foreach (i = 2:5) %dorng%
      {
         trymodA<-tryCatch(fitHMM(data = data.ind[[j]], 
                           nbStates = i,
                           dist= list(AC="pois"), 
                           delta0= rep(1.e-100,i),
                           Par0 = list(AC=Par_list[[i]]), retryFits = 3), # retryFits to discuss, maybe useful if we work for different individuals with different initial parameters. 
                    error=function(e) e)
        if(inherits(trymodA,"error")){
       AIC[i,] <- "error"
          } else 
            AIC[i,1] <- AIC(trymodA)
            AIC[i,2] <- i
      }
  AIC_list[[j]] <- AIC
  }

stopImplicitCluster()

# 2.3 Save AIC for each ind 
saveRDS(AIC_list, "AIC_allID.rds")

## We need to attach metadata to AIC_list element in order to discriminate between each group. 
## We also need to find the number of state that give the lower AIC for each individuals. Maybe with sapply or lapply or tapply... 

# clean Global.Env
rm(trymodA)
rm(Par_list)
rm(ID1)
rm(data)




