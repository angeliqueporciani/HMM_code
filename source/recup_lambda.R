# load package
library(momentuHMM)
library(tidyverse)
library(plyr)
library(Rmisc)
library(here)

## maybe these script could be more useful for plotting stationnary probs coming from all individuals models. 

# load data (best model for each ID)
# functions below allow to load all models in one steps and stock them in a list. 

files_names <- list.files(here("./output/HMM/bestmodels"))
nb_files <- length(files_names)
data_names <- vector("list",length=nb_files)

for (i in 1 : nb_files) {
  data_names[i] <- strsplit(files_names[i], split=".rds")
}

for (i in 1:nb_files) {
  assign(data_names[[i]], 
         readRDS(paste(here("./output/HMM/bestmodels", files_names[i]))))
}

#on stock tous les data dans une liste
dataset_list <- vector("list",length=nb_files)

for (i in 1:nb_files) {
  dataset_list[[i]] <- get(data_names[[i]])
}

# recup lambda and ICs for each models retained from previous steps
lambda <- list()
ICL <- list()
ICU <- list()

for (i in 1:length(dataset_list)){
  lambda[[i]] <- cbind(dataset_list[[i]]$CIreal$AC$est)
  ICL[[i]] <- cbind(dataset_list[[i]]$CIreal$AC$lower)
  ICU[[i]] <- cbind(dataset_list[[i]]$CIreal$AC$upper, unique(dataset_list[[i]]$data$ID))
}

lambda_df <- do.call(rbind.data.frame, lambda)

for (i in 1:length(lambda)){
  lambda[i,] <- sort(lambda_df[i,])
}
# comment faire pour que les IC soit dans le même ordre que les lambda également ??
ICL_df <-  do.call(rbind.data.frame, ICL)
ICU_df <-  do.call(rbind.data.frame, ICU)

#creation d'une table regroupant tous les lambda de tous les ID avec ICL et ICU quand j'aurai trouvé comment tout garder dans le bon ordre. 
tab_lambda <- cbind(lambda_df, ICL_df, UCL_df)# ca vas pas tant que tous le monde n'est pas dans le bon ordre. 
kable(tab_lambda)


