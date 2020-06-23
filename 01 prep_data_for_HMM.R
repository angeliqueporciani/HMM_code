## Preparation of the data for the HMM 

# load packages 

library(momentuHMM)
library(forcats)
library(tidyverse)
# load consolidated list 
source("./src/00 Input and Clean Data.R")
##Elimination des control
actif <- Filter(function(x) attr(x, which = "Status", exact = TRUE) !="Control", activity)
length(actif)##control bien eliminé

# creation of a list of dataframe with metadata, take care of the object class, the following part works with "tbl_df" format
activity2 <- list()
for (i in 1:length(actif))
{
  activity2[[i]] <- data.frame(AC=as.integer(actif[[i]][,1]), Food=rep(xtsAttributes(actif[[i]])$Food), 
                                Status=rep(xtsAttributes(actif[[i]])$Status),
                                Monitor=rep(xtsAttributes(actif[[i]])$Monitor),
                                Time=index(actif[[i]]), 
                                Channel=rep(xtsAttributes(actif[[i]])$Channel),
                                ID= rep(i),
                                Winglength=rep(xtsAttributes(actif[[i]])$Wing_length),
                                LightStatus=rep(xtsAttributes(actif[[i]])$Light_status),
                                Hour=as.integer(strftime(index(actif[[i]]), format = "%H", tz="")),
                                Day=as.character(strftime(index(actif[[i]]), format = "%d", tz="")))
}


#creation of a dataframe
activityDF <- data.frame()
for (i in 1:length(activity2))
{
  activityDF <- rbind(activityDF,activity2[[i]])
}

# creation of a momentuHMM object (big dataframe) for HMM
data <- prepData(activityDF, coordNames = NULL)
data <- data[(data['Time'] > '2018-10-28 05:59:00'),]#substracting the first day. 
data$Day <- factor(data$Day)
data$Day <- fct_relevel(data$Day, "28", "29", "30", "31", "01", "02", "03")#sort days in real time scale (end of a month and a begining of the other)
saveRDS(object = data, file = "./output/data_momentuHMM.rds")

# creation of a list 
data.ind <- vector("list", 200)
for (i in 1:200){
  data.ind[[i]] <- data[data$ID==i,]
}
saveRDS(object = data.ind, file = "./output/data.ind.rds")

# Clean .GlobalEnv
rm(actif)
rm(activity2)# reload data.ind instead of activity2 name. 
rm(activityDF)
