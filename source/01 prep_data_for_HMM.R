## Preparation of the data for the HMM 

# load packages 

library(momentuHMM)
library(forcats)
library(tidyverse)
library(xts)
library(lubridate)

# load data as dataframe (created by script )
# DF
activityDF <- readRDS("./Data/ActivityDF.rds")

# source("./source/00 Input and Clean Data.R")Script source pour la création des data. Pas besoin de le charger à chaque fois. 

# Préparation des data pour HMM (compatible momentuHMM)

## Deleting control 

activityDF <- subset(activityDF, Status!="Control")

## Add Hour and Day 

activityDF$Hour <-hour(activityDF$Time) 
activityDF$Day <-day(activityDF$Time) 

## Add AC (correspond to Activity but in HMM I used AC as notation)

activityDF$AC <- activityDF$Activity

## add ID colums for individual notation

activityDF$ID <- as.factor(paste0(activityDF$Monitor, activityDF$Channel))

# creation of a list of dataframe with metadata, take care of the object class, the following part works with "tbl_df" format

# creation of a momentuHMM object (big dataframe) for HMM
data <- prepData(activityDF, coordNames = NULL)
data <- data[(data['Time'] > '2018-10-28 05:59:00'),]#substracting the first day. 
data$Day <- factor(data$Day)
data$Day <- fct_relevel(data$Day, "28", "29", "30", "31", "1", "2", "3")#sort days in real time scale (end of a month and a begining of the other)

saveRDS(object = data, file = "./Data/data_momentuHMM.rds")

# creation of a list could be useful for some action later

data.ind <- split(data, data$ID)

saveRDS(object = data.ind, file = "./Data/data.ind.rds")

