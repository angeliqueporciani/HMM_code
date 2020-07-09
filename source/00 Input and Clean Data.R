# READ ME
# DO NOT DELETE OR EDIT THIS FILE!
# run this script at the beginning of all subsequent analyses to input data
# to this effect, use the 'source' function at the start of the script contatining your analysis

# Load packages

library(abcdee)
library(readxl)

# Input LAM data

path <- "./Data/Raw_data /"

R02M01 <- load.LAM(file = paste0(path, "R02M01.txt"), name = "R02M01")
R02M02 <- load.LAM(file = paste0(path, "R02M02.txt"), name = "R02M02")
R02M03 <- load.LAM(file = paste0(path, "R02M03.txt"), name = "R02M03")
R02M54 <- load.LAM(file = paste0(path, "R02M54.txt"), name = "R02M54")
R02M55 <- load.LAM(file = paste0(path, "R02M55.txt"), name = "R02M55")
R02M56 <- load.LAM(file = paste0(path, "R02M56.txt"), name = "R02M56")
R02M57 <- load.LAM(file = paste0(path, "R02M57.txt"), name = "R02M57")

LAMs <- list(M01 = R02M01,
             M02 = R02M02,
             M03 = R02M03,
             M54 = R02M54,
             M55 = R02M55,
             M56 = R02M56,
             M57 = R02M57)

## Input Metadata

Metadata <- read_excel("data/R02/Metadata/Metadata_R02_Insemination_expt.xlsx",
                       col_types = c("numeric", "numeric", "text", "text", "text", "numeric", "numeric"))

saveRDS(object = Metadata, file = paste0(path, "Metadata.rds"))

## Consolidate LAM data with Metadata

source("./function/consolidate.LAMs.R") # this function will be included in 'abcdee' in due course

activity <- consolidate.LAMs(LAMs, Metadata)

source("./function/xtsList2df.R")  # this function will be included in 'abcdee' in due course

activityDF <- xtsList2df(activity)

saveRDS(object = activity, file = paste0(path, "Activity.rds"))

# Clean .GlobalEnv

rm(R02M01); rm(R02M02); rm(R02M03); rm(R02M54); rm(R02M55); rm(R02M56); rm(R02M57)

rm(path)