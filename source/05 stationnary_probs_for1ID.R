#################################################################
###Stationnary probability : probs of being in one state ########
#################################################################

## We have 2 strategies for graphical representation : 
## 1. For one individual we need to have the 4 states on the same graphics with the metadata in the title. 
## 2. For several individual we need to have the stationnary probs for one state of all individuals choosen on the same graph. 
## NB: when we work with more than one individual we need to check that the states are the same L1<L2<L3<L4. 

#Load packages 
library(momentuHMM)
library( ggplot2)
library(dplyr)
library(tidyverse)
# Load data
# load the best model selected in step 03. 

mod <- readRDS("./output/modDC2423.rds")
modnull <- readRDS("./output/modnull2_23.rds")

##use the stationnary fonction from momentuHMM for extracting stationnary probs
sp <- stationary(model = mod, covs=data.frame(Hour=c(0:24)))

# 1. Frist strategy, for one individual. 

###basic plots from R, not very cosmetic
par(mfrow=c(1,1))
plot(sp[,1], type="l", ylim=c(0, 1), col="red")
par(new=TRUE)
plot(sp[,2], type="l", ylim=c(0,1), col="blue")
par(new=TRUE)
plot(sp[,3], type="l", ylim=c(0,1), col="green")
par(new=TRUE)
plot(sp[,4], type="l", ylim=c(0,1))

##ggplot for more comsetic figures
##transfo in DF for all the covariable factor
sp_DF<- data.frame(sp)

# Change the DF to have 2 columns, one sprobs and other states
spDF2 <- gather(sp_DF, "States", "SP")

# Add time variables 
x <- 0:24
spDF2$Time <- rep(x)##add to DF

# graph
spplot<-ggplot(data=spDF2, aes(x=Time, y=SP,colour=States)) + 
  geom_line()+theme_bw()+
  labs(title=paste0("Stationnary probabilities of ID",unique(mod_DC$data$ID),"(",unique(mod_DC$data$Food),"_",unique(mod_DC$data$Status),")"), x="Time (h)", y="Probabilities")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))

spplot
# save the result
ggsave(paste0("./img/Stationnary probs/ID",unique(mod$data$ID),".pdf"))
## we need to remind : (and maybe include that in graphic)
mod$CIreal$AC$est

# These steps may be replicated for all individuals you want. Take care of the model name at the begining. 

timeInStates(mod)


### Use of sp coming from model of one individual. 
