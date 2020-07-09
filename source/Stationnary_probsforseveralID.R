## Stationnary probs for several individuals 

library(momentuHMM)
library(ggplot2)
library(gridExtra)
library(cowplot)
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


# then, dataset_list contains best models results for each individuals \o/ 

sptot <- list()
for(i in 1:length(dataset_list)){
sp <- stationary(model = dataset_list[[i]])
covs <- dataset_list[[i]]$rawCovs
df <- sort(as.data.frame(dataset_list[[i]]$CIreal$AC$est))
sp <- as.data.frame(sp[,c(names(df))])
sp <- cbind(sp, covs)
sp$ID <- unique(dataset_list[[i]]$data$ID)
sp$Food <- unique(dataset_list[[i]]$data$Food)
sp$Status<- unique(dataset_list[[i]]$data$Status)
colnames(sp) <- c("state1", "state2", "state3","state4", "Hour", "ID", "Food","Status")
sptot[[i]] <- sp
}



# PLOT----

# If we work with the same number of states for all ID 
dftot <- do.call(rbind.data.frame, sptot)
st1 <- summarySE(dftot,measurevar="state1", groupvars=c("Time","Food","Status"),na.rm=T)
st2 <- summarySE(dftot,measurevar="state2", groupvars=c("Time","Food","Status"),na.rm=T)
st3 <- summarySE(dftot,measurevar="state3", groupvars=c("Time","Food","Status"),na.rm=T)
st4 <- summarySE(dftot,measurevar="state4", groupvars=c("Time","Food","Status"),na.rm=T)

##Plot creation for each state and stat probs comming from all ID ----
# we can compare for each state the behavior of the sprobs of each groups. 

p1<-ggplot(data=st1, aes(x=Time, y=state1)) + 
  geom_line(aes(y = state1, colour= Food))+theme_bw()+
  geom_ribbon(aes(ymin = state1-ci, ymax = state1+ci, shape=Food), fill = "grey70", alpha=0.5)+
  labs(title="Probability to be in state1", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p1

p2<-ggplot(data=st2, aes(x=Time, y=state2)) + 
  geom_line(aes(y = state2, colour= Food))+theme_bw()+
  geom_ribbon(aes(ymin = state2-ci, ymax = state2+ci, shape=Food), fill = "grey70", alpha=0.4)+
  labs(title="Probability to be in state3", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p2

p3<-ggplot(data=st3, aes(x=Time, y=state3)) + 
  geom_line(aes(y = state3, colour= Food))+theme_bw()+
  geom_ribbon(aes(ymin = state3-ci, ymax = state3+ci, shape=Food), fill = "grey70", alpha=0.4)+
  labs(title="Probability to be in state3", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p3

p4<-ggplot(data=st4, aes(x=Time, y=state3)) + 
  geom_line(aes(y = state4, colour= Food))+theme_bw()+
  geom_ribbon(aes(ymin = state4-ci, ymax = state4+ci, shape=Food), fill = "grey70", alpha=0.4)+
  labs(title="Probability to be in state3", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p4

# Other method : Allowing the visualisation of curve for each ID for one state.----

p14<-ggplot(data=df4, aes(x=Hour, y=state1, colour=ID)) + 
  geom_line()+theme_bw()+
  labs(title="Probability to be in state1", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p14

p24 <-ggplot(data=df4, aes(x=Hour, y=state2,colour=ID)) + geom_line()+theme_bw()+
  labs(title="Probability to be in state2", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p24

p34<-ggplot(data=df4, aes(x=Hour, y=state3,colour=ID)) + geom_line()+theme_bw()+
  labs(title="Probability to be in state3", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p34

p44<-ggplot(data=df4, aes(x=Hour, y=state4,colour=ID)) + geom_line()+theme_bw()+
  labs(title="Probability to be in state4", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p44

#------------
# changer l'esthétique des rubans. 
# proposer d'autre graphes ? 

##Several graph on the same grid 
library(gridExtra)
library(cowplot)

##1. on crée les graphiques
##2. on recup la légende avec une fonction 
##3. on retire la légende de graphiques qu'on veut plus 
##4. on crée le panel avec juste la légende qu'on veut 

##fonction qui permet de recup la légende d'un graph ggplot pour la remettre ensuite que là ou on veux
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
##2.  

legend <- get_legend(p1)
##3. 
p1 <- p1+theme(legend.position="none")
p2 <- p2+theme(legend.position="none")
p3 <- p3+theme(legend.position="none")
p4 <- p4+theme(legend.position="none")

##grid with all graphs
plot_grid(p1, p2, p3, p4, ncol = 2, nrow = 2)
grid.arrange(p1, p2, legend,p3, p4, ncol=3, nrow=2) 




