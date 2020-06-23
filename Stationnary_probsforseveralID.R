## Stationnary probs for several individuals 

library(momentuHMM)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(tidyverse)
library(plyr)
library(Rmisc)

# load data (best model for each ID)
# functions below allow to load all models in one steps and stock them in a list. 
library(here)

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
sp3 <- list()
sp4 <- list()
sptot <- list()
for(i in 1:length(dataset_list)){
sp <- stationary(model = dataset_list[[i]], covs=data.frame(Hour=c(0:24)))
df <- sort(as.data.frame(dataset_list[[i]]$CIreal$AC$est))
sp <- as.data.frame(sp[,c(names(df))])
sp$ID <- unique(dataset_list[[i]]$data$ID)
sp$Food <- unique(dataset_list[[i]]$data$Food)
sp$Status<- unique(dataset_list[[i]]$data$Status)
sp$Time<- 0:24
sptot[[i]] <- sp
}


threestates <- c(1,3,4,7)
fourstates <- c(2,5,6,8,9,10)
sp3 <- sptot[threestates]
sp4 <- sptot[fourstates]

for (i in 1:length(sp3))
{
  colnames(sp3[[i]]) <- c("state1", "state2", "state3", "ID", "Food","Status","Time")
  
}

for (i in 1:length(sp4))
{
  colnames(sp4[[i]]) <- c("state1", "state2", "state3","state4", "ID", "Food","Status","Time")
  
}

df3 <- data.frame()
for (i in 1:length(sp3))
{
  df3 <-  rbind(df3, sp3[[i]])
}

df4 <- data.frame()
for (i in 1:length(sp4))
{
  df4 <-  rbind(df4, sp4[[i]])
}

## verif 
summary(df4)
summary(df3)

df4 <- subset(df4, ID!=1 & ID!=115)
df4 <- droplevels(df4)
df3 <- droplevels(df3)

##Plot creation for all states----

# for 4 states (VS)---
p14<-ggplot(data=df4, aes(x=Time, y=state1, colour=ID)) + 
  geom_line()+theme_bw()+
  labs(title="Probability to be in state1", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p14

p24 <-ggplot(data=df4, aes(x=Time, y=state2,colour=ID)) + geom_line()+theme_bw()+
  labs(title="Probability to be in state2", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p24

p34<-ggplot(data=df4, aes(x=Time, y=state3,colour=ID)) + geom_line()+theme_bw()+
  labs(title="Probability to be in state3", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p34

p44<-ggplot(data=df4, aes(x=Time, y=state4,colour=ID)) + geom_line()+theme_bw()+
  labs(title="Probability to be in state4", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p44
# IC pour le calcul des stationary probs ? Ca existe ?

# for state3(IB)---
p13<-ggplot(data=df3, aes(x=Time, y=state1,colour=ID)) + 
  geom_line()+theme_bw()+
  labs(title="Probability to be in state1", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p13

p23 <-ggplot(data=df3, aes(x=Time, y=state2,colour=ID)) + geom_line()+theme_bw()+
  labs(title="Probability to be in state2", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p23

p33<-ggplot(data=df3, aes(x=Time, y=state3,colour=ID)) + geom_line()+theme_bw()+
  labs(title="Probability to be in state3", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p33


# Comparaisons---
df3$state4 <- NA
df3 <- df3[,c(names(df4))]
dftot <- rbind(df4,df3)
str()
st1 <- summarySE(dftot,measurevar="state1", groupvars=c("Time","Food","Status"),na.rm=T)

p1<-ggplot(data=st1, aes(x=Time, y=state1)) + 
  geom_line(aes(y = state1, colour= Food))+theme_bw()+
  geom_ribbon(aes(ymin = state1-ci, ymax = state1+ci, shape=Food), fill = "grey70", alpha=0.5)+
  labs(title="Probability to be in state1", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p1

st2 <- summarySE(dftot,measurevar="state2", groupvars=c("Time","Food","Status"),na.rm=T)
p2<-ggplot(data=st2, aes(x=Time, y=state2)) + 
  geom_line(aes(y = state2, colour= Food))+theme_bw()+
  geom_ribbon(aes(ymin = state2-ci, ymax = state2+ci, shape=Food), fill = "grey70", alpha=0.4)+
  labs(title="Probability to be in state3", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p2

st3 <- summarySE(dftot,measurevar="state3", groupvars=c("Time","Food","Status"),na.rm=T)
p3<-ggplot(data=st3, aes(x=Time, y=state3)) + 
  geom_line(aes(y = state3, colour= Food))+theme_bw()+
  geom_ribbon(aes(ymin = state3-ci, ymax = state3+ci, shape=Food), fill = "grey70", alpha=0.4)+
  labs(title="Probability to be in state3", x="Time (h)", y="Probability")+
  theme(plot.title=element_text(hjust = 0.5))+ scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p3

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


## graph for 1 ID (the 23)

ID23 <- subset(df4, ID==23)
ID23b <- ID23%>%
pivot_longer(1:4, names_to = "States", values_to = "value")

ID23b%>%
  arrange(Time, tbreaks)

# graphique
tbreaks <- c(12, 14, 16, 18, 20, 22, 24, 0, 2, 4, 6, 8, 10)

breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24)

p23<-ggplot(data=ID23b, aes(x=Time, y=value, colour=States)) + 
  geom_line()+theme_bw()+
  labs(title="Stationnary probabilities for ID 2", x="Time (h)", y="Probabilities")+
  
  theme(plot.title=element_text(hjust = 0.5))+ 
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10, 12, 14,16, 18, 20, 22, 24))
p23
str(ID23b)

## recup lambda +IC 
lambda <- list()
ICL <- list()
ICU <- list()

for (i in 1:length(dataset_list)){
lambda[[i]] <- cbind(dataset_list[[i]]$CIreal$AC$est, unique(dataset_list[[i]]$data$ID))
ICL[[i]] <- cbind(dataset_list[[i]]$CIreal$AC$lower, unique(dataset_list[[i]]$data$ID))
ICU[[i]] <- cbind(dataset_list[[i]]$CIreal$AC$upper, unique(dataset_list[[i]]$data$ID))
}

VSnum <- c(2,6,8,9,10)

for (i in VSnum){
  print(ICU[[i]])
}


