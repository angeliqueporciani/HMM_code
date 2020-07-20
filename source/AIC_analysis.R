## recup et attach des metadata aux AIC de tous les ID 
library(tidyverse)
# ce script est juste pour recuperer les données d'AIC comme je voulais sans refaire tourner la boucle incomplète que j'avais au début pour le script02. 
# il ne sera plus utile pour de future analyse sur d'autre data. 

# load data 
setwd("/Users/porciani/ownCloud/PostDocAnorythm/Anorythm/HMM_code")
data.ind <- readRDS("./Data/data.ind.rds")
AIC_allID <- readRDS("/Users/porciani/Dropbox/Share with Angelique Amadou/Projet Insemination/output/HMM/AIC_allID.rds")

AIC_DF <- vector("list", 200)
for (j in seq_along(AIC_allID)){
  
attributes(AIC_allID[[j]]) <- data.ind[[j]][1,c(1,5,6,7)]
AIC_DF[[j]] <- cbind.data.frame(attributes(AIC_allID[[j]]), AIC=AIC_allID[[j]][1:5], States=AIC_allID[[j]][6:10]) 

}

AIC_DF_tot <- do.call(rbind, AIC_DF)

AICdf <- AIC_DF_tot %>%
  subset(States!="0"&Status!="Non-inseminated")%>%
  group_by(ID)%>%
  filter(AIC == min(AIC)) %>% 
  mutate(Supfac=paste0(Status, Food))%>%
  mutate(States=as.numeric(States))%>%
  ungroup()

ggplot(AICdf, aes(x=Supfac, y=States))+
 geom_boxplot()+
  geom_point()
t1 <- table(AICdf$Supfac, AICdf$States)
addmargins(t1) 
library(kableExtra)
kable(addmargins(t1), "markdown")

