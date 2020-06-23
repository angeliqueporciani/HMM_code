## récuperation des AIC (nb etats) pour chaque ID 

AICSID16 <- readRDS("./output/ID16/AICID16.rds")
AICSID23 <- readRDS("./output/ID23/AICID23.rds")
AICSID34 <- readRDS("./output/ID34/AICID34.rds")
AICSID63 <- readRDS("./output/ID63/AICID63.rds")
AICSID115 <- readRDS("./output/ID115/AICID115.rds")

ID16 <- readRDS("./output/ID16/ID16.rds")
ID23 <- readRDS("./output/ID23/ID23.rds")
ID34 <- readRDS("./output/ID34/ID34.rds")
ID63 <- readRDS("./output/ID63/ID63.rds")
ID115 <- readRDS("./output/ID115/ID115.rds")


IDVSlist <- list(ID16, ID23, ID34, ID63, ID115)
AICSlist <- list(AICSID16, AICSID23, AICSID34, AICSID63, AICSID115)

AICDF <- list()
for (i in 1:length(AICSlist))
{
  AICDF[[i]] <- as.data.frame(AICSlist[[i]])
}

AICDF2 <- data.frame()
for (i in 1:length(IDVSlist)){
  AICDF[[i]]$ID <-rep(unique(IDVSlist[[i]]$ID))
  AICDF2 <- rbind(AICDF2, AICDF[[i]][-1,]) 
}

colnames(AICDF2) <- c("AIC", "nbStates", "ID")

## ggplot 
library(ggplot2)
AICSplot <- ggplot(data=AICDF2, aes(x=nbStates, y=AIC,colour=ID)) + 
  geom_line()+theme_bw()+
  labs(title="AIC of 5 Virgin sugar fed female", x="Nb of states", y="AIC")+
  theme(plot.title=element_text(hjust = 0.5))
AICSplot

ggsave("./img/AICStateVS.pdf")
