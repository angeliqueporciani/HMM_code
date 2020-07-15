##test recup des paramètres des matrices de transition 

##fonction utile pour ce script (toute issues de momentuhmm)

#@getXB
#@w2n et autre
#@delta_bc
#@fonction_plot_recup (getSpline...)
#@tr_matrix_rcpp
#@cosinorCos et cosinorSin (fonction plot_recup)
#@fonctionXBloop et is.formula
#@newFormulas
source("./function/fonction_tpm/delta_bc.R")
source("./function/fonction_tpm/fonction_is_and_XBloop.R")
source("./function/fonction_tpm/cosinor.R")
source("./function/fonction_tpm/stateFormulas.R")
source("./function/fonction_tpm/newFormulas.R")
source("./function/fonction_tpm/getSplineFormula.R")
source("./function/fonction_tpm/getXB.R")
source("./function/fonction_tpm/fonction_w2n_et_autre.R")
source("./function/fonction_tpm/trMatrix_rcpp.R")
library(momentuHMM)

splineList<-c("bs","ns","bSpline","mSpline","cSpline","iSpline")
meansList<-c("matrix","numeric","integer","logical","Date","POSIXlt","POSIXct","difftime")

##transfo du modèle
m <- modDC2434
m <- readRDS("./output/modC_1_SLD.rds")
m <- delta_bc(m)


######################
## Prepare the data ##
######################
##memo: faire une fonction
ID <- unique(m$data$ID)
nbStates <- length(m$stateNames)
F <- unique(m$data$Food)
S <- unique(m$data$Status)


##creation covs
covs=data.frame(Status="Virgin")##à changer en fontion des la cov qu'on veut pour les tpm 


if(!is.data.frame(covs)) stop('covs must be a data frame')
if(nrow(covs)>1) stop('covs must consist of a single row')
if(!all(names(covs) %in% names(m$data))) stop('invalid covs specified')
if(any(names(covs) %in% "ID")) covs$ID<-factor(covs$ID,levels=unique(m$data$ID))
for(j in names(m$data)[which(names(m$data) %in% names(covs))]){
  if(inherits(m$data[[j]],"factor")) covs[[j]] <- factor(covs[[j]],levels=levels(m$data[[j]]))
  if(is.na(covs[[j]])) stop("check value for ",j)
}
for(j in names(m$data)[which(!(names(m$data) %in% names(covs)))]){
  if(inherits(m$data[[j]],"factor")) covs[[j]] <- m$data[[j]][which(m$data$ID %in% ID)][1]
  else if(inherits(m$data[[j]],"angle")) covs[[j]] <- CircStats::circ.mean(m$data[[j]][which(m$data$ID %in% ID)][!is.na(m$data[[j]][which(m$data$ID %in% ID)])])
  else if(any(class(m$data[[j]]) %in% meansList)) covs[[j]]<-mean(m$data[[j]][which(m$data$ID %in% ID)],na.rm=TRUE)
}
#######################
##T.P. en fonction covs
#######################
formula<-m$conditions$formula
newForm <- newFormulas(formula, nbStates)
formulaStates <- newForm$formulaStates
formterms <- newForm$formterms
newformula <- newForm$newformula
nbCovs <- ncol(model.matrix(newformula,m$data))-1 # substract intercept column

require(momentuHMM)

par(mar=c(5,4,4,2)-c(0,0,1.5,1)) # bottom, left, top, right
alpha <- 0.05
gamInd<-(length(m$mod$estimate)-(nbCovs+1)*nbStates*(nbStates-1)+1):(length(m$mod$estimate))-ncol(m$covsDelta)*(nbStates-1)*(!m$conditions$stationary)
quantSup<-qnorm(1-(1-alpha)/2)
beta <- m$mle$beta
delta <- m$mle$delta
if(nrow(beta)>1) {
  
  # values of each covariate
  rawCovs <- m$rawCovs[which(m$data$ID %in% ID),,drop=FALSE]
  if(is.null(covs)) {
    rawCovs <- m$rawCovs
    meanCovs <- colSums(rawCovs)/nrow(rawCovs)
  } else {
    rawCovs <- m$data[,names(covs),drop=FALSE]
    meanCovs <- as.numeric(covs)
  }
  
  for(cov in 1:ncol(rawCovs)) {
    
    par(mfrow=c(nbStates,nbStates))
    
    if(!is.factor(rawCovs[,cov])){
      
      gridLength <- 101
      
      inf <- min(rawCovs[,cov],na.rm=T)
      sup <- max(rawCovs[,cov],na.rm=T)
      
      # set all covariates to their mean, except for "cov"
      # (which takes a grid of values from inf to sup)
      tempCovs <- data.frame(matrix(covs[names(rawCovs)][[1]],nrow=gridLength,ncol=1))
      if(ncol(rawCovs)>1)
        for(i in 2:ncol(rawCovs))
          tempCovs <- cbind(tempCovs,rep(covs[names(rawCovs)][[i]],gridLength))
      
      tempCovs[,cov] <- seq(inf,sup,length=gridLength)
    } else {
      gridLength<- nlevels(rawCovs[,cov])
      # set all covariates to their mean, except for "cov"
      tempCovs <- data.frame(matrix(covs[names(rawCovs)][[1]],nrow=gridLength,ncol=1))
      if(ncol(rawCovs)>1)
        for(i in 2:ncol(rawCovs))
          tempCovs <- cbind(tempCovs,rep(covs[names(rawCovs)][[i]],gridLength))
      
      tempCovs[,cov] <- as.factor(levels(rawCovs[,cov]))
    }
    
    names(tempCovs) <- names(rawCovs)
    tmpcovs<-covs[names(rawCovs)]
    for(i in which(unlist(lapply(rawCovs,is.factor)))){
      tempCovs[[i]] <- factor(tempCovs[[i]],levels=levels(rawCovs[,i]))
      tmpcovs[i] <- as.character(tmpcovs[[i]])
    }
    for(i in which(!unlist(lapply(rawCovs,is.factor)))){
      tmpcovs[i]<-round(covs[names(rawCovs)][i],2)
    }
    
    tmpSplineInputs<-getSplineFormula(newformula,m$data,tempCovs)
    
    desMat <- model.matrix(tmpSplineInputs$formula,data=tmpSplineInputs$covs)
    
    trMat <- trMatrix_rcpp(nbStates,beta,desMat,m$conditions$betaRef)
    
    
  }
}

##renvoi un array qui contient toutes les matrices de transition pour chaque heure 
trMat[,,1]##=matrice de transition pour la premiere heure (minuit)

##si on veut récuperer toute les transitions de 1 vers autres états il faut prendre la première ligne et toute les colonnes de toutes les tpm 
##ex : transition de 1 -> 1  pour toutes les heures

trMat[1,1,]

#####Création du dataframe contenant les probabilités de transitions en fonction du temps pour chaque type de transition 

##1. pour chaque status(vierge/inseminé) stocker la trMat dans un array identifié
###a.virgin
tpm <- trMat 

##b.inseminated
inseminatedtpm <- trMat

##changement des dimension des matrices pour avoir un dataframe de toute les proba en fonction de l'heure pour une transition 
##virgin
dim(tpm) <- c(nbStates*nbStates, length(tempCovs[,cov]))
tpm <- t(tpm)
DF_trMat <- data.frame(tpm)
colnames(DF_trMat) <- c("1 -> 1","1 -> 2",
                         "1 -> 3",
                         "1 -> 4",
                         "2 -> 1",
                         "2 -> 2",
                         "2 -> 3",
                         "2 -> 4",
                         "3 -> 1",
                         "3 -> 2",
                         "3 -> 3",
                         "3 -> 4",
                         "4 -> 1",
                         "4 -> 2",
                         "4 -> 3",
                         "4 -> 4")

DF_trMat$status <- rep("virgin")
DF_trMat$hour <- tempCovs[,cov]

##inseminated
dim(inseminatedtpm) <- c(nbStates*nbStates, length(tempCovs[,cov]))
inseminatedtpm <- t(inseminatedtpm)
DF_ItrMat <- data.frame(inseminatedtpm)
colnames(DF_ItrMat) <- c("1 -> 1","1 -> 2",
                         "1 -> 3",
                         "1 -> 4",
                         "2 -> 1",
                         "2 -> 2",
                         "2 -> 3",
                         "2 -> 4",
                         "3 -> 1",
                         "3 -> 2",
                         "3 -> 3",
                         "3 -> 4",
                         "4 -> 1",
                         "4 -> 2",
                         "4 -> 3",
                         "4 -> 4")

DF_ItrMat$status <- rep("inseminated")
DF_ItrMat$hour <- tempCovs[,cov]

##fusion par ligne des matrices pour avoir un DF complet utilisable sous ggplot
DF_tpm <- rbind(DF_ItrMat,DF_VtrMat)
dim(DF_tpm)

#########################
###Plot en png#########
#######################
#stockage dans le repertoire courant
library(ggplot2)

for (i in 1:(nbStates*nbStates)){
  #png(file = paste0("switching_state", i, ".png")) 
  p <- ggplot(data=DF_tpm, aes(x=hour, y= DF_tpm[,1],colour=status)) + geom_line()+theme_bw()+
    labs(title=names(DF_tpm[1]), x="Time (h)", y="Probability")+
    theme(plot.title=element_text(hjust = 0.5,size=10))+
    scale_x_continuous(breaks=c(0,2,4,6, 8, 10, 12, 14,16, 18, 20, 22, 24))
  print(p)
  #dev.off()
}


###plot par états initiaux (faire la même chose pour les inséminées)
par(mfrow = c(2,2), las = 1, bty = "n")

plot(tempCovs[,cov], virgintpm[,1], type="l", ylim=c(0,1), xlab=names(rawCovs)[cov], main = "Inactivity => {1...4}", ylab = "Transition Probability")
lines(tempCovs[,cov], virgintpm[,2], col = "green")
lines(tempCovs[,cov], virgintpm[,3], col = "blue")
lines(tempCovs[,cov], virgintpm[,4], col = "red")

plot(tempCovs[,cov], virgintpm[,9], type="l", ylim=c(0,1), xlab=names(rawCovs)[cov], main = "Baseline Activity => {1...4}", ylab = "Transition Probability")
lines(tempCovs[,cov], virgintpm[,10], col = "green")
lines(tempCovs[,cov], virgintpm[,11], col = "blue")
lines(tempCovs[,cov], virgintpm[,12], col = "red")

plot(tempCovs[,cov], virgintpm[,5], type="l", ylim=c(0,1), xlab=names(rawCovs)[cov], main = " Stronger Activity => {1...4}", ylab = "Transition Probability")
lines(tempCovs[,cov], virgintpm[,6], col = "green")
lines(tempCovs[,cov], virgintpm[,7], col = "blue")
lines(tempCovs[,cov], virgintpm[,8], col = "red")

plot(tempCovs[,cov], virgintpm[,13], type="l", ylim=c(0,1), xlab=names(rawCovs)[cov], main = "Hyperactivity => {1...4}", ylab = "Transition Probability")
lines(tempCovs[,cov], virgintpm[,14], col = "green")
lines(tempCovs[,cov], virgintpm[,15], col = "blue")
lines(tempCovs[,cov], virgintpm[,16], col = "red")

par(mfrow = c(1,1))

trProbsSE_10 <- getTrProbs(m, formula=m$conditions$formula)
