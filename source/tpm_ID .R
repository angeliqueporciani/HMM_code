## Recupération des probabilités de transition pour chaque individu 
# Ce package permet de recuperer les proba de transition de chaque individu en partant du principe qu'on ajuste un modèle à 4 états. 
# Il n'y a que des covariables temporelles (heure du jour) et peut être le jour qui peut être ajouté? 


# 1. load fonction + package
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

# creation de list utilisé par les fonctions. 
splineList<-c("bs","ns","bSpline","mSpline","cSpline","iSpline")
meansList<-c("matrix","numeric","integer","logical","Date","POSIXlt","POSIXct","difftime")

# 2. appel du modele selectionné dans les parties precedentes. 

m <- readRDS("./output/modC_1_SLD.rds")

# 3. Recup de la tpm (transition proba matrix)

ID <- unique(m$data$ID)
nbStates <- length(m$stateNames)

formula<-m$conditions$formula
newForm <- newFormulas(formula, nbStates)
formulaStates <- newForm$formulaStates
formterms <- newForm$formterms
newformula <- newForm$newformula
nbCovs <- ncol(model.matrix(newformula,m$data))-1 # substract intercept column


alpha <- 0.05
gamInd<-(length(m$mod$estimate)-(nbCovs+1)*nbStates*(nbStates-1)+1):(length(m$mod$estimate))-ncol(m$covsDelta)*(nbStates-1)*(!m$conditions$stationary)
quantSup<-qnorm(1-(1-alpha)/2)
beta <- m$mle$beta
delta <- m$mle$delta

 # values of each covariate
rawCovs <- m$rawCovs[which(m$data$ID %in% ID),,drop=FALSE]
rawCovs <- m$rawCovs
meanCovs <- colSums(rawCovs)/nrow(rawCovs)
    
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

##renvoi un array qui contient toutes les matrices de transition pour chaque heure 
trMat[,,1]##=matrice de transition pour la premiere heure (minuit)

##si on veut récuperer toute les transitions de 1 vers autres états il faut prendre la première ligne et toute les colonnes de toutes les tpm 
##ex : transition de 1 -> 1  pour toutes les heures

trMat[1,1,]

# transfo de l'array en df 
tpm <- trMat 
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

DF_trMat$Status <- rep(unique(m$data$Status))
DF_trMat$Food <- rep(unique(m$data$Food))
DF_trMat$Hour <- tempCovs[,cov]

# idée: faut recup chaque tpm pour chq id dans un DFnest peut être ? 
