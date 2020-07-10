w2n <- function(wpar,bounds,parSize,nbStates,nbCovs,estAngleMean,circularAngleMean,consensus,stationary,cons,fullDM,DMind,workcons,nbObs,dist,Bndind,nc,meanind,covsDelta,workBounds)
{
  
  # identify initial distribution parameters
  if(!stationary & nbStates>1){
    nbCovsDelta <- ncol(covsDelta)-1 # substract intercept column
    
    foo <- length(wpar)-(nbCovsDelta+1)*(nbStates-1)+1
    
    tmpwpar <- w2wn(wpar[foo:length(wpar)],workBounds$delta)
    
    delta <- c(rep(0,nbCovsDelta+1),tmpwpar)
    deltaXB <- covsDelta%*%matrix(delta,nrow=nbCovsDelta+1)
    expdelta <- exp(deltaXB)
    delta <- expdelta/rowSums(expdelta)
    for(i in which(!is.finite(rowSums(delta)))){
      tmp <- exp(Brobdingnag::as.brob(deltaXB[i,]))
      delta[i,] <- as.numeric(tmp/Brobdingnag::sum(tmp))
    }
    wpar <- wpar[-(foo:length(wpar))]
  }
  else delta <- NULL
  
  # identify regression coefficients for the transition probabilities
  if(nbStates>1) {
    foo <- length(wpar)-(nbCovs+1)*nbStates*(nbStates-1)+1
    
    tmpwpar <- w2wn(wpar[foo:length(wpar)],workBounds$beta)
    
    beta <- tmpwpar
    beta <- matrix(beta,nrow=nbCovs+1)
    wpar <- wpar[-(foo:length(wpar))]
  }
  else beta <- NULL
  
  distnames <- names(dist)
  parCount<- lapply(fullDM,ncol)
  for(i in distnames[unlist(circularAngleMean)]){
    parCount[[i]] <- length(unique(gsub("cos","",gsub("sin","",colnames(fullDM[[i]])))))
  }
  parindex <- c(0,cumsum(unlist(parCount))[-length(fullDM)])
  names(parindex) <- names(fullDM)
  
  parlist<-list()
  
  for(i in distnames){
    
    tmpwpar<-wpar[parindex[[i]]+1:parCount[[i]]]
    
    if(estAngleMean[[i]] & Bndind[[i]]){ 
      bounds[[i]][,1] <- -Inf
      bounds[[i]][which(bounds[[i]][,2]!=1),2] <- Inf
      
      foo <- length(tmpwpar) - nbStates + 1
      x <- tmpwpar[(foo - nbStates):(foo - 1)]
      y <- tmpwpar[foo:length(tmpwpar)]
      angleMean <- Arg(x + (0+1i) * y)
      kappa <- sqrt(x^2 + y^2)
      tmpwpar[(foo - nbStates):(foo - 1)] <- angleMean
      tmpwpar[foo:length(tmpwpar)] <- kappa
    }
    parlist[[i]]<-w2nDM(tmpwpar,bounds[[i]],fullDM[[i]],DMind[[i]],cons[[i]],workcons[[i]],nbObs,circularAngleMean[[i]],consensus[[i]],nbStates,0,nc[[i]],meanind[[i]],workBounds[[i]])
    
    if((dist[[i]] %in% angledists) & !estAngleMean[[i]]){
      tmp<-matrix(0,nrow=(parSize[[i]]+1)*nbStates,ncol=nbObs)
      tmp[nbStates+1:nbStates,]<-parlist[[i]]
      parlist[[i]] <- tmp
    }
    
  }
  
  parlist[["beta"]]<-beta
  parlist[["delta"]]<-delta
  
  return(parlist)
}

w2wn <- function(wpar,workBounds,k=0){
  
  ind1<-which(is.finite(workBounds[,1]) & is.infinite(workBounds[,2]))
  ind2<-which(is.finite(workBounds[,1]) & is.finite(workBounds[,2]))
  ind3<-which(is.infinite(workBounds[,1]) & is.finite(workBounds[,2]))
  
  wpar[ind1] <- exp(wpar[ind1])+workBounds[ind1,1]
  wpar[ind2] <- (workBounds[ind2,2]-workBounds[ind2,1]) * boot::inv.logit(wpar[ind2])+workBounds[ind2,1]
  wpar[ind3] <- -(exp(-wpar[ind3]) - workBounds[ind3,2])
  
  if(k) wpar <- wpar[k]
  return(wpar)
}

w2nDM<-function(wpar,bounds,DM,DMind,cons,workcons,nbObs,circularAngleMean,consensus,nbStates,k=0,nc,meanind,workBounds){
  
  wpar <- w2wn(wpar,workBounds)
  
  a<-bounds[,1]
  b<-bounds[,2]
  
  zeroInflation <- any(grepl("zeromass",rownames(bounds)))
  oneInflation <- any(grepl("onemass",rownames(bounds)))
  
  piInd<-(abs(a- -pi)<1.e-6 & abs(b - pi)<1.e-6)
  ind1<-which(piInd)
  zoInd <- as.logical((grepl("zeromass",rownames(bounds)) | grepl("onemass",rownames(bounds)))*(zeroInflation*oneInflation))
  ind2<-which(zoInd)
  ind3<-which(!piInd & !zoInd)
  
  if(!consensus){
    XB <- p <- getXB(DM,nbObs,wpar,cons,workcons,DMind,circularAngleMean,consensus,nbStates,nc,meanind)
    l_t <- matrix(1,nrow(XB),ncol(XB))
  } else {
    tmpXB <- getXB(DM,nbObs,wpar,cons,workcons,DMind,circularAngleMean,consensus,nbStates,nc,meanind)
    XB <- p <- tmpXB$XB
    l_t <- matrix(tmpXB$l_t,nrow(XB),ncol(XB))
  }
  
  if(length(ind1) & !circularAngleMean)
    p[ind1,] <- (2*atan(XB[ind1,]))
  
  if(length(ind2)){
    for(j in 1:nbStates){
      zoParInd <- which(grepl(paste0("zeromass_",j),rownames(bounds)) | grepl(paste0("onemass_",j),rownames(bounds)))
      zoPar <- rbind(XB[zoParInd,,drop=FALSE],rep(0,ncol(XB)))
      expzo <- exp(zoPar)
      zo <- expzo/rep(colSums(expzo),each=3)
      for(i in which(!is.finite(colSums(zo)))){
        tmp <- exp(Brobdingnag::as.brob(zoPar[,i]))
        zo[,i] <- as.numeric(tmp/Brobdingnag::sum(tmp))
      }
      p[zoParInd,] <- zo[-3,]
    }
  }
  
  ind31<-ind3[which(is.finite(a[ind3]) & is.infinite(b[ind3]))]
  ind32<-ind3[which(is.finite(a[ind3]) & is.finite(b[ind3]))]
  ind33<-ind3[which(is.infinite(a[ind3]) & is.finite(b[ind3]))]
  
  p[ind31,] <- (l_t[ind31,,drop=FALSE] * exp(XB[ind31,,drop=FALSE])+a[ind31])
  p[ind32,] <- ((b[ind32]-a[ind32])*(l_t[ind32,,drop=FALSE] * boot::inv.logit(XB[ind32,,drop=FALSE]))+a[ind32])
  p[ind33,] <- -(exp(-XB[ind33,,drop=FALSE]) - b[ind33])
  
  if(!any(is.na(p))){ 
    if(any(p<a | p>b)){
      stop("Scaling error. Check initial values and bounds.")
    }
  }
  
  if(k) {
    p <- p[k]
  } else if(DMind) {
    p <- matrix(p,length(ind1)+length(ind2)+length(ind3),nbObs)
  }
  return(p)
} 