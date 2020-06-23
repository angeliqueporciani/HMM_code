delta_bc <- function(m){
  
  if(is.momentuHMM(m) | is.miSum(m)){
    if(is.null(m$conditions$workBounds)){
      distnames <- names(m$conditions$dist)
      
      parCount<- lapply(m$conditions$fullDM,ncol)
      for(i in distnames[unlist(m$conditions$circularAngleMean)]){
        parCount[[i]] <- length(unique(gsub("cos","",gsub("sin","",colnames(m$conditions$fullDM[[i]])))))
      }
      parindex <- c(0,cumsum(unlist(parCount))[-length(m$conditions$fullDM)])
      names(parindex) <- distnames
      
      workBounds <- vector('list',length(distnames))
      names(workBounds) <- distnames
      if(is.miSum(m)){
        beta <- m$Par$beta$beta$est
        delta <- m$Par$beta$delta$est
      } else {
        beta <- m$CIbeta$beta$est
        delta <- m$CIbeta$delta$est
      }
      m$conditions$workBounds <- getWorkBounds(workBounds,distnames,m$mod$estimate,parindex,parCount,m$conditions$DM,beta,delta)
    }
    if(is.null(m$conditions$betaCons)){
      if(is.miSum(m) & !is.null(m$Par$beta$beta)) m$conditions$betaCons <- matrix(1:length(m$Par$beta$beta$est),nrow(m$Par$beta$beta$est),ncol(m$Par$beta$beta$est))
      else if(is.momentuHMM(m) & !is.null(m$mle$beta)) m$conditions$betaCons <- matrix(1:length(m$mle$beta),nrow(m$mle$beta),ncol(m$mle$beta))
    }
    if(is.null(m$conditions$betaRef)) m$conditions$betaRef <- as.integer(1:length(m$stateNames))
    if(is.momentuHMM(m)){
      if(is.null(m$mod$wpar)) m$mod$wpar <- m$mod$estimate
      if(is.null(m$mod$Sigma) & !is.null(m$mod$hessian)) m$mod$Sigma <- MASS::ginv(m$mod$hessian)
    } else {
      ####### compatability hack for change to MIcombine in momentuHMM >= 1.4.3 ######
      if(is.null(m$conditions$optInd)){
        for(i in names(m$conditions$dist)){
          m$conditions$cons[[i]]<-rep(1,length(m$conditions$cons[[i]]))
          m$conditions$workcons[[i]]<-rep(0,length(m$conditions$workcons[[i]]))
          m$conditions$workBounds[[i]]<-matrix(c(-Inf,Inf),nrow(m$conditions$workBounds[[i]]),2,byrow=TRUE)
        }
      }
      ################################################################################
    }
  } else if(!is.miHMM(m) & any(unlist(lapply(m,is.momentuHMM)))){
    m <- HMMfits(m)
  }
  m
}