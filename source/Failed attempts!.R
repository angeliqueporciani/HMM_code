##########################
###Ecriture des modèles###
##########################

library(momentuHMM)
library(forcats)
library(foreach)
library(doParallel)
library(doRNG)

data <- readRDS("./data/Amadou/Insemination-expt/R02/data_momentuHMM.rds") # this is on Carlo's HD
data$day <- fct_relevel(data$day, "28", "29", "30", "31", "01", "02", "03")

#distri initiale 
Par0 <-c(0.0001, 3, 50, 80) ##peut être à revoir sur la base d'une distri globale 

##on peut fixer des transitions impossible si jamais 
##pas obligé de creer des pseudo-matrice si on fixe pas les paramètres et les bounds, on peut utiliser la forme "formula". 
##par contre il faut récuperer les valeurs des paramètres des modèles individuels pour fitter le modèle complet. Soit on fait 1 par 1 pour les 200 individus ... 
##on peut essayer sans dans un premier temps, mais ca semble risky. 
##faut essayer de faire comme l'exemple des HarbourSeal de momentuhmm. 
nbStates <- 4
N <- nlevels(data$ID) # number of individuals

##list d'individus
data.ind <- vector("list", N)
for (i in 1:N) {
  data.ind[[i]] <- subset(data, ID == i)
}

##paramètres initiaux pour modèle null
ncores = 12
tmpDM<-list(AC=list(lambda=~day))
tmpformula <- ~ cosinor(hour, period = 24)
tmpPar2 <- getPar0(model=modA, formula=tmpformula, DM=tmpDM)

#boucle recuperation des paramètres du modèle initial 
registerDoParallel(cores=ncores) #faut mettre ncores quand yen a plusieurs
tmpPar2 <- list()
  tmpPar2<- foreach (i = 1:length(data.ind)) %dorng%
    {
  trymodA<-tryCatch(fitHMM(data = data.ind[[i]], 
                           nbStates = 4, 
                           dist= list(AC="pois"), 
                           Par0 = list(AC=Par0)), 
                    error=function(e) e)
  if(inherits(trymodA,"error")){
    tmpPar2[[i]] <- "error"
  } else 
    tmpPar2[[i]] <-getPar0(model=trymodA, formula=tmpformula, DM=tmpDM)
    }
  stopImplicitCluster()

  #ca marche
  
####FIT des modèles complets pour chaque individus en récupérant les paramètres initiaux des modeles A#####
  
  registerDoParallel(cores=ncores) #faut mettre ncores quand yen a plusieurs
  bestFit.all <- vector("list", length(data.ind))

  bestFit.all<-foreach(i=1:length(data.ind)) %dorng% 
  {
    tryFits <- tryCatch(fitHMM(data.ind[[i]],nbStates=nbStates,dist=list(AC="pois"),
                               Par0=tmpPar2[[i]]$Par,
                               beta0=tmpPar2[[i]]$beta,
                               delta0=tmpPar2[[i]]$delta,
                               DM=tmpDM, 
                               formula=tmpformula),
                        error=function(e) e)
    if(inherits(tryFits,"error"))
      {
      bestFit.all[[i]] <- "error"
    } else 
      bestFit.all[[i]] <-tryFits
  }
  stopImplicitCluster()

    ##Fonctionne !!! \o/ 
  
##########################################################
  
###########################################################
########Essai avec update : fonctionne pas car ce n'est pas une formula qui est utilisé pour fitHMM
  #fit du modèle validé 1ere fois avant de lancer la boucle (initialisation des paramètres)
mod <- fitHMM(data = data.ind[[1]],
              nbStates = nbStates,
              dist = list(AC="pois"),
              Par0 = tmpPar2[[1]]$Par,
              beta0 = tmpPar2[[1]]$beta,
              delta0 = tmpPar2[[1]]$delta,
              DM = tmpDM,
              formula = tmpformula)
class(mod)
##boucle du update pour le modèle complet du dessus 

bestFit.all <- list()
##ca ne fonctionne pas tel quel ... 
mod <-as.formula(data.ind[[1]], nbStates = nbStates, dist = list(AC="pois"),Par0 = tmpPar2[[1]]$Par,
          beta0 = tmpPar2[[1]]$beta,
          delta0 = tmpPar2[[1]]$delta,
          DM = tmpDM,
          formula = tmpformula)

  for(i in 2:N) {
    mod2=try(update(mod,
                    data = data.ind[[i]],
                    Par0 = tmpPar2[[i]]$Par,
                    beta0 = tmpPar2[[i]]$beta,
                    delta0 = tmpPar2[[i]]$delta
                    ),
             TRUE)
    if(isTRUE(class(mod2)=="try-error")) { next } else { bestFit.all[[i]] = mod2 }
    }

##ca fonctionne pas car update ne fonctionne que si ya une formula  

# la boucle n'abouti pas à cause de cette erreur :
# Error in { : task 8 failed - "non-finite value supplied by 'nlm'"

stopImplicitCluster()
########################################### END

if(any(tmpPar2$delta==1)){
  del0 <- rep(1.e-100,nbStates)
  del0[which(tmpPar2$delta==1)] <- 1-1.e-100*(nbStates-1)
} else del0 <- tmpPar2$delta

mod2 <- try(update(mod, data = data.ind[[i]]), TRUE)
if(isTRUE(class(mod2) == "try-error")) { next } else { bestFit.all[[i]] = mod2 }

foo <- tryCatch(update(mod, data = subset(data,ID==300)), error = function(e) e)
if(inherits(foo, "error")) { next } else {bestFit.all[[i]] <- "good"}

