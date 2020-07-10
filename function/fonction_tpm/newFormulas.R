newFormulas<-function(formula,nbStates)
{
  stateForms<- terms(formula, specials = paste0(rep(c("state","toState"),each=nbStates),1:nbStates))
  newformula<-formula
  formulaStates <- vector('list',nbStates*(nbStates-1))
  formulaStates[1:(nbStates*(nbStates-1))] <- list(newformula)
  formterms<-attr(terms.formula(newformula),"term.labels")
  
  if(nbStates>1){
    if(length(unlist(attr(stateForms,"specials")))){
      newForm<-attr(stateForms,"term.labels")[-unlist(attr(stateForms,"specials"))]
      for(i in 1:nbStates){
        if(!is.null(attr(stateForms,"specials")[[paste0("state",i)]])){
          for(j in 1:(nbStates-1)){
            newForm<-c(newForm,gsub(paste0("state",i),paste0("betaCol",(i-1)*(nbStates-1)+j),attr(stateForms,"term.labels")[attr(stateForms,"specials")[[paste0("state",i)]]]))
          }
        }
        if(!is.null(attr(stateForms,"specials")[[paste0("toState",i)]])){
          betaInd<-matrix(0,nbStates,nbStates,byrow=TRUE)
          diag(betaInd)<-NA
          betaInd[!is.na(betaInd)] <- seq(1:(nbStates*(nbStates-1)))
          betaInd<-t(betaInd)[,i]
          betaInd<-betaInd[!is.na(betaInd)]
          for(j in betaInd){
            newForm<-c(newForm,gsub(paste0("toState",i),paste0("betaCol",j),attr(stateForms,"term.labels")[attr(stateForms,"specials")[[paste0("toState",i)]]]))
          }
        }
      }
      newformula<-as.formula(paste("~",paste(newForm,collapse="+")))
    }
    formulaStates<-stateFormulas(newformula,nbStates*(nbStates-1),spec="betaCol")
    if(length(unlist(attr(terms(newformula, specials = c(paste0("betaCol",1:(nbStates*(nbStates-1))),"cosinor")),"specials")))){
      allTerms<-unlist(lapply(formulaStates,function(x) attr(terms(x),"term.labels")))
      newformula<-as.formula(paste("~",paste(allTerms,collapse="+")))
      formterms<-attr(terms.formula(newformula),"term.labels")
    } else {
      formterms<-attr(terms.formula(newformula),"term.labels")
      newformula<-formula
    }
  }  
  return(list(formulaStates=formulaStates,formterms=formterms,newformula=newformula))
}