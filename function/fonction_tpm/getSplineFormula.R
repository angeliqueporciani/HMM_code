
getSplineFormula<-function(formula,data,covs){
  newcovs<-covs
  splineInd<-FALSE
  splineCovs<-character()
  newformula<-character()
  Terms<-terms(formula,specials=splineList)
  factors<-attr(Terms,"factors")
  specials<-rownames(factors)[unlist(attr(Terms,"specials"))]
  for(k in rownames(factors)){
    if(k %in% specials){
      splineInd<-TRUE
      splineCovs<-unique(c(splineCovs,all.vars(as.formula(paste0("~",k)))))
      splineExpr<-qdapRegex::rm_between(k, "(", ",", extract=TRUE)[[1]]
      sp<-eval(substitute(eval(parse(text=k))),data,parent.frame())
      tmpcovs<-predict(sp,eval(substitute(eval(parse(text=splineExpr))),covs,parent.frame()))
      tmp<-colnames(model.matrix(as.formula(paste0("~",k)),data)[,-1])
      tmp<-gsub("[()]","",tmp)
      tmp<-gsub(" ","_",tmp)
      tmp<-gsub(",","_",tmp)
      tmp<-gsub("=","_",tmp)
      colnames(tmpcovs)<-tmp
      newcovs<-cbind(newcovs,tmpcovs)
      for(l in colnames(factors)){
        if(factors[k,l]){
          tmp<-colnames(model.matrix(as.formula(paste0("~",l)),data))[-1]
          tmp<-gsub("[()]","",tmp)
          tmp<-gsub(" ","_",tmp)
          tmp<-gsub(",","_",tmp)
          tmp<-gsub("=","_",tmp)
          newformula<-c(newformula,tmp)
        }
      }
    } else {
      if(length(specials)) {
        tmpspec<-specials
        tmpspec<-gsub("(","\\(",tmpspec,fixed=TRUE)
        tmpspec<-gsub(")","\\)",tmpspec,fixed=TRUE)
        lfact <- grep(paste(tmpspec,collapse="|"),colnames(factors), value=TRUE,invert=TRUE)
      } else lfact <- colnames(factors)
      for(l in lfact){
        if(factors[k,l]){
          newformula<-c(newformula,l)
        }
      }
    }
  }
  if(!splineInd) newformula <- formula
  else newformula <- as.formula(paste0("~",paste0(unique(newformula),collapse="+")))
  
  return(list(formula=newformula,covs=newcovs))
}
