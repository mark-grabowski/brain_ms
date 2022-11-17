make.slouch.simmap.table<-function(simmap.output,num.it){
                                   
                                   #,model.type,pred.type,num.sp,prev.output=NULL){
  #Code to create or update a table from slouch output
  #Format for calling function: make.slouch.table(Slouch output,type of model,type of predictor variable,
  #sample size,previous make.slouch output or leave blank)
  #Model types code:
  #OU.FF.DE - OU model with fixed factors and direct covariates
  #OU.AD.DE - OU model with random covariates and covariates 
  #OU.FF.AD.DE - OU model with fixed factors, random and direct covariates
  #OU.DE - OU model with only direct covariates
  #BM - BM model
  col.pars<-25
  header.output<-c("Model Type","Predictor","Parameters","Evol Slope Mean","Evol Slope Min","Evol Slope Max",
                   "Opt Slope Mean","Opt Slope Min","Opt Slope Max","t1/2 (myr) Mean","t1/2 (myr) Min","t1/2 (myr) Max","Vy Mean","Vy Min","Vy Max",
                   "R2 (%) Mean","R2 (%) Min","R2 (%) Max","logL Mean","logL Min","logL Max","AICc Mean","AICc Min","AICc Max","Num")
  
  row.num<-dim(simmap.output[[1]])
  total.num<-length(simmap.output)
  model.num<-total.num/num.it
  it<-0
  complete.models<-NULL
  for(j in 1:model.num){
    ind.model<-data.frame(matrix(NA,nrow=row.num,ncol=length(header.output)))
    names(ind.model)<-header.output
    slouch.rep<-as.data.frame(data.table::rbindlist(simmap.output[c((1+it):(it+num.it))]))
    print(paste("it",it,"num.it",num.it,"total.num",total.num,"model.num",model.num))
    print(slouch.rep[1,1])
    
    ind.model[1,c(1,2,25)]<-slouch.rep[1,c(1,2,20)]
    ind.model[1:row.num,3]<-slouch.rep$Parameters[1:row.num]
    
    for(i in 1:row.num){
      if(!is.null(slouch.rep$Parameters[i])){
      sub.para<-subset(slouch.rep,Parameters==slouch.rep$Parameters[i])  
      ind.model[i,4]<-mean(sub.para$`Evol Parameter Est.`,na.rm=TRUE)
      ind.model[i,5]<-min(sub.para$`Evol Parameter Est.`,na.rm=TRUE)
      ind.model[i,6]<-max(sub.para$`Evol Parameter Est.`,na.rm=TRUE)
      ind.model[i,7]<-mean(sub.para$`Optimal Parameter Est`,na.rm=TRUE)
      ind.model[i,8]<-min(sub.para$`Optimal Parameter Est`,na.rm=TRUE)
      ind.model[i,9]<-max(sub.para$`Optimal Parameter Est`,na.rm=TRUE)
      }
    }

    #return(slouch.rep)
    ind.model[1,10]<-mean(slouch.rep$`t1/2 (myr)`,na.rm=TRUE)
    ind.model[1,11]<-min(slouch.rep$`t1/2 (myr)`,na.rm=TRUE)
    ind.model[1,12]<-max(slouch.rep$`t1/2 (myr)`,na.rm=TRUE)
    ind.model[1,13]<-mean(slouch.rep$`Vy`,na.rm=TRUE)
    ind.model[1,14]<-min(slouch.rep$`Vy`,na.rm=TRUE)
    ind.model[1,15]<-max(slouch.rep$`Vy`,na.rm=TRUE)
    ind.model[1,16]<-mean(slouch.rep$`R2 (%)`,na.rm=TRUE)
    ind.model[1,17]<-min(slouch.rep$`R2 (%)`,na.rm=TRUE)
    ind.model[1,18]<-max(slouch.rep$`R2 (%)`,na.rm=TRUE)
    ind.model[1,19]<-mean(slouch.rep$`logL`,na.rm=TRUE)
    ind.model[1,20]<-min(slouch.rep$`logL`,na.rm=TRUE)
    ind.model[1,21]<-max(slouch.rep$`logL`,na.rm=TRUE)
    ind.model[1,22]<-mean(slouch.rep$`AICc`,na.rm=TRUE)
    ind.model[1,23]<-min(slouch.rep$`AICc`,na.rm=TRUE)
    ind.model[1,24]<-max(slouch.rep$`AICc`,na.rm=TRUE)
    complete.models<-rbind(complete.models,ind.model)
    it<-it+num.it  
    print(ind.model)
    
  }    
  return(complete.models)
}
