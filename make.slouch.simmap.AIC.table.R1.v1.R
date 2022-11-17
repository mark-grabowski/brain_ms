make.slouch.simmap.AIC.table<-function(simmap.output,num.it){
  
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
  col.pars<-18
  header.output<-c("Model Type","Predictor","t1/2 (myr) Mean","t1/2 (myr) Min","t1/2 (myr) Max","Vy Mean","Vy Min","Vy Max",
                   "R2 (%) Mean","R2 (%) Min","R2 (%) Max","logL Mean","logL Min","logL Max","AICc Mean","AICc Min","AICc Max","Num")
  
  row.num<-dim(simmap.output[[1]])
  total.num<-length(simmap.output)
  model.num<-total.num/num.it
  it<-0
  complete.models<-NULL
  for(j in 1:model.num){
    ind.model<-data.frame(matrix(NA,nrow=1,ncol=length(header.output)))
    names(ind.model)<-header.output
    slouch.rep<-as.data.frame(data.table::rbindlist(simmap.output[c((1+it):(it+num.it))]))
    print(paste("it",it,"num.it",num.it,"total.num",total.num,"model.num",model.num))
    print(slouch.rep[1,1])
    
    ind.model[1,c(1,2,18)]<-slouch.rep[1,c(1,2,20)]

    #return(slouch.rep)
    ind.model[1,3]<-mean(slouch.rep$`t1/2 (myr)`,na.rm=TRUE)
    ind.model[1,4]<-min(slouch.rep$`t1/2 (myr)`,na.rm=TRUE)
    ind.model[1,5]<-max(slouch.rep$`t1/2 (myr)`,na.rm=TRUE)
    ind.model[1,6]<-mean(slouch.rep$`Vy`,na.rm=TRUE)
    ind.model[1,7]<-min(slouch.rep$`Vy`,na.rm=TRUE)
    ind.model[1,8]<-max(slouch.rep$`Vy`,na.rm=TRUE)
    ind.model[1,9]<-mean(slouch.rep$`R2 (%)`,na.rm=TRUE)
    ind.model[1,10]<-min(slouch.rep$`R2 (%)`,na.rm=TRUE)
    ind.model[1,11]<-max(slouch.rep$`R2 (%)`,na.rm=TRUE)
    ind.model[1,12]<-mean(slouch.rep$`logL`,na.rm=TRUE)
    ind.model[1,13]<-min(slouch.rep$`logL`,na.rm=TRUE)
    ind.model[1,14]<-max(slouch.rep$`logL`,na.rm=TRUE)
    ind.model[1,15]<-mean(slouch.rep$`AICc`,na.rm=TRUE)
    ind.model[1,16]<-min(slouch.rep$`AICc`,na.rm=TRUE)
    ind.model[1,17]<-max(slouch.rep$`AICc`,na.rm=TRUE)
    complete.models<-rbind(complete.models,ind.model)
    it<-it+num.it  
    print(ind.model)
    
  }    
  return(complete.models)
}
