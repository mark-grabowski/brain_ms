make.slouch.table<-function(output,model.type,pred.type,num.sp,prev.output=NULL){
  #SB Revision 1 Verion
  #Code to create or update a table from slouch output
  #Format for calling function: make.slouch.table(Slouch output,type of model,type of predictor variable,
  #sample size,previous make.slouch output or leave blank)
  #Model types code:
  #OU.FF.DE - OU model with fixed factors and direct covariates
  #OU.AD.DE - OU model with random covariates and covariates 
  #OU.FF.AD.DE - OU model with fixed factors, random and direct covariates
  #OU.DE - OU model with only direct covariates
  #BM - BM model
  
  col.pars<-20
  if(is.null(output$beta_primary$coefficients_bias_corr)==FALSE){
    row.pars<-dim(output$beta_primary$coefficients_bias_corr)[1]
  }
  else{
    row.pars<-dim(output$beta_primary$coefficients)[1]
  }
  num.fact<-length(unique(output$fixed.fact))
  header.output<-c("Model Type","Predictor","Parameters","Evol Parameter Est.","EPE SE","Optimal Parameter Est","OPE SE","t1/2 (myr)","t1/2 min","t1/2 max",
                   "Vy","Vy (min)","Vy (max)","R2 (%)","logL","AICc","Pairwise Trends","Contrast","C SE","Num")
  
  if(model.type=="OU.FF.DE")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,8]<-output$evolpar$hl
    #print("OK")
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,9]<-output$supported_range[1,1]
      saved.output[1,10]<-output$supported_range[1,2]}
    saved.output[1,11]<-output$evolpar$vy
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,12]<-output$supported_range[2,1]
      saved.output[1,13]<-output$supported_range[2,2]}
        
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients_bias_corr)
    saved.output[1:row.pars,4]<-output$beta_primary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,5]<-output$beta_primary$coefficients_bias_corr[,2]
    
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),17]<-row.names(output$beta_primary$trend_diff)
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),18]<-output$beta_primary$trend_diff[,1]
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),19]<-output$beta_primary$trend_diff[,2]
    
    
    
  }
  if(model.type=="OU.FF.NDE")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,8]<-output$evolpar$hl
    #print("OK")
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,9]<-output$supported_range[1,1]
      saved.output[1,10]<-output$supported_range[1,2]}
    saved.output[1,11]<-output$evolpar$vy
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,12]<-output$supported_range[2,1]
      saved.output[1,13]<-output$supported_range[2,2]}
    
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients)
    saved.output[1:row.pars,4]<-output$beta_primary$coefficients[,1]
    saved.output[1:row.pars,5]<-output$beta_primary$coefficients[,2]
    
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),17]<-row.names(output$beta_primary$trend_diff)
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),18]<-output$beta_primary$trend_diff[,1]
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),19]<-output$beta_primary$trend_diff[,2]
    
    
  }
  
  
  
  
  if(model.type=="OU.AD.DE")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,8]<-output$evolpar$hl
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,9]<-output$supported_range[1,1]
      saved.output[1,10]<-output$supported_range[1,2]}
    saved.output[1,11]<-output$evolpar$vy
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,12]<-output$supported_range[2,1]
      saved.output[1,13]<-output$supported_range[2,2]}
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients_bias_corr)
    saved.output[1:row.pars,4]<-output$beta_evolutionary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,5]<-output$beta_evolutionary$coefficients_bias_corr[,2]
    
    saved.output[1:row.pars,6]<-output$beta_primary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,7]<-output$beta_primary$coefficients_bias_corr[,2]
    
  }
  if(model.type=="OU.FF.AD.DE")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,8]<-output$evolpar$hl
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,9]<-output$supported_range[1,1]
      saved.output[1,10]<-output$supported_range[1,2]}
    saved.output[1,11]<-output$evolpar$vy
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,12]<-output$supported_range[2,1]
      saved.output[1,13]<-output$supported_range[2,2]}
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients_bias_corr)
    saved.output[1:row.pars,4]<-output$beta_evolutionary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,5]<-output$beta_evolutionary$coefficients_bias_corr[,2]
    
    #saved.output[(num.fact+1):row.pars,6]<-output$beta_primary$coefficients_bias_corr[(num.fact+1):row.pars,1]
    #saved.output[(num.fact+1):row.pars,7]<-output$beta_primary$coefficients_bias_corr[(num.fact+1):row.pars,2]
    
    saved.output[1:row.pars,6]<-output$beta_primary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,7]<-output$beta_primary$coefficients_bias_corr[,2]
    
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),17]<-row.names(output$beta_primary$trend_diff)
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),18]<-output$beta_primary$trend_diff[,1]
    #saved.output[1:(length(row.names(output$beta_primary$trend_diff))),19]<-output$beta_primary$trend_diff[,2]
    
    
    
  }
  
  if(model.type=="OU.AD")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,8]<-output$evolpar$hl
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,9]<-output$supported_range[1,1]
      saved.output[1,10]<-output$supported_range[1,2]}
    saved.output[1,11]<-output$evolpar$vy
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,12]<-output$supported_range[2,1]
      saved.output[1,13]<-output$supported_range[2,2]}
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients_bias_corr)
    saved.output[1:row.pars,4]<-output$beta_evolutionary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,5]<-output$beta_evolutionary$coefficients_bias_corr[,2]
    
    saved.output[1:row.pars,6]<-output$beta_primary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,7]<-output$beta_primary$coefficients_bias_corr[,2]
    
  }
  
  if(model.type=="OU.DE")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,8]<-output$evolpar$hl
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,9]<-output$supported_range[1,1]
      saved.output[1,10]<-output$supported_range[1,2]}
    saved.output[1,11]<-output$evolpar$vy
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,12]<-output$supported_range[2,1]
      saved.output[1,13]<-output$supported_range[2,2]}
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients_bias_corr)
    
    saved.output[1:row.pars,4]<-output$beta_primary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,5]<-output$beta_primary$coefficients_bias_corr[,2]
    
  }
  
  if(model.type=="OU.NP")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,8]<-output$evolpar$hl
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,9]<-output$supported_range[1,1]
      saved.output[1,10]<-output$supported_range[1,2]}
    saved.output[1,11]<-output$evolpar$vy
    if(is.null(output$supported_range[1,1])==FALSE){
      saved.output[1,12]<-output$supported_range[2,1]
      saved.output[1,13]<-output$supported_range[2,2]}
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients)
    saved.output[1:row.pars,4]<-output$beta_primary$coefficients[,1]
    saved.output[1:row.pars,5]<-output$beta_primary$coefficients[,2]
    
  }
  
  if(model.type=="FF.BM")
  {
    
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    
    
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,11]<-output$evolpar$sigma2_y
    #if(is.null(output$supported_range[1,1])==FALSE){
    #  saved.output[1,12]<-output$supported_range[2,1]
    #  saved.output[1,13]<-output$supported_range[2,2]}
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients_bias_corr)
    saved.output[1:row.pars,4]<-output$beta_primary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,5]<-output$beta_primary$coefficients_bias_corr[,2]
  
    if(is.null(row.names(output$beta_primary$trend_diff))==FALSE){
      saved.output[1:(length(row.names(output$beta_primary$trend_diff))),17]<-row.names(output$beta_primary$trend_diff)
      saved.output[1:(length(row.names(output$beta_primary$trend_diff))),18]<-output$beta_primary$trend_diff[,1]
      saved.output[1:(length(row.names(output$beta_primary$trend_diff))),19]<-output$beta_primary$trend_diff[,2]
    }
    else{
      saved.output[1:(length(row.names(output$beta_primary$trend_diff))),18]<-output$beta_primary$trend_diff[1]
      saved.output[1:(length(row.names(output$beta_primary$trend_diff))),19]<-output$beta_primary$trend_diff[2]
    }
  
    
  }
  
  if(model.type=="FF.BM.NDE")
  {
    #saved.output<-data.frame(matrix(NA,length(row.names(output$beta_primary$trend_diff)),col.pars))
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,11]<-output$evolpar$sigma2_y
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients_bias_corr)
    saved.output[1:row.pars,4]<-output$beta_primary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,5]<-output$beta_primary$coefficients_bias_corr[,2]
  
    
    saved.output[1:(length(row.names(output$beta_primary$trend_diff))),17]<-output$beta_primary$trend_diff[,1]
    saved.output[1:(length(row.names(output$beta_primary$trend_diff))),18]<-row.names(output$beta_primary$trend_diff)
    saved.output[1:(length(row.names(output$beta_primary$trend_diff))),19]<-output$beta_primary$trend_diff[,2]
    
    
  }
  
  if(model.type=="BM")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,11]<-output$evolpar$sigma2_y
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients_bias_corr)
    saved.output[1:row.pars,4]<-output$beta_primary$coefficients_bias_corr[,1]
    saved.output[1:row.pars,5]<-output$beta_primary$coefficients_bias_corr[,2]
  
    
    
     
  }
  
  
  if(model.type=="BM.NP")
  {
    saved.output<-data.frame(matrix(NA,row.pars,col.pars))
    saved.output[1,1]<-model.type
    saved.output[1,2]<-pred.type
    saved.output[1,20]<-num.sp
    
    #print(saved.output)
    saved.output[1,11]<-output$evolpar$sigma2_y
    saved.output[1,14]<-output$modfit$`R squared`
    saved.output[1,15]<-output$modfit$Support
    saved.output[1,16]<-output$modfit$AICc
    
    saved.output[1:row.pars,3]<-row.names(output$beta_primary$coefficients)
    
    saved.output[1:row.pars,4]<-output$beta_primary$coefficients[,1]
    saved.output[1:row.pars,5]<-output$beta_primary$coefficients[,2]
    
  }
  
  
  
  
  names(saved.output)<-header.output
  saved.output<-rbind(prev.output,saved.output)
  return(saved.output)
}
