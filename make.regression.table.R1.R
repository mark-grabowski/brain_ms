make_regression_table<-function(slouch.table){
  saved.table<-NULL
  table.names<-c("Model Type","Predictors","t1/2","t1/2 min","t1/2 max","Vy","Vy min","Vy max","R2","logL","AICc","N=")
  for(i in 1:dim(slouch.table)[1]){
    if(is.na(slouch.table[i,1])==FALSE){
      saved.table<-rbind(saved.table,slouch.table[i,c(1,2,8:16,20)])
      }
    }
  names(saved.table)<-table.names
  return(saved.table)
}