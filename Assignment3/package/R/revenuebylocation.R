revenuebylocation=function(datatable1,datatable2){
  combineddata<-cbind(datatable1,datatable2)
  
  Total_Revenue<-rep(0, times=ncol(combineddata))
  for( i in 1:ncol(combineddata)-1){
    for(j in 1:nrow(combineddata)-1){
      
      Total_Revenue[i]=sum(combineddata[,i]*combineddata[, ncol(combineddata)])}
    
  }
  q2<-rbind(combineddata, Total_Revenue)
  return(q2)
}