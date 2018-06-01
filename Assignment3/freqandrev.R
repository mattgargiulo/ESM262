freqandrev<-function(datatable1,datatable2){
  
  rownames=rownames(datatable1)
  tmpq1= rep("none", ncol(datatable1))
  
  for (i in 1:ncol(datatable1)) {
    b=which.max(datatable1[,i])
    tmpq1[i]=rownames[b]
  }
 
  
  combineddata<-cbind(datatable1,datatable2)
  
  Total_Revenue<-rep(0, times=ncol(combineddata))
  for( i in 1:ncol(combineddata)-1){
    for(j in 1:nrow(combineddata)-1){
      
      Total_Revenue[i]=sum(combineddata[,i]*combineddata[, ncol(combineddata)])
      }
    
  }

  q2<-rbind(combineddata, Total_Revenue)

    q3<-sum(Total_Revenue)

    
  return(list(frequency=tmpq1, `Revenue By Location`= q2, `Total Revenue`= q3))
}




