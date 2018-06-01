freqandrev<-function(datatable1,datatable2){
  
  rownames=rownames(datatable1)
  tmpq1= rep("none", ncol(datatable1))
  
  for (i in 1:ncol(datatable1)) {
    b=which.max(datatable1[,i])
    tmpq1[i]=rownames[b]
    # this function spits out highest caught per location
  }
 
  
  combineddata<-cbind(datatable1,datatable2)
  
  Total_Revenue<-rep(0, times=ncol(combineddata))
  for( i in 1:ncol(combineddata)-1){
    for(j in 1:nrow(combineddata)-1){
      
      Total_Revenue[i]=sum(combineddata[,i]*combineddata[, ncol(combineddata)])
      }
    
  }

  q2<-rbind(combineddata, Total_Revenue)
  # spits out revenue per location

    q3<-sum(Total_Revenue)
    # aggregates the revenue

#### need to work on     
    g=Total_Revenue[1:5]
    title= sprintf(" The total revenue is %d dollars", q3)
    plot(g, xlab="locations",col="blue", ylab="Revenue ($)", main = title)  
    return(graph)
####    
    
  return(list(frequency=tmpq1, `Revenue By Location`= q2, `Total Revenue`= q3))
  

      
}



