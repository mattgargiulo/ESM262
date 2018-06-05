#' freqandrev
#'
#' summaries fish catches for different locations that take as inputs
#' @author Matt Gargiulo
#' @param database1 a table that has the number caught for each fish species for each location.
#' @param database2 ia table that has prices for different fish
#' @return 1) Most caught fish at each location. 2) Revenue per location ($) 3) total revenue ($)

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
    plot<-barplot(g, xlab="locations",col="blue", ylab="Revenue ($)", main = title, names.arg = c("a","b","c","d","e"))


####
  answer=list(frequency=tmpq1, `Revenue By Location`= q2, `Total Revenue`= q3, plot(plot))
  return(answer)
}




