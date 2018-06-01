# which.max will give a row value
# need to find the row name of that value
#need to create a variable for rownames

mostfreqfish<-function(datatable){
  rownames=rownames(datatable)
  tmpq1= rep("none", ncol(datatable))
  
  for (i in 1:ncol(datatable)) {
    b=which.max(datatable[,i])
    tmpq1[i]=rownames[b]
  }
  return(tmpq1)
}