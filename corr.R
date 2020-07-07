corr <- function(directory, threshold = 0) {
  # directory contains 332 csv files containing pollutant data
  # corr returns vector of correlations between nitrate and sulfate for datasets above a threshold of complete observations
  path <- paste(".",directory,paste(stringr::str_pad(1:332,width=3,pad='0'),".csv",sep=""),sep="/")
  mydata <-lapply(path,read.csv)
  comp <- complete.cases(data.matrix(unlist(mydata)))
  # comp1 <- comp[(sum(comp)>threshold)]
  comp <- sapply(mydata,function(x) {sum(complete.cases(x))})
  corel <- sapply(mydata,function(s) {cor(data.matrix(s[-1,]$nitrate),data.matrix(s[-1,]$sulfate),use="pairwise.complete.obs",method="pearson")})
  #for(s in mydata) {
   # comp[s$ID] <- sum(complete.cases(data.matrix(s)))
  #}
  #corel <- numeric()
  #for(s in mydata) {
   # corel[s$ID] <- cor(data.matrix(s[-1,]$nitrate),data.matrix(s[-1,]$sulfate),use="pairwise.complete.obs",method="pearson")
  #}
  corel[(comp>threshold)]

}