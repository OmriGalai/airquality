pollutantmean <- function(directory, pollutant, id = 1:332) {
  # "directory" has 332 .csv files with observations including date, sulfate levels and nitrate levels
  # numbered 001 through 332
  # This function takes the mean of the specified pollutant across non-NA observations for specified sensor(s)
  library(plyr)
  library(readr)
  path <- paste(".",directory,paste(stringr::str_pad(id,width=3,pad='0'),".csv",sep=""),sep="/")
  mydata <- ldply(path,read.csv)
  if(identical(pollutant,"nitrate"))
    summarize(.(mydata),Mean=mean(mydata$nitrate,na.rm=T))
  else if(identical(pollutant,"sulfate"))
    summarize(.(mydata),Mean=mean(mydata$sulfate,na.rm=T))
}