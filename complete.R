
complete <- function(directory, id = 1:332) {
  require(dplyr)
  path <- paste(".",directory,paste(stringr::str_pad(id,width=3,pad='0'),".csv",sep=""),sep="/")
  mydata <- ldply(path,read.csv)
  # head(mydata)
  mydata$ID <- as.factor(mydata$ID)
  # is.factor(mydata$ID)
  mydata %>%
  mutate(comp=complete.cases(mydata)) %>%
  group_by(ID) %>%
  summarise("Nobs"=sum(comp))
  
  
  
}