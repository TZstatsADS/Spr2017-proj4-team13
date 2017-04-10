split_coauthor<-function(rawdata){
  coauthor<-rawdata$Coauthor
  split_coauthor<-sapply(coauthor,strsplit,split=";")
  return(split_coauthor)
}