######################################################################
##This is the function of C2 constraint                             ##
##The function is used to determine whether two papers have at least##
##one secondary author with the same name                           ##
######################################################################

#The inputs are : two paper IDs, the file name of the target author
constraint <- function(paper1,paper2,authorname){
  #corresponding info of two papers
  pp1 <- authorname[authorname$PaperID==paper1,]
  pp2 <- authorname[authorname$PaperID==paper2,]
  
  #seperate the coauthors
  coauthor1 <- unlist((strsplit(pp1$Coauthor,split = ";")))
  coauthor2 <- unlist((strsplit(pp2$Coauthor,split = ";")))
  #calculate the number of secondary coauthors
  num.coauthor <- sum(coauthor1%in%coauthor2)
  #return binary value
  if(num.coauthor==0){return(0)}
  else{return(1)}
}