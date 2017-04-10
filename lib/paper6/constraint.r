######################################################################
##This is the function of C2 constraint                             ##
##The function is used to determine whether two papers have at least##
##one secondary author with the same name                           ##
######################################################################

#The inputs are : two paper IDs, the file name of the target author
constraint <- function(paper1,paper2,Split_coauthor){
  #corresponding info of two papers
  pp1 <- Split_coauthor[[paper1]]
  pp2 <- Split_coauthor[[paper2]]

  #calculate the number of secondary coauthors
  num.coauthor <- sum(pp1%in%pp2)
  #return binary value
  if(num.coauthor==0){return(0)}
  else{return(1)}
}



