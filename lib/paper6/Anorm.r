#####################################################################
### This is the function of calculating norm value under matrix A ###
### Input: vector, parameter matrix                               ###
#####################################################################

Anorm <- function(x1,x2,A){
   return(as.numeric(t(x1)%*%A%*%as.matrix(x2)))
}


