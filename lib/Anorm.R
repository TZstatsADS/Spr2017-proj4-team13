#####################################################################
### This is the function of calculating norm value under matrix A ###
### Input: vector, parameter matrix                               ###
#####################################################################

Anorm <- function(x,A){
   return(sqrt(t(x)%*%A%*%x))
 }