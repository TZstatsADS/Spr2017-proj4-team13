#########################################################################
### This is the function to calculate the distance between two papers ###
### Input: two vectors, parameter matrix A                            ###
#########################################################################

distance <- function(x1,x2,A){
  numer <- t(x1)%*%A%*%x2
  denom <- Anorm(x1,A)*Anorm(x2,A)
  D <- 1-numer/denom
  return(D)
}