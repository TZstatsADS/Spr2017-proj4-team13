#########################################################################
### This is the function to calculate the distance between two papers ###
### Input: two vectors, parameter matrix A                            ###
#########################################################################

distance <- function(x1,x2,A){
  Anorm <- function(x1,x2,A){
    return(t(x1)%*%A%*%x2)
  }
  numer <- t(x1)%*%A%*%x2
  denom <- sqrt(Anorm(x1,x1,A))*sqrt(Anorm(x2,x2,A))
  D <- 1-numer/denom
  return(D)
}
