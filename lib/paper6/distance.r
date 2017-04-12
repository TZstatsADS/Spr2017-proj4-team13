#########################################################################
### This is the function to calculate the distance between two papers ###
### Input: two vectors, parameter matrix A                            ###
#########################################################################

distance <-function(x1,x2,A){
  numer <- Anorm(x1,x2,A)
  denom <- sqrt(Anorm(x1,x1,A)*Anorm(x2,x2,A))
  D <- 1-numer/denom
  return(D)
}


