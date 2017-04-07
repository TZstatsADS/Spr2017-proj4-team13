#####################################################################################################
#### This is the function of calculting the partial derivative of distance function              ####
#### Input: two vectors parameter matrix, index of column to calculate the partial derivative    ####
#####################################################################################################

partial_D <- function(x1,x2,A,m){
  Anorm <- function(x1,x2,A){
    return(t(x1)%*%A%*%x2)
  }
  numer <- x1[m]*x2[m]*sqrt(Anorm(x1,x1,A)*Anorm(x2,x2,A))-Anorm(x1,x2,A)*((x1[m])^2*Anorm(x1,x1,A)+(x2[m])^2*Anorm(x2,x2,A))/(2*sqrt(Anorm(x1,x1,A)*Anorm(x2,x2,A)))
  denom <- Anorm(x1,x1,A)*Anorm(x2,x2,A)
  return(numer/denom)
}
