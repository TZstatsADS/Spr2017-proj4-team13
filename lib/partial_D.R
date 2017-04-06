#####################################################################################################
#### This is the function of calculting the partial derivative of distance function              ####
#### Input: two vectors parameter matrix, index of column to calculate the partial derivative    ####
#####################################################################################################

partial_D <- function(x1,x2,A,m){
  numer <- x1[m]*x2[m]*Anorm(x1,A)*Anorm(x2,A)-t(x1)%*%A%*%x2*((x1[m])^2*(Anorm(x1,A))^2+(x2[m])^2*(Anorm(x2,A))^2)/(2*Anorm(x1,A)*Anorm(x2,A))
  denom <- (Anorm(x1,A))^2*(Anorm(x2,A))^2
  return(numer/denom)
}