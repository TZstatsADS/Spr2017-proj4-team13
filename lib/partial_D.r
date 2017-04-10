#####################################################################################################
#### This is the function of calculating the partial derivative of distance function             ####
#### Input: two vectors, parameter matrix                                                      
#### Output: partial derivatives (vector of length nrow(X))
#####################################################################################################

partial_D <- function(x1,x2,A){
  x1_norm<-Anorm(x1,x1,A)  #Number
  x2_norm<-Anorm(x2,x2,A)  #Number
  x1x2_norm<-x1_norm*x2_norm  #Number
  x1x2_norm_sqrt<-sqrt(x1x2_norm)  #Number
  
  numer <- x1*x2*x1x2_norm_sqrt-Anorm(x1,x2,A)*(x2^2*x1_norm+x1^2*x2_norm)/(2*x1x2_norm_sqrt)
  partial<-numer/x1x2_norm
  return(partial)
}

#length(partial_D(X[1,],X[2,],A,1))
#x1<-X[1,]
#x2<-X[2,]


