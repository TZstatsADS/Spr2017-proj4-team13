#fobj(X,cluster,centroids,A)
fobj <- function(X,cluster,centroids,A,indicator_matrix){
  n <- nrow(X)
  Matrix <- matrix(NA,ncol = n,nrow=n)
  Vector <- c()
  for(i in 1:(n-1))
  {
    for(j in (i+1):n)
    {
      
      if(indicator_matrix[i,j]!=0){
        Matrix[i,j] <- distance(X[i,],X[j,],A)*0.7
      }
      
    }
    Vector[i] <- distance(X[i,],centroids[cluster[i],],A)
  }
  obj <- 2*sum(Matrix, na.rm = T)+sum(Vector)
  return(obj)
}
