#fobj(X,cluster,centroids,A)
fobj <- function(X,cluster,centroids,A,rawdata){
  n <- nrow(X)
  Matrix <- matrix(NA,ncol = n,nrow=n)
  Vector <- c()
  for(i in 1:(n-1))
  {
    for(j in (i+1):n)
    {
      Matrix[i,j] <- distance(X[i,],X[j,],A)*(cluster[i]!=cluster[j])*0.7*constriant(rawdata,i,j)
    }
    Vector[i] <- distance(X[i,],centroids[cluster[i],],A)
  }
  obj <- sum(Matrix, na.rm = T)+sum(Vector)
  return(obj)
}