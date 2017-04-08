#fcen(X,cluster,A)
fcen <- function(X,cluster,A)
{
  K <- length(unique(custer))
  centroids <- matrix(NA,nrow=K,ncol = ncol(X))
  for ( i in 1:K)
  {
    Xsub <- X[cluster ==i,]
    centroids[i,] <- sum(Xsub)/sqrt(Anorm(Xsub,Xsub,A))
  }
  return(centroids)
}