fcen <- function(X,cluster,A){
  if(is.null(dim(X))){
    X<-t(as.matrix(X))
  }
  
  K <- length(unique(cluster))
  cluster<-as.numeric(factor(cluster))
  
  centroids <- matrix(NA,nrow=K,ncol = ncol(X))
  
  for (i in 1:K){ 
    index<-cluster ==i
    if (sum(index)>1){
      Xsub <- X[cluster ==i,]
      Xsum<-colSums(Xsub)
      centroids[i,] <- Xsum/sqrt(Anorm(Xsum,Xsum,A))
    } else {
      Xsub <- X[cluster ==i,]
      centroids[i,] <- Xsub/sqrt(Anorm(Xsub,Xsub,A))
    }
  }
    
  return(centroids)
}
