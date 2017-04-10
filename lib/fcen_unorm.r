#fcen(X,cluster,A)
fcen_unorm <- function(X,cluster,A){
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
      centroids[i,] <- colSums(Xsub)
    } else {
      Xsub <- X[cluster ==i,]
      centroids[i,] <- Xsub
    }
  }
  return(centroids)
}


#fcen_unorm(X,cluster,A)
