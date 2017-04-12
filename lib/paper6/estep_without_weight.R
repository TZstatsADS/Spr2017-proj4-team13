estep_fixed_clusters2 <- function(cluster,X,centroids,A){
  #Number of Data
  n <- nrow(X)
  
  for(i in 1:n){
    K<-length(unique(cluster))
    cluster<-as.numeric(factor(cluster))
    
    #Get Distance Vector for all Xs with its center
    Vector<-c()
    for (m in 1:n){
      Vector[m] <- distance(X[m,],centroids[cluster[m],],A)
    }
    
    label_matrix<-matrix(NA,n,n)
    for (j in 1:n){
      label_matrix[j,]<-cluster==cluster[j]
    }
    
    ##Loop for all existing clusters
    obj.value<- c()
    for(k in 1:K){

      Vector[i]<-distance(X[i,],centroids[k,],A=A)
      
      obj.value[k]<-sum(Vector)
    }
    ##Update cluster for i:
    cluster[i] <- which.min(obj.value)
    ##update center:
    centroids<-fcen(X,cluster,A)
  }
  return(cluster)
}
