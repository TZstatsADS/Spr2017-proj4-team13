# cluster(label,n*1 dim), X(n*p dim), centroids(k*p dim), A(p*p dim)
# Suppose we have obj function: fobj(X,cluster,centroids,A)???returns a value of the objective function
# Suppose we have center function: fcen(X,cluster,A),return a centroids matrix
# E-step
# update centroids and recalculate A

estep <- function(cluster,X,centroids,A){
  #number of data
  n <- nrow(X)
  #number of clusters
  K <- length(unique(cluster))
  #centroids <- fcen(X,cluster,A)
  for(i in 1:n){
    obj.value<- c()
    for(k in 1:K){
      label1 <- cluster[i] 
      cluster[i] <- k
      centroids[label1,] <- fcen(X[cluster==label1,],cluster[cluster==label1],A)
      centroids[k,] <- fcen(X[cluster==k,],cluster[cluster==k],A)
      #centroids <- fcen(X,cluster,A)
      
      label_matrix<-matrix(NA,n,n)
      for (j in 1:n){
        label_matrix[j,]<-cluster==cluster[j]
      }
      indicator_matrix<- label_matrix*Constraint
      
      
      obj.value [k]<- fobj(X,cluster,centroids,A,indicator_matrix)
    }
    cluster[i] <- which.min(obj.value)
  }
  return(cluster)
}
