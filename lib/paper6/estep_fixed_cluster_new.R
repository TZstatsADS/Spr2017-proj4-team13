# cluster(label,n*1 dim), X(n*p dim), centroids(k*p dim), A(p*p dim)
# Suppose we have obj function: fobj(X,cluster,centroids,A)???returns a value of the objective function
# Suppose we have center function: fcen(X,cluster,A),return a centroids matrix
# E-step
# update centroids and recalculate A

estep_fixed_clusters <- function(cluster,X,centroids,A,weight=1){
  #Number of Data
  n <- nrow(X)
  
  #Get Distance Matrix for all Xs
  Dis_Matrix<-matrix(NA,n,n)
  for (m in 1:n){
    Dis_Matrix[m,]<-apply(X,1,distance,x2=X[m,],A=A)
  }
  
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
      cluster[i]<-k
      
      ##Update 
      new_label_indicator<-(cluster==k)
      label_matrix[i,]<-new_label_indicator
      label_matrix[,i]<-new_label_indicator
      
      Dis_Use<-Dis_Matrix*weight*label_matrix*Constraint
      Vector[i]<-distance(X[i,],centroids[k,],A=A)
        
      obj.value[k]<-sum(Vector)+sum(Dis_Use)
      }
      ##Update cluster for i:
      cluster[i] <- which.min(obj.value)
      ##update center:
      centroids<-fcen(X,cluster,A)
      }
  return(cluster)
}
