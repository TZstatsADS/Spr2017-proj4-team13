# cluster(label,n*1 dim), X(n*p dim), centroids(k*p dim), A(p*p dim)
# Suppose we have obj function: fobj(X,cluster,centroids,A)???returns a value of the objective function
# Suppose we have center function: fcen(X,cluster,A),return a centroids matrix
# E-step
# update centroids and recalculate A

estep_fixed_clusters <- function(cluster,X,centroids,A,weight=0.7){
  #Number of Data
  n <- nrow(X)
  K<-length(unique(cluster))
  cluster<-as.numeric(factor(cluster))
  
  #Get Distance Matrix for all Xs
  Dis_Matrix<-matrix(NA,n,n)
  for (m in 1:n){
    Dis_Matrix[m,]<-apply(X,1,distance,x2=X[m,],A=A)
  }
  
  #Get Distance Vector for all Xs with its center
  Vector<-c()
  for (m in 1:n){
    Vector[m] <- distance(X[m,],centroids[cluster[m],],A)
  }
  
  for(i in 1:n){
    obj.value<- c()
    if(sum(cluster==cluster[i])!=1){
      label_matrix<-matrix(NA,n,n)
      
      for (j in 1:n){
        label_matrix[j,]<-cluster==cluster[j]
      }
      
      for(k in 1:K){
        label1 <- cluster[i] 
        cluster[i] <- k
        centroids[label1,] <- fcen(X[cluster==label1,],cluster[cluster==label1],A)
        centroids[k,] <- fcen(X[cluster==k,],cluster[cluster==k],A)
        
        ##Update 
        new_label_indicator<-(cluster==k)
        label_matrix[i,]<-new_label_indicator
        label_matrix[,i]<-new_label_indicator
        
        Vector[i]<-distance(X[i,],centroids[k,],A)

        Dis_Use<-Dis_Matrix*weight*label_matrix*Constraint
        obj.value[k]<- sum(Dis_Use)+sum(Vector)
      }
      ##Update cluster for i:
      cluster[i] <- which.min(obj.value)
      
      ##update center:
      centroids<-fcen(X,cluster,A)
    }
  }
  return(cluster)
}


