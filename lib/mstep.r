#We need function fcen(X,cluster,A) and function partial_obj(matrix,authorname,label,m)
mstep <- function(cluster,X,A,ita){
  n<-nrow(X)
  m<-nrow(A)
  
  label_matrix<-matrix(NA,n,n)
  
  for (i in 1:n){
    label_matrix[i,]<-cluster==cluster[i]
    }
  
  ##Obtained the indicator matrix to determine which xi and xj are culculated:
  indicator_matrix<- label_matrix*Constraint
 
  centroids <- fcen(X,cluster,A)
  centroids_unorm <- fcen_unorm(X,cluster,A)
  
  par2sum <- matrix(NA,n,m)
  par1<-matrix(NA,n,m)
  
  for(i in 1:(n-1)){
    par2<-matrix(NA,n,m)
    for(j in (i+1):nrow(matrix)){
      if(label_matrix[i,j]!=0){
        x1<-X[i,]
        x2<-X[j,]
        par2[j,]<-partial_D(x1,x2,A)
      }
    }
    
    par2sum[i,] <-colSums(par2)
    par1[i,]<-partial_D(x1,centroids_unorm[cluster[i],],A)
  }
  
  partial<-2*colSums(par2sum,na.rm = T)+colSums(par1,na.rm = T)
  A<-A+ita*diag(partial)
  
  return(list(centroids=centroids,A=A))
}
