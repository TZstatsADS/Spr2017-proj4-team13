#We need function fcen(X,cluster,A) and function partial_obj(matrix,authorname,label,m)
mstep <- function(cluster,X,A,ita,rawdata){
  centroids <- fcen(X,cluster,A)
  M <- nrow(A)
  for(m in 1:M){
    A[m,m] <- A[m,m]+ita*partial_obj(X,rawdata,cluster,A,m) 
  }
  return(centroids,A)
}