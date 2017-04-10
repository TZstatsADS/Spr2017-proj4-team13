#This is a function that performs the spectral cluster algorithm using QR decomposition to assign clusters assignments
#Package needed:  None

#Input: 
#my.dat: a n by m citation data matrix
#n.cluster: numerical value indeicates the number of clusters indicated

#output:
#cluster: A numerical vector that contains cluster results for each observation


Spectral.Cluster = function(my.dat, n.cluster=2) {
  
  #A m # of features by n # of observations
  
  my.dat = t(my.dat)
  
  N = ncol(my.dat)#Measure number of observations
  
  #Construct Gram matrix
  Gram = t(my.dat) %*% my.dat
  
  #Obtain eigenvctors for the gram matrix
  ev = eigen(Gram, symmetric = TRUE)
  
  #Form matrix X by taking the n.cluster largest eigenvectors from Lap
  X = ev$vectors[,1:n.cluster]
  
  #Construct the transpose matrix of X
  Y = t(X)
  
  #Perform QR decomposition of matrix Y
  Y.decom = qr(Y,LAPACK = TRUE)
  
  #Obtain R matrix Y.R
  Y.R = qr.R(Y.decom)
  
  #Obtain the permutaion matrix Y.P
  column.order = Y.decom$pivot
  I = diag(nrow = N)
  Y.P = I[,column.order]
  
  #Obtain our R_hat matrix specified in the paper
  R_hat = Y.R %*% t(Y.P)
  
  #Make every entry in R_hat becomes an absolute value
  R_hat = apply(R_hat,2,abs)
  
  #Assgin clusters
  cluster = apply(R_hat,2,which.max)
  
  return(cluster)  
}

