#library(mlbench)
#set.seed(111)
#obj = mlbench.spirals(100,1,0.025)
#my.dat =  4 * obj$x


#This is a function that performs the spectral cluster algorithm using QR decomposition to assign clusters
#Package needed:  expm
#Input: 
#my.dat: a m by n citation data matrix
#n.cluster: numerical value indeicates the number of clusters indicated
#output:
#cluster: A numerical vector that contains cluster results for each observation

library(expm)

Spectral.Cluster = function(my.dat, n.cluster=2) {
  
  #A m # of features by n # of observations
  
  N = ncol(my.dat)#Measure number of observations
  
  #Construct Gram matrix
  Gram = t(my.dat) %*% my.dat
  
  #for(i in 1 : Nrow){
   # for(j in 1 : Nrow){
    #  if(i != j){
     #   Affinity[i,j] = exp(-(1/2*(sigma^2)) * norm(as.matrix(my.dat[i,]-my.dat[j,]), type="F")^2)}
    #}
  #}  
  
  #Construct degree matrix Degree   
  #Degree = diag(apply(Affinity,1,sum))
  
  #Construct normalizaed graph Laplacian
  #Lap = (Degree %^% (-1/2)) %*% Affinity %*% (Degree %^% (-1/2))
  
  #Obtain eigenvctors for the gram matrix
  ev = eigen(Gram, symmetric = TRUE)
  
  #Form matrix X by taking the n.cluster largest eigenvectors from Lap
  X = ev$vectors[,1:n.cluster]
  
  #Renormalizaing each X's rows
 # for(i in 1:dim(X)[1])
  #{
   # normalize = sqrt(sum(X[i,]^2))
    #for(j in 1 : dim(X)[2])
    #{
     # X[i,j] = X[i,j]/normalize
    #}
  #}
  
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
#cluster = Spectral.Cluster(my.dat)
#A = Spectral.Cluster(my.dat,n.line=3,n.cluster=2,normal = F) 
#B = Spectral.Cluster(my.dat,n.line=3,n.cluster=2,normal = T) 

#plot = data.frame(x=my.dat[,1],y=my.dat[,2],class = cluster)

#par(mfrow=c(1,2))
#plot(my.dat,main="Original")
#plot(x = plot$x, y = plot$y, col=plot$class,main="Spectral Cluster with QR")



#library(stats)
#K = kmeans(my.dat, centers=2,nstart = 5)
#par(mfrow=c(1,2))
#plot(x = plot$x, y = plot$y, col=plot$class,main="Spectral Cluster with QR")
#plot(my.dat, col=K$cluster,main="K means")
