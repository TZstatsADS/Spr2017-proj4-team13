library(mlbench)
set.seed(111)
obj = mlbench.spirals(100,1,0.025)
my.dat =  4 * obj$x


#This is a function that performs the spectral cluster algorithm
#Package needed: stats, expm
#Input: 
      #my.dat: data matrix
      #sigma: numerical values indicating bandwidth of RBF kernel
      #n.cluster: numerical value indeicates the number of clusters
#output:
      #km.ouput: A k-means object that contains clluster results

library(stats)
library(expm)

Spectral.Cluster = function(my.dat,sigma = 1, n.cluster=2) {
  
  Nrow = nrow(my.dat)
  
  #Construct Affinity matrix
  Affinity = matrix(rep(0,Nrow^2), ncol=Nrow)
  
  for(i in 1 : Nrow){
    for(j in 1 : Nrow){
      if(i != j){
      Affinity[i,j] = exp(-(1/2*(sigma^2)) * norm(as.matrix(my.dat[i,]-my.dat[j,]), type="F")^2)}
    }
  }  
  
  #COnstruct degree matrix Degree   
  Degree = diag(apply(Affinity,1,sum))
  
  #Construct normalizaed graph Laplacian
  Lap = (Degree %^% (-1/2)) %*% Affinity %*% (Degree %^% (-1/2))
  
  #My parts ended
  ##########################################################################################################
  ev = eigen(Lap, symmetric=TRUE)#find eigenvectors for the Laplacian
  
  
  
  #Final   = ev$vectors[,1:n.cluster]#Locate the n.cluster largest eigenvectors to construct final matrix
  Final   = ev$vectors[,(ncol(ev$vectors)-n.cluster+1):ncol(ev$vectors)]#Locate the n.cluster smallest eigenvectors to construct final matrix 
  
  #perform k-means algorithm to cluster the final matrix
  km.ouput = kmeans(Final, centers=n.cluster, nstart=5)
  
  return(km.ouput)  
}

A = Spectral.Cluster(my.dat,n.line=3,n.cluster=2,normal = F) 
B = Spectral.Cluster(my.dat,n.line=3,n.cluster=2,normal = T) 


par(mfrow=c(1,2))
plot(my.dat,main="Original")
plot(my.dat, col=A$cluster,main="Unnormalized Lap")

par(mfrow=c(1,2))
plot(my.dat,main="Original")
plot(my.dat, col=B$cluster,main="normalized Lap")

