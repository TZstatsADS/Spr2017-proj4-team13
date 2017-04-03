library(mlbench)
set.seed(111)
obj = mlbench.spirals(100,1,0.025)
my.dat =  4 * obj$x


#This is a function that performs the spectral cluster algorithm
#Package needed: stats, expm
#Input: 
      #my.dat: data matrix
      #n.line: numerical values indicating how many lines to de drawn for each point
      #n.cluster: numerical value indeicates the number of clusters
      #A boolean variable indicating whether to calculate a normalized laplacian matrix
#output:
      #km.ouput: A k-means object that contains clluster results

library(stats)
library(expm)

Spectral.Cluster = function(my.dat,n.line=3,n.cluster=2,normal = F) {
  
  Nrow = nrow(my.dat)
  
  #Construct Matrix that contains similarity between each values
  Similar = matrix(rep(NA,Nrow^2), ncol=Nrow)
  
  for(i in 1:Nrow) {
    for(j in 1:Nrow) {
      Similar[i,j] = exp( -1 * norm(as.matrix(my.dat[i,]-my.dat[j,]), type="F"))
    }
  }
  
  N = length(Similar[,1])
  
  #Construct Affinity matrix
  Affinity = matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best = sort(Similar[i,], decreasing=TRUE)[1:n.line]
      for (b in best) {
        j = which(Similar[i,] == b)
        Affinity[i,j] = Similar[i,j]
        Affinity[j,i] = Similar[i,j]
      }
    }
  
  #COnstruct degree matrix Degree   #Not sure since this part
  Degree = diag(apply(Affinity,1,sum))
  
  #Construct unnormalizaed graph Laplacian
  Lap = Degree - Affinity
  
  if(normal){
  #Construct normalizaed graph Laplacian
  Lap = (Degree %^% (-1/2)) %*% Lap %*% (Degree %^% (-1/2))
  }
  
  ev = eigen(Lap, symmetric=TRUE)#find eigenvectors for the Laplacian
  Final   = ev$vectors[,(ncol(ev$vectors)-n.cluster+1):ncol(ev$vectors)]#Locate the n.cluster smallest eigenvectors to construct final matrix 
  
  #perform k-means algorithm to cluster the final matrix
  km.ouput = kmeans(Final, centers=n.cluster, nstart=5)
  
  return(km.ouput)  
}

A = Spectral.Cluster(my.dat,n.line=3,n.cluster=2,normal = F) 
B = Spectral.Cluster(my.dat,n.line=3,n.cluster=2,normal = T) 


#mrofw(c(1,2))
par(mfrow=c(1,2))
plot(my.dat,main="Original")
plot(my.dat, col=A$cluster,main="Unnormalized Lap")

par(mfrow=c(1,2))
plot(my.dat,main="Original")
plot(my.dat, col=B$cluster,main="normalized Lap")

