#library(mlbench)

#set.seed(111)
#obj = mlbench.spirals(100,1,0.025)
#my.dat =  4 * obj$x
#plot(my.dat)

library(stats)
#This is a function that performs the spectral cluster algorithm
#Package needed: stats
#Input: 
      #my.dat: data matrix
      #n.line: numerical values indicating how many lines to de drawn for each point
      #n.cluster: numerical value indeicates the number of clusters
#output:
      #km.ouput: A k-means object that contains clluster results

Spectral.Cluster = function(my.dat,n.line=2,n.cluster=2) {
  
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
  
  #COnstruct degree matrix Degree
  Degree = diag(apply(Affinity,1,sum))
  
  #Construct unnormalizaed graph Laplacian
  Lap = Degree - Affinity
  
  ev = eigen(Lap, symmetric=TRUE)#find eigenvectors for the Laplacian
  Final   = evL$vectors[,(ncol(ev$vectors)-n.cluster+1):ncol(ev$vectors)]#Locate the k smallest eigenvectors to construct final matrix 
  
  #perform k-means algorithm to cluster the final matrix
  km.ouput = kmeans(Final, centers=n.cluster, nstart=5)
  
  return(km.ouput)  
}

#A = make.affinity(my.dat,3,2) 


#plot(my.dat, col=A$cluster)
