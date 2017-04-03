library(mlbench)
library(stats)

set.seed(111)
obj <- mlbench.spirals(100,1,0.025)
my.data <-  4 * obj$x
plot(my.data)


make.affinity <- function(my.data,n.neighboors=2,k=2) {
  
  Nrow <- nrow(my.data)
  
  Similar <- matrix(rep(NA,Nrow^2), ncol=Nrow)
  
  for(i in 1:Nrow) {
    for(j in 1:Nrow) {
      Similar[i,j] <- exp(- 1 * norm(as.matrix(my.data[i,]-my.data[j,]), type="F"))
    }
  }
  #S <- make.similarity(my.data)
  
  N <- length(Similar[,1])
  
  if (n.neighboors >= N) {  # fully connected
    Affinity <- Similar
  } else {
    Affinity <- matrix(rep(0,N^2), ncol=N)
    for(i in 1:N) { # for each line
      # only connect to those points with larger similarity 
      best.similarities <- sort(Similar[i,], decreasing=TRUE)[1:n.neighboors]
      for (s in best.similarities) {
        j <- which(Similar[i,] == s)
        Affinity[i,j] <- Similar[i,j]
        Affinity[j,i] <- Similar[i,j] # to make an undirected graph, ie, the matrix becomes symmetric
      }
    }
  }
  
  D = diag(apply(Affinity,1,sum))
  
  U = D - Affinity
  evL <- eigen(U, symmetric=TRUE)
  Z   <- evL$vectors[,(ncol(evL$vectors)-k+1):ncol(evL$vectors)]
  km <- kmeans(Z, centers=k, nstart=5)
  
  return(km)  
}

A <- make.affinity(my.data,3,2) 


plot(my.data, col=A$cluster)
