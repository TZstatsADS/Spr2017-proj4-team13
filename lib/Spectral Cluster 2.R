source('lib/TFIDF.R')
AKumar <- read.csv('data/namecsv/AKumar.csv',stringsAsFactors = F)
tf <- Create_Title(AKumar)



library(stats)
library(expm)

Spectral.Cluster = function(my.dat,sigma = 1, n.cluster=2) {
  
  Nrow = nrow(my.dat)
  
  #Construct Affinity matrix
  Affinity = matrix(rep(0,Nrow^2), ncol=Nrow)
  
  for(i in 1 : Nrow){
    for(j in 1 : Nrow){
      if(i != j){
        Affinity[i,j] = exp(-(1/(2*sigma^2)) * norm(as.matrix(my.dat[i,]-my.dat[j,]), type="F")^2)}
    }
  }  
  
  #Construct degree matrix Degree   
  Degree = diag(apply(Affinity,1,sum))
  
  #Construct normalizaed graph Laplacian
  Lap = (Degree %^% (-1/2)) %*% Affinity %*% (Degree %^% (-1/2))
  
  ev = eigen(Lap, symmetric = TRUE)#Calculate the eigenvectors of Lap
  
  #Form matrix X by staking the n.cluster largest eigenvectors from Lap
  X = ev$vectors[,1:n.cluster]
  
  #My parts ended
  ##########################################################################################################
  
  library(text2vec)
  #renormalizing each X rows
  Y <- normalize(X,norm = 'l2')
  
  
  #perform k-means algorithm to cluster the final matrix
  #km.ouput = kmeans(Y, centers=n.cluster, nstart=5)
  
  #EM algorithm
  library(EMCluster)
  
  em <- assign.class(Y,init.EM(Y,nclass = 2))
  
  
  return(em)  
}

A = Spectral.Cluster(my.dat,n.cluster=2) 
B = Spectral.Cluster(my.dat,n.cluster=2) 


par(mfrow=c(1,2))
plot(my.dat,main="Original")
plot(my.dat, col=A$class,main="Unnormalized Lap")

par(mfrow=c(1,2))
plot(my.dat,main="Original")
plot(my.dat, col=B$class,main="normalized Lap")






