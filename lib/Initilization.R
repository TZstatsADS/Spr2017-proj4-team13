######################################
#######Initilization fucntion#########
######################################
#Input: Clearned text data which contains the information for each publication
#Output: 1.The Coresponding 


library(text2vec)

#AKumar<-read.csv("Testing_File.csv",header = T,as.is=T)
#source("../lib/Create_Matrix.R")

initialization<-function(rawdata){
  ##Get Author ID and find the k:
  AuthorID<-rawdata$AuthorID
  True_K<-length(unique(AuthorID))
  
  ##Create the Matrix
  X<-Create_X(rawdata)

  ##Random generate :
  p<-ncol(X)
  n<-nrow(X)
  A<-diag(runif(p,0,2))
  
  ##Split the Coauthor:
  split_CoAuthor<-strsplit(rawdata[,1],split = ";")
  
  ##Initilize the clusters:
  I<-1:nrow(rawdata)
  cluster<-rep(NA,n)  ##cluster assignment
  a<-c()
  k<-1
  
  while(length(setdiff(I,a))!=0){
    candi<-c()
    i=setdiff(I,a)[1]
    cluster[i]<-k
    
    for(j in setdiff(I,c(i,a))){
        nu_coauthor <- sum(split_CoAuthor[[i]]%in%split_CoAuthor[[j]])
        if (nu_coauthor!=0){
          cluster[j]<-k
          candi<-c(candi,j) 
        }
      }
      a<-c(a,i,candi)
      k<-k+1
    }
  
  ##Get the centroids:
  ##Compare the number of the lamda and K:
  lamda<-length(unique(cluster))
  centroids<-matrix(NA,lamda,p)
  
  for(i in 1:lamda){
    index2<-cluster==i
    x<-rbind(X[index2,])
    sum_x<-as.matrix(colSums(x))
    centroids[i,]<-t(sum_x)/sqrt(as.numeric((t(sum_x)%*%A%*%sum_x)))
  }
  
  ##if lamda> Total No. of authors,get the upper triangular matrix:
  if (lamda>True_K){
    dist<-matrix(NA,lamda,lamda)
    for(i in 1:(lamda-1)){
      for(j in (i+1):lamda){
        dist[i,j]<-sqrt(t(centroids[i,])%*%A%*%as.matrix(centroids[j,]))
      }}
    
    ##Get the other half 
    for(i in lamda:1){
      for(j in 1:i){
        dist[i,j]<-dist[j,i]
      }
    }
    
    ##Get the farest K centers and combine other centers:
    row_sum<-rowSums(dist,na.rm = T)
    index2<-order(row_sum,decreasing = T)[1:True_K]
    
    #This step is ensure that cluster i is cluster i
    for (k in index2){
      dist[k,k]<-(-1)
    }
    
    ##Combied clusters:  
    dist2<-dist[index2,]
    rownames(dist2)<-index2
    index3<-apply(dist2,2,which.min)
    cluster_candi<-as.numeric(rownames(dist2)[index3])

    ##Replace the orginal clusters for each observation:
    for (i in 1:n){
    cluster[i]<-cluster_candi[cluster[i]]
    }
    
    ##Relable the new clusters: new lables shound from 1: True_K
    cluster<-as.numeric(factor(cluster))

    ##Get the new cluster centroids:
    for(i in 1:True_K){
      index4<-cluster==i
      x<-rbind(X[index4,])
      sum_x<-as.matrix(colSums(x))
      centroids[i,]<-t(sum_x)/sqrt(as.numeric((t(sum_x)%*%A%*%sum_x)))
    }
  }
  
  ##if lamda< Total No. of authors
  if (lamda<True_K){
    ##Get the globle center: 
    sum_X<-as.matrix(colSums(X))
    glo_center<- t(sum_X)/sqrt(as.numeric((t(sum_X)%*%A%*%sum_X)))
    random<-matrix(runif((True_K-lamda)*p,0,0.5),(True_K-lamda),p)
    centroids_add<-t(apply(random,1,"+",glo_center))
    centroids<-rbind(centroids,centroids_add)
  }
 
  
  return(list(cluster=cluster,X=X,centroids=centroids))
  }


answer<-initialization(AKumar)


####aissng Author ID to the clusters and testing the accuracy:
dim(centroids)
length(cluster)
AuthorID
Author_Pre<-NULL

for(l in 1:True_K){
  inde<-cluster==l
  Author_Pre[inde]<-which.max(cluster[inde])
}

sum(Author_Pre==AuthorID)

