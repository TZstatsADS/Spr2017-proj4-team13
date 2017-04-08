######################################
#######Initilization fucntion#########
######################################
#Input: Clearned text data which contains the information for each publication
#Output: 1.The Coresponding 
library(text2vec)

AKumar<-read.csv("Testing_File.csv",header = T,as.is=T)
source("../lib/Create_Matrix.R")

initialization<-function(authorname){
  
  ##Get Author ID and find the k:
  AuthorID<-authorname$AuthorID
  True_K<-length(unique(AuthorID))
  
  ##Create the Matrix
  X<-Create_X(authorname)

  ##Random generate :
  p<-ncol(X)
  A<-diag(runif(p,0,2))
  
  ##Split the Coauthor:
  split_CoAuthor<-strsplit(authorname[,1],split = ";")
  
  
  ##Initilize the clusters:
  I<-1:nrow(authorname)
  cluster<-rep(NA,nrow(authorname))  ##cluster assignment
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
  
  ##if lamda> Total No. of authors
  if (lamda>True_K){
    #dist<-matrix(NA,lamda,lamda)
    #for(i in 1:(lamda-1)){
     # for(j in (i+1):lamda){
      #  dist[i,j]<-sqrt(t(centroids[i,])%*%A%*%as.matrix(centroids[j,]))
      #}}
    index2<-sample(lamda,True_K)
    centroids<-centroids[index2,]
  }
  
  ##if lamda< Total No. of authors
  if (lamda<True_K){
    ##Get the globle center: 
    sum_X<-as.matrix(colSums(X))
    glo_center<- centroids[i,]<-t(sum_X)/as.numeric((t(sum_X)%*%A%*%sum_X))
    random<-matrix(runif((True_K-lamda)*p,0,0.5),(Ture_K-lamda),p)
    centroids_add<-t(apply(random,1,"+",glo_center))
    centroids<-rbind(centroids,centroids_add)
  }
  
  return(list(cluser=cluster,X=X,centroids=centroids))
  }


answer<-initialization(AKumar)
