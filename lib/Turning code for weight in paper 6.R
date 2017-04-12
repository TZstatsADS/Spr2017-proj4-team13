##Truning for weight:
weight<-seq(0,0.5,0.05)
pre<-c()
recal<-c()
F1<-c()
accur<-c()
for(h in 1:length(weight)){
  cluster<-answer$cluster
  cluster2<-cluster
  A<-answer$A
  m=0
  
  while(any(cluster!=cluster2)|(m==0)){
    cluster<-cluster2
    M_step<-mstep(cluster=cluster,X=X,A=A,ita=0.01)
    A<-M_step$A
    centroids<-M_step$centroids
    m=m+1
    
    cluster2<-estep_fixed_clusters(cluster=cluster,X=X,centroids=centroids,A=A,weight=weight[h])
    cluster2<-as.numeric(factor(cluster2))
  }
  
  Trueid <- True_Author
  Testid <- cluster2
  n <- length(Trueid)
  True_matrix <- matrix(NA,n,n)
  for(i in 1:n){
    True_matrix[i,] <- as.numeric(sapply(Trueid,"==",Trueid[i]))
  }
  
  Test_matrix <- matrix(NA,n,n)
  
  for(j in 1:n){
    Test_matrix[j,] <- sapply(Testid,"==",Testid[j])*3+2
    
  }
  match_matrix <- True_matrix+Test_matrix
  
  #true mis, test mis
  mis.mis <- sum(match_matrix==2)/2
  #true match, test mis
  mat.mis <- sum(match_matrix==3)/2
  #true mis, test match
  mis.mat <- sum(match_matrix==5)/2
  #true match,test mismatch
  mat.mat <- (sum(match_matrix==6)-n)/2
  
  pre[h]<-mat.mat/(mat.mat+mat.mis)
  recal[h]<-mat.mat/(mat.mat+mis.mat)
}
F1<-2*pre*recal/(pre+recal)
##generate plot:
plot(weight,F1,type="b",col=red,main = "F1 score for different weight",xlab="weight",ylab = "F1 score")
k
