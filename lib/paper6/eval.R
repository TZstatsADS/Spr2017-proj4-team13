evalu<-function(Trueid=True_Author,Testid=cluster2){
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
  
  pre<-mat.mat/(mat.mat+mat.mis)
  recal<-mat.mat/(mat.mat+mis.mat)
  F1<-2*pre*recal/(pre+recal)
  accur<-(mis.mis+mat.mat)/sum(mis.mis,mis.mat,mat.mis,mat.mat)
  return(c(pre,recal,F1,accur))
}

