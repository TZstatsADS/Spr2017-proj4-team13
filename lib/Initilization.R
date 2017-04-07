#############################
##Initilization fucntion
##############################
#Input: Clearned text data which contains the information for each publication
#Output: 1.The Coresponding 
library(text2vec)

AKumar<-read.csv("Testing_File.csv",header = T,as.is=T)

initialization<-function(authorname){
  
  
  authorname<-AKumar
  ##Get Author ID and find the k:
  AuthorID<-authorname$AuthorID
  k<-unique(AuthorID)
  
  
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
  
  
  ##Compare the number of the lamda and K:
  
  ##Get the globle center:
  
  ##
  
  
  
  return(cluser)
  
  
}



