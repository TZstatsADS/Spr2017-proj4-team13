######Preparetion,Data Loading and Preliminary Analysis######
folder.path="../data/namecsv/"

##Sourse all functions:
functions=list.files(path = "../lib/paper6",pattern = "*.[Rr]")

for(i in 1:length(functions)){
  source(paste("../lib/paper6/",functions[i],sep=""))
}

#Get all files and load them 
authores=list.files(path = folder.path, pattern = "*.csv")
authores<-substr(authores, start=1, stop=nchar(authores)-4)

rawdata<-as.list(1:length(authores))
names(rawdata)<-authores

for (i in authores){
   rawdata[[i]]<-read.csv(paste(folder.path,i,".csv",sep = ""),header = T,as.is=T)
}

################################################
##if you want to run the algorithm on individual csv files, 
##then you can access all the data from the X_all data list
############################################

##change the raw data to matrixes:
X_all<-lapply(rawdata,Create_X)

#######################
####Testing set ==AKumar
#In<-sample(1:244,20,replace = F)
########################
Akumar_rawdata<-rawdata[["AKumar"]]
#Akumar_rawdata<-Akumar_rawdata[1:20,]
Akumar_X<-X_all[["AKumar"]]
#Akumar_X<-Akumar_X[In,]
X<-Akumar_X
data<-Akumar_rawdata
#colnames(data)
True_Author<-data$AuthorID
Split_coauthor<-split_coauthor(data)
######################## ########


####Get Constrian Matrix:
n<-nrow(X)
Constraint<-matrix(NA,n,n)
for(i in 1:n){
  Constraint[i,]<-sapply(1:n,constraint,paper2=i,Split_coauthor)
}
#dim(Constraint)

##Initilization:
answer<-initialization(data,X)
unique(answer$cluster)
length(unique(answer$cluster))
length(answer$cluster)
dim(answer$A)

dim(answer$centroids)

##EM Steps:
cluster<-answer$cluster
cluster2<-cluster
A<-answer$A
m=0

a1<-Sys.time()
while(any(cluster!=cluster2)|(m==0)){
  cluster<-cluster2
  M_step<-mstep(cluster=cluster,X=X,A=A,ita=0.01)
  A<-M_step$A
  centroids<-M_step$centroids
  m=m+1
  
  cluster2<-estep_fixed_clusters(cluster=cluster,X=X,centroids=centroids,A=A)
  cluster2<-as.numeric(factor(cluster2))
  }

a2<-Sys.time()
a2-a1
##Conditions:

#A<-matrix(1:4,2,2)
#rowSums(A)
#A[1,]



#cluster2<-answer$cluster
##assign authors to each clusters:
C<-length(unique(cluster2))

n<-length(cluster2)
New_cluster<-c()

for(k in 1:C){
  author<-names(which.max(table(True_Author[cluster2==k])))
  author<-as.numeric(author)
  New_cluster[k]<-author
}

##Assigen the predicted authors to each publication:
Assinged_Author<-c()
for(k in 1:C){
  Assinged_Author[cluster2==k]=New_cluster[k]
}


##Testing Accuracy1:
mean(Assinged_Author!=True_Author)
#save(Assinged_Author,file = "../output/Assigen_Author_Akumar1:20")


##Testing Accuracy2:
Trueid <- True_Author
Testid <- answer$cluster
n <- length(Trueid)
True_matrix <- matrix(NA,n,n)
for(i in 1:n){
  True_matrix[i,] <- as.numeric(sapply(Trueid,"==",Trueid[i]))
}

Test_matrix <- matrix(NA,n,n)
time1 <- Sys.time()
for(j in 1:n){
  Test_matrix[j,] <- sapply(Testid,"==",Testid[j])*3+2
  
}
time2 <- Sys.time()
time2-time1
match_matrix <- True_matrix+Test_matrix

#true mis, test mis
mis.mis <- sum(match_matrix==2)/2
#true match, test mis
mat.mis <- sum(match_matrix==3)/2
#true mis, test match
mis.mat <- sum(match_matrix==5)/2
#true match,test mismatch
mat.mat <- (sum(match_matrix==6)-n)/2

mis.mis;mis.mat;mat.mis;mat.mat
sum(mis.mis,mis.mat,mat.mis,mat.mat)
choose(20,2)
pre<-mat.mat/(mat.mat+mat.mis)
recal<-mat.mat/(mat.mat+mis.mat)
F1<-2*pre*recal/(pre+recal)
accur<-(mis.mis+mat.mat)/sum(mis.mis,mis.mat,mat.mis,mat.mat)
