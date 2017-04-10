######Preparetion,Data Loading and Preliminary Analysis######
folder.path="../data/namecsv/"

##Sourse all functions:
functions=list.files(path = "../lib/")
for(i in 1:length(functions)){
  source(paste("../lib/",functions[i],sep=""))
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

#######################Testing set ==AKumar
Akumar_rawdata<-rawdata[["AKumar"]]
Akumar_rawdata<-Akumar_rawdata[1:20,]
Akumar_X<-X_all[["AKumar"]]
Akumar_X<-Akumar_X[1:20,]
X<-Akumar_X
data<-Akumar_rawdata
colnames(data)
Split_coauthor<-split_coauthor(data)
################################


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

while(any(cluster!=cluster2)|(m==0)){
  cluster<-cluster2
  M_step<-mstep(cluster=cluster,X=X,A=A,ita=0.01)
  A<-M_step$A
  centroids<-M_step$centroids
  m=m+1
  
  cluster2<-estep(cluster=cluster,X=X,centroids=centroids,A=A)
  cluster2<-as.numeric(factor(cluster2))
  }


##Conditions:

A<-matrix(1:4,2,2)
rowSums(A)
A[1,]
