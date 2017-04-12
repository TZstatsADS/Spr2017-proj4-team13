setwd("~/Desktop/5243 ADS/proj4/Spr2017-proj4-team13-master/doc")


library(stringr)

data.lib="../data/nameset"
data.files=list.files(path=data.lib, "*.txt")

data.files

## remove "*.txt"
query.list=substring(data.files, 
                     1, nchar(data.files)-4)

query.list

## add a space
query.list=paste(substring(query.list, 1, 1), 
                 " ", 
                 substring(query.list, 
                           2, nchar(query.list)),
                 sep=""
)

query.list


f.line.proc=function(lin, nam.query="."){
  
  # remove unwanted characters
  char_notallowed <- "\\@#$%^&?" # characters to be removed
  lin.str=str_replace(lin, char_notallowed, "")
  
  # get author id
  lin.str=strsplit(lin.str, "_")[[1]]
  author_id=as.numeric(lin.str[1])
  
  # get paper id
  lin.str=lin.str[2]
  paper_id=strsplit(lin.str, " ")[[1]][1]
  lin.str=substring(lin.str, nchar(paper_id)+1, nchar(lin.str))
  paper_id=as.numeric(paper_id)
  
  # get coauthor list
  lin.str=strsplit(lin.str, "<>")[[1]]
  coauthor_list=strsplit(lin.str[1], ";")[[1]]
  
  #print(lin.str)
  for(j in 1:length(coauthor_list)){
    if(nchar(coauthor_list[j])>0){
      nam = strsplit(coauthor_list[j], " ")[[1]]
      if(nchar(nam[1])>0){
        first.ini=substring(nam[1], 1, 1)
      }else{
        first.ini=substring(nam[2], 1, 1)
      }
    }
    last.name=nam[length(nam)]
    nam.str = paste(first.ini, last.name)
    coauthor_list[j]=nam.str
  }
  
  match_ind = charmatch(nam.query, coauthor_list, nomatch=-1)
  
  #print(nam.query)
  #print(coauthor_list)
  #print(match_ind)
  
  if(match_ind>0){
    
    coauthor_list=coauthor_list[-match_ind]
  }
  
  paper_title=lin.str[2]
  journal_name=lin.str[3]
  coauthor_list=str_replace(coauthor_list," ","")
  coauthor_list=paste(coauthor_list, collapse = ";")
  
  list(author_id, 
       paper_id, 
       coauthor_list, 
       paper_title, 
       journal_name)
}


data_list=list(1:length(data.files))

for(i in 1:length(data.files)){
  
  ## Step 0 scan in one line at a time.
  
  dat=as.list(readLines(paste(data.lib, data.files[i], sep="/")))
  data_list[[i]]=lapply(dat, f.line.proc, nam.query=query.list[i])
  
  
}


# 1
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[1]])){
  for (j in 1:5){
    df[i,j]<-data_list[[1]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/AGupta.csv",row.names = F)

# 2
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[2]])){
  for (j in 1:5){
    df[i,j]<-data_list[[2]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/AKumar.csv",row.names = F)
# 3
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[3]])){
  for (j in 1:5){
    df[i,j]<-data_list[[3]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/CChen.csv",row.names = F)
# 4
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[4]])){
  for (j in 1:5){
    df[i,j]<-data_list[[4]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/DJohnson.csv",row.names = F)
# 5
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[5]])){
  for (j in 1:5){
    df[i,j]<-data_list[[5]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/JLee.csv",row.names = F)
# 6
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[6]])){
  for (j in 1:5){
    df[i,j]<-data_list[[6]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/JMartin.csv",row.names = F)
# 7
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[7]])){
  for (j in 1:5){
    df[i,j]<-data_list[[7]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/JRobinson.csv",row.names = F)
# 8
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[8]])){
  for (j in 1:5){
    df[i,j]<-data_list[[8]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/JSmith.csv",row.names = F)
# 9
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[9]])){
  for (j in 1:5){
    df[i,j]<-data_list[[9]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/KTanaka.csv",row.names = F)
# 10
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[10]])){
  for (j in 1:5){
    df[i,j]<-data_list[[10]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/MBrown.csv",row.names = F)
# 11
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[11]])){
  for (j in 1:5){
    df[i,j]<-data_list[[11]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/MJones.csv",row.names = F)
# 12
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[12]])){
  for (j in 1:5){
    df[i,j]<-data_list[[12]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/MMiller.csv",row.names = F)
# 13
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[13]])){
  for (j in 1:5){
    df[i,j]<-data_list[[13]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/SLee.csv",row.names = F)
# 14
df <- data.frame(AuthorID=numeric(),
                 PaperID=numeric(),
                 Coauthor=character(),
                 Paper=character(), 
                 Journal=character(),
                 stringsAsFactors=FALSE) 
for (i in 1:length(data_list[[14]])){
  for (j in 1:5){
    df[i,j]<-data_list[[14]][[i]][[j]]
  }
}
#write.csv(df,file="../data/namecsv/YChen.csv",row.names = F)


