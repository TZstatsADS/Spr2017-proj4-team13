setwd("~/Desktop/5243 ADS/proj4/Spr2017-proj4-team13-master/data/nameset")

#### Generate each csv file
gatherinfo<-function(filename){
  name<-deparse(substitute(filename))
  filename<- read.csv(paste0(name,".txt"), header = F, sep = ">")
  colnames(filename) <- c("Coauthor", "Paper", "Journal")
  filename$AuthorID <- sub("_.*","",filename$Coauthor)
  filename$Coauthor <- gsub("<","",sub("^.*?\\s","", filename$Coauthor))
  filename$Coauthor <- gsub(" ","",filename$Coauthor)
  filename$Coauthor <- gsub(paste0(name,";"),"",filename$Coauthor)
  filename$Coauthor <- gsub(paste0(";",name),"",filename$Coauthor)
  filename$Coauthor <- gsub(name,"",filename$Coauthor)
  filename$Paper <- gsub("<","",filename$Paper)
  filename$PaperID <- rownames(filename)
  return(filename)
}

#Read in the text file and Ouptut the csv file
AGupta<-gatherinfo(AGupta)
write.csv(AGupta,"AGupta.csv", row.names = F)

AKumar<-gatherinfo(AKumar)
write.csv(AKumar,"AKumar.csv", row.names = F)

CChen<-gatherinfo(CChen)
write.csv(CChen,"CChen.csv", row.names = F)

DJohnson<-gatherinfo(DJohnson)
write.csv(DJohnson,"DJohnson.csv", row.names = F)

JLee<-gatherinfo(JLee)
write.csv(JLee,"JLee.csv", row.names = F)

JMartin<-gatherinfo(JMartin)
write.csv(JMartin,"JMartin.csv", row.names = F)

JRobinson<-gatherinfo(JRobinson)
write.csv(JRobinson,"JRobinson.csv", row.names = F)

JSmith<-gatherinfo(JSmith)
write.csv(JSmith,"JSmith.csv", row.names = F)

KTanaka<-gatherinfo(KTanaka)
write.csv(KTanaka,"KTanaka.csv", row.names = F)

MBrown<-gatherinfo(MBrown)
write.csv(MBrown,"MBrown.csv", row.names = F)

MJones<-gatherinfo(MJones)
write.csv(MJones,"MJones.csv", row.names = F)

MMiller<-gatherinfo(MMiller)
write.csv(MMiller,"MMiller.csv", row.names = F)

SLee<-gatherinfo(SLee)
write.csv(SLee,"SLee.csv", row.names = F)

YChen<-gatherinfo(YChen)
write.csv(YChen,"YChen.csv", row.names = F)

##### Generate All_Stacked csv file

setwd("D:/Columbia University/Spring2017-Applied Data Science/Project_4_Bz2290/Spr2017-proj4-team13/lib")

#Read in all relevant csv files generated from previous step
AGupta = read.csv("../data/namecsv/AGupta.csv",header = TRUE, stringsAsFactors = FALSE)
AKumar = read.csv("../data/namecsv/AKumar.csv",header = TRUE, stringsAsFactors = FALSE)
CChen = read.csv("../data/namecsv/CChen.csv",header = TRUE, stringsAsFactors = FALSE)
DJohnson = read.csv("../data/namecsv/DJohnson.csv",header = TRUE, stringsAsFactors = FALSE)
JLee =  read.csv("../data/namecsv/JLee.csv",header = TRUE, stringsAsFactors = FALSE)
JMartin = read.csv("../data/namecsv/JMartin.csv",header = TRUE, stringsAsFactors = FALSE)
JRobinson  = read.csv("../data/namecsv/JRobinson.csv",header = TRUE, stringsAsFactors = FALSE)
JSmith = read.csv("../data/namecsv/JSmith.csv",header = TRUE, stringsAsFactors = FALSE)
KTanaka = read.csv("../data/namecsv/KTanaka.csv",header = TRUE, stringsAsFactors = FALSE)
MBrown = read.csv("../data/namecsv/MBrown.csv",header = TRUE, stringsAsFactors = FALSE)
MJones = read.csv("../data/namecsv/MJones.csv",header = TRUE, stringsAsFactors = FALSE)
MMiller = read.csv("../data/namecsv/MMiller.csv",header = TRUE, stringsAsFactors = FALSE)
SLee = read.csv("../data/namecsv/SLee.csv",header = TRUE, stringsAsFactors = FALSE)
YChen = read.csv("../data/namecsv/YChen.csv",header = TRUE, stringsAsFactors = FALSE)

#Stack the files together 
All = rbind(AGupta,AKumar,CChen,DJohnson,JLee,JMartin,JRobinson,JSmith,KTanaka,MBrown,MJones,MMiller,SLee,YChen)

#Regenerate new unique author id
ID = c(AGupta$AuthorID,AKumar$AuthorID+length(unique(AGupta$AuthorID)))
ID = c(ID,CChen$AuthorID+length(unique(ID)))
ID = c(ID,DJohnson$AuthorID+length(unique(ID)))
ID = c(ID,JLee$AuthorID+length(unique(ID)))
ID = c(ID,JMartin$AuthorID+length(unique(ID)))
ID = c(ID,JRobinson$AuthorID+length(unique(ID)))
ID = c(ID,JSmith$AuthorID+length(unique(ID)))
ID = c(ID,KTanaka$AuthorID+length(unique(ID)))
ID = c(ID,MBrown$AuthorID+length(unique(ID)))
ID = c(ID,MJones$AuthorID+length(unique(ID)))
ID = c(ID,MMiller$AuthorID+length(unique(ID)))
ID = c(ID,SLee$AuthorID+length(unique(ID)))
ID = c(ID,YChen$AuthorID+length(unique(ID)))

#Generate new paper id
paper.id = 1:8453

#Generate the final file
All$AuthorID = ID
All$PaperID = paper.id

#Ouput the files
write.csv(All,file="../data/namecsv/All_Stacked.csv")
