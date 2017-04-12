#setwd("~/Desktop/5243 ADS/proj4/Spr2017-proj4-team13-master/data/nameset")

#### Generate each csv file
gatherinfo<-function(filename){
  name<-deparse(substitute(filename))
  filename<- read.csv(paste0("../data/nameset/",name,".txt"), header = F, sep = ">")
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
write.csv(AGupta,"../data/namecsv/AGupta.csv", row.names = F)

AKumar<-gatherinfo(AKumar)
write.csv(AKumar,"../data/namecsv/AKumar.csv", row.names = F)

CChen<-gatherinfo(CChen)
write.csv(CChen,"../data/namecsv/CChen.csv", row.names = F)

DJohnson<-gatherinfo(DJohnson)
write.csv(DJohnson,"../data/namecsv/DJohnson.csv", row.names = F)

JLee<-gatherinfo(JLee)
write.csv(JLee,"../data/namecsv/JLee.csv", row.names = F)

JMartin<-gatherinfo(JMartin)
write.csv(JMartin,"../data/namecsv/JMartin.csv", row.names = F)

JRobinson<-gatherinfo(JRobinson)
write.csv(JRobinson,"../data/namecsv/JRobinson.csv", row.names = F)

JSmith<-gatherinfo(JSmith)
write.csv(JSmith,"../data/namecsv/JSmith.csv", row.names = F)

KTanaka<-gatherinfo(KTanaka)
write.csv(KTanaka,"../data/namecsv/KTanaka.csv", row.names = F)

MBrown<-gatherinfo(MBrown)
write.csv(MBrown,"../data/namecsv/MBrown.csv", row.names = F)

MJones<-gatherinfo(MJones)
write.csv(MJones,"../data/namecsv/MJones.csv", row.names = F)

MMiller<-gatherinfo(MMiller)
write.csv(MMiller,"../data/namecsv/MMiller.csv", row.names = F)

SLee<-gatherinfo(SLee)
write.csv(SLee,"../data/namecsv/SLee.csv", row.names = F)

YChen<-gatherinfo(YChen)
write.csv(YChen,"../data/namecsv/YChen.csv", row.names = F)

