setwd("~/Desktop/5243 ADS/proj4/Spr2017-proj4-team13-master/data/nameset")


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
