setwd("~/Desktop/5243 ADS/proj4/Spr2017-proj4-team13-master/data/nameset")
#setwd("D:/Columbia University/Spring2017-Applied Data Science/Project_4_Bz2290/Spr2017-proj4-team13/lib")

gatherinfo<-function(filename){
  name<-deparse(substitute(filename))
  filename<- read.csv(paste0(name,".txt"), header = F, sep = ">")
  ##############Possible Modifications#####################
  #name <- paste0("../data/nameset/",filename,".txt")
  #filename<- read.csv(name,header = F, sep = ">")
  #########################################################
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


#################### Modifications ############################
#Possible modifications....unable to load your Rdata file
#AGupta<-gatherinfo("AGupta")
#AGupta<-as.data.frame(AGupta)
#save(object = AGupta, file="../data/AGupta.Rdata")
#load("../data/AGupta.Rdata")
###############################################################
AGupta<-gatherinfo(AGupta)
write.csv(AGupta,"AGupta.Rdata", row.names = F)

AKumar<-gatherinfo(AKumar)
write.csv(AKumar,"AKumar.Rdata", row.names = F)

CChen<-gatherinfo(CChen)
write.csv(CChen,"CChen.Rdata", row.names = F)

DJohnson<-gatherinfo(DJohnson)
write.csv(DJohnson,"DJohnson.Rdata", row.names = F)

JLee<-gatherinfo(JLee)
write.csv(JLee,"JLee.Rdata", row.names = F)

JMartin<-gatherinfo(JMartin)
write.csv(JMartin,"JMartin.Rdata", row.names = F)

JRobinson<-gatherinfo(JRobinson)
write.csv(JRobinson,"JRobinson.Rdata", row.names = F)

JSmith<-gatherinfo(JSmith)
write.csv(JSmith,"JSmith.Rdata", row.names = F)

KTanaka<-gatherinfo(KTanaka)
write.csv(KTanaka,"KTanaka.Rdata", row.names = F)

MBrown<-gatherinfo(MBrown)
write.csv(MBrown,"MBrown.Rdata", row.names = F)

MJones<-gatherinfo(MJones)
write.csv(MJones,"MJones.Rdata", row.names = F)

MMiller<-gatherinfo(MMiller)
write.csv(MMiller,"MMiller.Rdata", row.names = F)

SLee<-gatherinfo(SLee)
write.csv(SLee,"SLee.Rdata", row.names = F)

YChen<-gatherinfo(YChen)
write.csv(YChen,"YChen.Rdata", row.names = F)
