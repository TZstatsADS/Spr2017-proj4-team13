#Read in text files to idetiy the data itself
######################################################################################
#Agupta.txt
AGupta <- data.frame(scan("../data/nameset/AGupta.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
AGupta$AuthorID <- sub("_.*","",AGupta$Coauthor)
# extract paper number under same author between "_" and first whitespace
AGupta$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", AGupta$Coauthor)
# delete "<" in AGupta$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
AGupta$Coauthor <- gsub("<","",sub("^.*?\\s","", AGupta$Coauthor))
# delete "<" in AGupta$Paper
AGupta$Paper <- gsub("<","",AGupta$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
AGupta$PaperID <- rownames(AGupta)
##############################################################################################
#AKumar.txt
AKumar <- data.frame(scan("../data/nameset/AKumar.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
AKumar$AuthorID <- sub("_.*","",AKumar$Coauthor)
# extract paper number under same author between "_" and first whitespace
AKumar$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", AKumar$Coauthor)
# delete "<" in AKumar$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
AKumar$Coauthor <- gsub("<","",sub("^.*?\\s","", AKumar$Coauthor))
# delete "<" in AKumar$Paper
AKumar$Paper <- gsub("<","",AKumar$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
AKumar$PaperID <- rownames(AKumar)
################################################################################################
#CChen.txt
CChen <- data.frame(scan("../data/nameset/CChen.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
CChen$AuthorID <- sub("_.*","",CChen$Coauthor)
# extract paper number under same author between "_" and first whitespace
CChen$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", CChen$Coauthor)
# delete "<" in CChen$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
CChen$Coauthor <- gsub("<","",sub("^.*?\\s","", CChen$Coauthor))
# delete "<" in CChen$Paper
CChen$Paper <- gsub("<","",CChen$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
CChen$PaperID <- rownames(CChen)
#################################################################################################
#DJohnson.txt
DJohnson <- data.frame(scan("../data/nameset/DJohnson.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
DJohnson$AuthorID <- sub("_.*","",DJohnson$Coauthor)
# extract paper number under same author between "_" and first whitespace
DJohnson$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", DJohnson$Coauthor)
# delete "<" in DJohnson$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
DJohnson$Coauthor <- gsub("<","",sub("^.*?\\s","", DJohnson$Coauthor))
# delete "<" in DJohnson$Paper
DJohnson$Paper <- gsub("<","",DJohnson$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
DJohnson$PaperID <- rownames(DJohnson)
################################################################################################
#JLee.txt
JLee <- data.frame(scan("../data/nameset/JLee.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
JLee$AuthorID <- sub("_.*","",JLee$Coauthor)
# extract paper number under same author between "_" and first whitespace
JLee$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", JLee$Coauthor)
# delete "<" in JLee$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
JLee$Coauthor <- gsub("<","",sub("^.*?\\s","", JLee$Coauthor))
# delete "<" in JLee$Paper
JLee$Paper <- gsub("<","",JLee$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
JLee$PaperID <- rownames(JLee)
####################################################################################################
#JMartin.txt
JMartin <- data.frame(scan("../data/nameset/JMartin.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
JMartin$AuthorID <- sub("_.*","",JMartin$Coauthor)
# extract paper number under same author between "_" and first whitespace
JMartin$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", JMartin$Coauthor)
# delete "<" in JMartin$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
JMartin$Coauthor <- gsub("<","",sub("^.*?\\s","", JMartin$Coauthor))
# delete "<" in JMartin$Paper
JMartin$Paper <- gsub("<","",JMartin$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
JMartin$PaperID <- rownames(JMartin)
#######################################################################################################
#JRobinson.txt
JRobinson <- data.frame(scan("../data/nameset/JRobinson.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
JRobinson$AuthorID <- sub("_.*","",JRobinson$Coauthor)
# extract paper number under same author between "_" and first whitespace
JRobinson$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", JRobinson$Coauthor)
# delete "<" in JRobinson$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
JRobinson$Coauthor <- gsub("<","",sub("^.*?\\s","", JRobinson$Coauthor))
# delete "<" in JRobinson$Paper
JRobinson$Paper <- gsub("<","",JRobinson$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
JRobinson$PaperID <- rownames(JRobinson)
###########################################################################################################
#JSmith.txt
JSmith <- data.frame(scan("../data/nameset/JSmith.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
JSmith$AuthorID <- sub("_.*","",JSmith$Coauthor)
# extract paper number under same author between "_" and first whitespace
JSmith$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", JSmith$Coauthor)
# delete "<" in JSmith$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
JSmith$Coauthor <- gsub("<","",sub("^.*?\\s","", JSmith$Coauthor))
# delete "<" in JSmith$Paper
JSmith$Paper <- gsub("<","",JSmith$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
JSmith$PaperID <- rownames(JSmith)
#######################################################################################################
#KTanaka.txt
KTanaka <- data.frame(scan("../data/nameset/KTanaka.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
KTanaka$AuthorID <- sub("_.*","",KTanaka$Coauthor)
# extract paper number under same author between "_" and first whitespace
KTanaka$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", KTanaka$Coauthor)
# delete "<" in KTanaka$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
KTanaka$Coauthor <- gsub("<","",sub("^.*?\\s","", KTanaka$Coauthor))
# delete "<" in KTanaka$Paper
KTanaka$Paper <- gsub("<","",KTanaka$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
KTanaka$PaperID <- rownames(KTanaka)
##############################################################################################
#MBrown.txt
MBrown <- data.frame(scan("../data/nameset/MBrown.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
MBrown$AuthorID <- sub("_.*","",MBrown$Coauthor)
# extract paper number under same author between "_" and first whitespace
MBrown$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", MBrown$Coauthor)
# delete "<" in MBrown$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
MBrown$Coauthor <- gsub("<","",sub("^.*?\\s","", MBrown$Coauthor))
# delete "<" in MBrown$Paper
MBrown$Paper <- gsub("<","",MBrown$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
MBrown$PaperID <- rownames(MBrown)

#################################################################################################
#MJones.txt
MJones <- data.frame(scan("../data/nameset/MJones.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
MJones$AuthorID <- sub("_.*","",MJones$Coauthor)
# extract paper number under same author between "_" and first whitespace
MJones$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", MJones$Coauthor)
# delete "<" in MJones$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
MJones$Coauthor <- gsub("<","",sub("^.*?\\s","", MJones$Coauthor))
# delete "<" in MJones$Paper
MJones$Paper <- gsub("<","",MJones$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
MJones$PaperID <- rownames(MJones)
####################################################################################################
#MMiller.txt
MMiller <- data.frame(scan("../data/nameset/MMiller.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
MMiller$AuthorID <- sub("_.*","",MMiller$Coauthor)
# extract paper number under same author between "_" and first whitespace
MMiller$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", MMiller$Coauthor)
# delete "<" in MMiller$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
MMiller$Coauthor <- gsub("<","",sub("^.*?\\s","", MMiller$Coauthor))
# delete "<" in MMiller$Paper
MMiller$Paper <- gsub("<","",MMiller$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
MMiller$PaperID <- rownames(MMiller)
####################################################################################################
#SLee.txt
SLee <- data.frame(scan("../data/nameset/SLee.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
SLee$AuthorID <- sub("_.*","",SLee$Coauthor)
# extract paper number under same author between "_" and first whitespace
SLee$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", SLee$Coauthor)
# delete "<" in SLee$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
SLee$Coauthor <- gsub("<","",sub("^.*?\\s","", SLee$Coauthor))
# delete "<" in SLee$Paper
SLee$Paper <- gsub("<","",SLee$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
SLee$PaperID <- rownames(SLee)
##################################################################################################
#YChen.txt
YChen <- data.frame(scan("../data/nameset/YChen.txt",
                          what = list(Coauthor = "", Paper = "", Journal = ""),
                          sep=">", quiet=TRUE),stringsAsFactors=FALSE)
# This need to be modified for different name set
# extract canonical author id befor "_"
YChen$AuthorID <- sub("_.*","",YChen$Coauthor)
# extract paper number under same author between "_" and first whitespace
YChen$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", YChen$Coauthor)
# delete "<" in YChen$Coauthor, you may need to further process the coauthor
# term depending on the method you are using
YChen$Coauthor <- gsub("<","",sub("^.*?\\s","", YChen$Coauthor))
# delete "<" in YChen$Paper
YChen$Paper <- gsub("<","",YChen$Paper)
# add PaperID for furthur use, you may want to combine all the nameset files and
# then assign the unique ID for all the citations
YChen$PaperID <- rownames(YChen)
#################################################################################################
test <- data.frame(scan("../data/nameset/testing.txt",
                         what = list(Coauthor = "", Paper = "", Journal = ""),
                         sep=">", quiet=TRUE),stringsAsFactors=FALSE)
